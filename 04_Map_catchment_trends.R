# install.packages(c("jsonlite", "sf", "ggplot2",
#                    "rnaturalearth", "rnaturalearthdata"))

library(jsonlite)
library(sf)
library(ggplot2)

record_id <- "21223242"

start_year <- 1980
end_year <- 2020
time_period <- paste0(start_year, "–", end_year)

# Allow sufficient time for large Zenodo downloads
options(timeout = 3600)

# Folders
out_dir <- paste0("data/raw/zenodo_", record_id)
output_dir <- "outputs/catchment_trends"
shapefile_dir <- file.path(output_dir, "catchment_shapefile")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(shapefile_dir, showWarnings = FALSE, recursive = TRUE)

api_url <- paste0("https://zenodo.org/api/records/", record_id)

record <- jsonlite::fromJSON(
  api_url,
  simplifyVector = FALSE
)

for (f in record$files) {
  out_file <- file.path(out_dir, f$key)
  
  dir.create(
    dirname(out_file),
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  expected_size <- as.numeric(f$size)
  
  complete_file <- file.exists(out_file) &&
    !is.na(expected_size) &&
    file.info(out_file)$size == expected_size
  
  # Remove incomplete downloads before downloading again
  if (file.exists(out_file) && !complete_file) {
    message("Removing incomplete file: ", f$key)
    file.remove(out_file)
  }
  
  if (complete_file) {
    message("Already downloaded: ", f$key)
    next
  }
  
  message("Downloading: ", f$key)
  
  download.file(
    url = f$links$self,
    destfile = out_file,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
}

# Load Zenodo RDS files

info_file <- file.path(
  out_dir,
  "catchment_info_table.rds"
)

trend_file <- file.path(
  out_dir,
  "trend_annual_pre_aet_q_q_surf_tws.rds"
)

if (!file.exists(info_file) || !file.exists(trend_file)) {
  stop(
    "Expected RDS files were not found in: ",
    out_dir
  )
}

catchment_info <- readRDS(info_file)
trend_data <- readRDS(trend_file)

catchment_info <- as.data.frame(catchment_info)
trend_data <- as.data.frame(trend_data)

message(
  "Catchment information columns: ",
  paste(names(catchment_info), collapse = ", ")
)

message(
  "Trend table columns: ",
  paste(names(trend_data), collapse = ", ")
)

# Find basin ID and geometry columns

common_columns <- intersect(
  names(catchment_info),
  names(trend_data)
)

basin_candidates <- common_columns[
  grepl(
    "basin|catchment|gauge|station|id",
    common_columns,
    ignore.case = TRUE
  )
]

if (length(basin_candidates) == 0) {
  basin_candidates <- common_columns
}

if (length(basin_candidates) == 0) {
  stop("No shared basin identifier was found.")
}

basin_column <- basin_candidates[1]

message("Using basin ID column: ", basin_column)

geometry_column <- "geometry"

if (!geometry_column %in% names(catchment_info)) {
  stop(
    "No 'geometry' column was found.\n",
    "Available columns are: ",
    paste(names(catchment_info), collapse = ", ")
  )
}

# Select trend variable

variable_column <- names(trend_data)[
  grepl(
    "^variable$|parameter|metric",
    names(trend_data),
    ignore.case = TRUE
  )
][1]

target_variable <- NULL

if (!is.na(variable_column)) {
  available_variables <- unique(trend_data[[variable_column]])
  available_variables <- available_variables[
    !is.na(available_variables)
  ]
  
  message(
    "Available trend variables: ",
    paste(available_variables, collapse = ", ")
  )
  
  if (is.null(target_variable)) {
    target_variable <- available_variables[1]
  }
  
  trend_data <- trend_data[
    trend_data[[variable_column]] == target_variable,
    ,
    drop = FALSE
  ]
}


# Filter for 1980–2020 when a year column is available

year_candidates <- names(trend_data)[
  grepl("^year$|_year$|year_", names(trend_data), ignore.case = TRUE)
]

year_candidates <- year_candidates[
  vapply(
    trend_data[year_candidates],
    is.numeric,
    logical(1)
  )
]

time_filtered <- FALSE
year_column <- NA_character_

if (length(year_candidates) > 0) {
  year_column <- year_candidates[1]
  
  trend_data <- trend_data[
    trend_data[[year_column]] >= start_year &
      trend_data[[year_column]] <= end_year,
    ,
    drop = FALSE
  ]
  
  time_filtered <- TRUE
  
  message(
    "Filtered trend data using ",
    year_column,
    ": ",
    time_period
  )
} else {
  warning(
    "No numeric year column was found. ",
    "The trend data may already be pre-calculated for a fixed period."
  )
}

if (nrow(trend_data) == 0) {
  stop(
    "No trend rows remain for the requested period: ",
    time_period
  )
}

# Find numeric trend-value column

trend_candidates <- names(trend_data)[
  grepl(
    "trend|slope|value",
    names(trend_data),
    ignore.case = TRUE
  )
]

trend_candidates <- trend_candidates[
  vapply(
    trend_data[trend_candidates],
    is.numeric,
    logical(1)
  )
]

if (length(trend_candidates) == 0) {
  numeric_columns <- names(trend_data)[
    vapply(trend_data, is.numeric, logical(1))
  ]
  
  excluded_columns <- c(basin_column, year_column)
  
  trend_candidates <- setdiff(
    numeric_columns,
    excluded_columns
  )
}

if (length(trend_candidates) == 0) {
  stop("No numeric trend-value column was found.")
}

trend_column <- trend_candidates[1]

message("Mapping trend column: ", trend_column)


# Summarise trend values for each catchment

trend_summary <- stats::aggregate(
  trend_data[[trend_column]],
  by = list(trend_data[[basin_column]]),
  FUN = function(x) mean(x, na.rm = TRUE)
)

names(trend_summary) <- c(basin_column, "trend_value")

trend_summary <- trend_summary[
  is.finite(trend_summary$trend_value),
  ,
  drop = FALSE
]

if (nrow(trend_summary) == 0) {
  stop("No valid trend values were found.")
}

# Join trend values to catchment geometry

catchment_basin_id <- as.character(catchment_info[[basin_column]])
trend_basin_id <- as.character(trend_summary[[basin_column]])

match_index <- match(
  catchment_basin_id,
  trend_basin_id
)

keep_rows <- !is.na(match_index)

map_data <- catchment_info[
  keep_rows,
  ,
  drop = FALSE
]

map_data$trend_value <- trend_summary$trend_value[
  match_index[keep_rows]
]

if (nrow(map_data) == 0) {
  stop("No catchment geometries could be joined to trend values.")
}

# Convert geometry to sf format

if (inherits(map_data[[geometry_column]], "sfc")) {
  map_sf <- sf::st_as_sf(
    map_data,
    sf_column_name = geometry_column
  )
} else if (is.character(map_data[[geometry_column]])) {
  map_sf <- sf::st_as_sf(
    map_data,
    wkt = geometry_column,
    crs = 4326
  )
} else {
  stop(
    "The geometry column is not an sf geometry column or WKT text."
  )
}

# Remove empty or invalid catchment geometries
empty_geometry <- sf::st_is_empty(map_sf)

valid_geometry <- sf::st_is_valid(
  map_sf,
  NA_on_exception = TRUE
)

valid_geometry[is.na(valid_geometry)] <- FALSE

map_sf <- map_sf[
  !empty_geometry & valid_geometry,
  ,
  drop = FALSE
]

if (nrow(map_sf) == 0) {
  stop("No valid catchment shapes are available for mapping.")
}

# Use longitude/latitude if the geometry has no CRS information
if (is.na(sf::st_crs(map_sf))) {
  warning(
    "Catchment geometry has no CRS. Assuming WGS84 longitude/latitude."
  )
  
  sf::st_crs(map_sf) <- 4326
}

map_sf <- sf::st_transform(map_sf, 4326)


# Create a visible trend colour scale

trend_values <- map_sf$trend_value
trend_values <- trend_values[is.finite(trend_values)]

if (length(trend_values) == 0) {
  stop("No valid trend values are available for mapping.")
}

# Limit extreme values so small differences remain visible
trend_range <- quantile(
  trend_values,
  probs = c(0.02, 0.98),
  na.rm = TRUE
)

lower_limit <- unname(trend_range[1])
upper_limit <- unname(trend_range[2])

if (lower_limit == upper_limit) {
  adjustment <- abs(lower_limit) * 0.1
  
  if (adjustment == 0) {
    adjustment <- 0.01
  }
  
  lower_limit <- lower_limit - adjustment
  upper_limit <- upper_limit + adjustment
}

# Use red-white-blue if values have both negative and positive trends
if (lower_limit < 0 && upper_limit > 0) {
  max_abs_value <- max(abs(lower_limit), abs(upper_limit))
  
  trend_colour_scale <- scale_fill_gradient2(
    low = "#B2182B",
    mid = "#F7F7F7",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-max_abs_value, max_abs_value),
    oob = scales::squish,
    name = "Trend"
  )
} else if (upper_limit <= 0) {
  trend_colour_scale <- scale_fill_gradientn(
    colours = c("#08306B", "#4292C6", "#F7FBFF"),
    limits = c(lower_limit, upper_limit),
    oob = scales::squish,
    name = "Trend"
  )
} else {
  trend_colour_scale <- scale_fill_gradientn(
    colours = c("#FFFFCC", "#FD8D3C", "#800026"),
    limits = c(lower_limit, upper_limit),
    oob = scales::squish,
    name = "Trend"
  )
}

# Map title

title_variable <- if (is.null(target_variable)) {
  trend_column
} else {
  target_variable
}

map_subtitle <- if (time_filtered) {
  paste("Time period:", time_period)
} else {
  "Time period depends on the downloaded pre-calculated trend table"
}

# Map 1: Catchment boundaries only

p_catchments <- ggplot(
  map_sf,
  aes(fill = trend_value)
) +
  geom_sf(
    colour = "grey20",
    linewidth = 0.05
  ) +
  trend_colour_scale +
  labs(
    title = paste("Annual catchment trends:", title_variable),
    subtitle = map_subtitle,
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

print(p_catchments)

catchment_map_file <- file.path(
  output_dir,
  "catchment_trends_1980_2020.png"
)

ggsave(
  catchment_map_file,
  p_catchments,
  width = 11,
  height = 8,
  dpi = 300
)

message("Catchment map saved to: ", catchment_map_file)


# Map 2: Catchments on a country-boundary basemap

if (requireNamespace("rnaturalearth", quietly = TRUE)) {
  world_map <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )
  
  map_bbox <- sf::st_bbox(map_sf)
  
  x_padding <- (map_bbox["xmax"] - map_bbox["xmin"]) * 0.08
  y_padding <- (map_bbox["ymax"] - map_bbox["ymin"]) * 0.08
  
  if (x_padding == 0) x_padding <- 0.5
  if (y_padding == 0) y_padding <- 0.5
  
  xmin <- unname(map_bbox["xmin"] - x_padding)
  xmax <- unname(map_bbox["xmax"] + x_padding)
  ymin <- unname(map_bbox["ymin"] - y_padding)
  ymax <- unname(map_bbox["ymax"] + y_padding)
  
  p_basemap <- ggplot() +
    geom_sf(
      data = world_map,
      fill = "grey93",
      colour = "grey55",
      linewidth = 0.2
    ) +
    geom_sf(
      data = map_sf,
      aes(fill = trend_value),
      colour = "grey20",
      linewidth = 0.05
    ) +
    trend_colour_scale +
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      expand = FALSE
    ) +
    labs(
      title = paste(
        "Annual catchment trends on geographic map:",
        title_variable
      ),
      subtitle = map_subtitle,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal()
  
  print(p_basemap)
  
  basemap_file <- file.path(
    output_dir,
    "catchment_trends_1980_2020_basemap.png"
  )
  
  ggsave(
    basemap_file,
    p_basemap,
    width = 11,
    height = 8,
    dpi = 300
  )
  
  message("Basemap version saved to: ", basemap_file)
} else {
  message(
    "Basemap skipped. Install rnaturalearth to create it:\n",
    "install.packages(c('rnaturalearth', 'rnaturalearthdata'))"
  )
}


shapefile_file <- file.path(
  shapefile_dir,
  "catchment_trends.shp"
)

sf::st_write(
  map_sf,
  shapefile_file,
  delete_layer = TRUE,
  quiet = TRUE
)

message("Catchment shapefile saved to: ", shapefile_file)
message("Finished.")
