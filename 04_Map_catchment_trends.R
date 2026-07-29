# 04_map_catchment_trends.R
# Download catchment trends from Zenodo and create a map

# Install once if needed:
# install.packages(c("jsonlite", "sf", "ggplot2"))

library(jsonlite)
library(sf)
library(ggplot2)

# Allow enough time for large downloads
options(timeout = 3600)

# Zenodo record
record_id <- "21223242"

# Downloaded data folder
out_dir <- paste0("data/raw/zenodo_", record_id)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Output folder for maps
output_dir <- "outputs/maps"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Download the Zenodo record information
api_url <- paste0("https://zenodo.org/api/records/", record_id)

record <- jsonlite::fromJSON(
  api_url,
  simplifyVector = FALSE
)

# Download each file in the Zenodo record
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

# Load the two RDS files
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

# Find the shared basin identifier column
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

# Find the geometry column
geometry_column <- "geometry"

if (!geometry_column %in% names(catchment_info)) {
  stop(
    "No 'geometry' column was found in catchment_info.\n",
    "Available columns are: ",
    paste(names(catchment_info), collapse = ", ")
  )
}

# Select one trend variable
variable_column <- names(trend_data)[
  grepl(
    "^variable$|parameter|metric",
    names(trend_data),
    ignore.case = TRUE
  )
][1]

# Leave NULL to use the first available trend variable
target_variable <- NULL

if (!is.na(variable_column)) {
  available_variables <- unique(trend_data[[variable_column]])
  available_variables <- available_variables[!is.na(available_variables)]
  
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

# Find the numeric trend column
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
  
  trend_candidates <- setdiff(
    numeric_columns,
    basin_column
  )
}

if (length(trend_candidates) == 0) {
  stop("No numeric trend value column was found.")
}

trend_column <- trend_candidates[1]

message("Mapping trend column: ", trend_column)

# Average repeated trend values for each basin
trend_summary <- stats::aggregate(
  trend_data[[trend_column]],
  by = list(trend_data[[basin_column]]),
  FUN = function(x) mean(x, na.rm = TRUE)
)

names(trend_summary) <- c(basin_column, "trend_value")

# Join geometry and trend data
map_data <- merge(
  catchment_info[, c(basin_column, geometry_column), drop = FALSE],
  trend_summary,
  by = basin_column
)

map_data <- map_data[
  is.finite(map_data$trend_value),
  ,
  drop = FALSE
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
    "The geometry column is not an sf geometry or WKT text column."
  )
}

# Create map
title_variable <- if (is.null(target_variable)) {
  trend_column
} else {
  target_variable
}

# Create a clearer colour scale for trend differences

trend_values <- map_sf$trend_value
trend_values <- trend_values[is.finite(trend_values)]

if (length(trend_values) == 0) {
  stop("No valid trend values are available for the colour scale.")
}

# Use the 2nd and 98th percentiles so extreme values do not hide differences
trend_range <- quantile(
  trend_values,
  probs = c(0.02, 0.98),
  na.rm = TRUE
)

lower_limit <- unname(trend_range[1])
upper_limit <- unname(trend_range[2])

# Prevent an invalid scale when all trend values are very similar
if (lower_limit == upper_limit) {
  adjustment <- abs(lower_limit) * 0.1
  
  if (adjustment == 0) {
    adjustment <- 0.01
  }
  
  lower_limit <- lower_limit - adjustment
  upper_limit <- upper_limit + adjustment
}

# Use a diverging scale if trends include both negative and positive values
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
} else {
  # Use a stronger sequential scale if all trends have the same direction
  trend_colour_scale <- scale_fill_gradientn(
    colours = c("#FFFFCC", "#FD8D3C", "#800026"),
    limits = c(lower_limit, upper_limit),
    oob = scales::squish,
    name = "Trend"
  )
}

p_map <- ggplot(
  map_sf,
  aes(fill = trend_value)
) +
  geom_sf(
    colour = "grey25",
    linewidth = 0.05
  ) +
  trend_colour_scale +
  labs(
    title = paste("Annual catchment trend:", title_variable),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

print(p_map)

# Save map
output_file <- file.path(
  output_dir,
  "catchment_trends.png"
)

ggsave(
  output_file,
  p_map,
  width = 10,
  height = 7,
  dpi = 300
)

message("Map saved to: ", output_file)




# Map the actual catchment boundary shapes

valid_geometry <- sf::st_is_valid(map_sf, NA_on_exception = TRUE)
valid_geometry[is.na(valid_geometry)] <- FALSE

map_sf <- map_sf[
  !sf::st_is_empty(map_sf) & valid_geometry,
  ,
  drop = FALSE
]

if (nrow(map_sf) == 0) {
  stop("No valid catchment shapes are available for mapping.")
}

title_variable <- if (is.null(target_variable)) {
  trend_column
} else {
  target_variable
}

p_map <- ggplot(
  map_sf,
  aes(fill = trend_value)
) +
  geom_sf(
    colour = "grey25",
    linewidth = 0.05
  ) +
  trend_colour_scale +
  labs(
    title = paste("Annual catchment trend:", title_variable),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

print(p_map)

output_file <- file.path(
  output_dir,
  "catchment_trends.png"
)

ggsave(
  output_file,
  p_map,
  width = 11,
  height = 8,
  dpi = 300
)

message("Catchment map saved to: ", output_file)



