# 03_analyse_co2.R
# Analyse one filtered CO2 basin

# Install once if needed:
# install.packages("ggplot2")

library(ggplot2)

out_dir <- "data/raw/zenodo_20479866"

analysis_dir <- "outputs/co2"
plot_dir <- file.path(analysis_dir, "plots")

dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# Leave as NULL to use the first filtered basin file.
# Or choose a basin ID that matches the saved file name.
target_basin <- NULL

# Find the filtered one-basin file
if (is.null(target_basin)) {
  filtered_files <- list.files(
    out_dir,
    pattern = "^co2_one_basin_.*\\.rds$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(filtered_files) == 0) {
    stop("No filtered basin file found. Run 03_filter_co2_data.R first.")
  }
  
  filtered_file <- filtered_files[1]
} else {
  safe_basin_name <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    as.character(target_basin)
  )
  
  filtered_file <- file.path(
    out_dir,
    paste0("co2_one_basin_", safe_basin_name, ".rds")
  )
}

if (!file.exists(filtered_file)) {
  stop("Filtered basin file was not found: ", filtered_file)
}

one_basin_data <- readRDS(filtered_file)

if (!is.data.frame(one_basin_data)) {
  one_basin_data <- as.data.frame(one_basin_data)
}

message("Using file: ", filtered_file)
message("Rows: ", nrow(one_basin_data))
message("Columns: ", paste(names(one_basin_data), collapse = ", "))

# Long-format columns
variable_column <- "variable"
value_column <- "values"

# Leave as NULL to use the first available variable.
# Example: target_variable <- "pre"
target_variable <- NULL

if (!all(c(variable_column, value_column) %in% names(one_basin_data))) {
  stop(
    "Expected columns 'variable' and 'values' were not found.\n",
    "Available columns are: ",
    paste(names(one_basin_data), collapse = ", ")
  )
}

if (!is.numeric(one_basin_data[[value_column]])) {
  stop("Column '", value_column, "' is not numeric.")
}

available_variables <- unique(one_basin_data[[variable_column]])
available_variables <- available_variables[!is.na(available_variables)]

if (length(available_variables) == 0) {
  stop("No variables were found in column: ", variable_column)
}

message("Available variables: ", paste(available_variables, collapse = ", "))

if (is.null(target_variable)) {
  target_variable <- available_variables[1]
}

if (!target_variable %in% available_variables) {
  stop(
    "Variable '", target_variable, "' was not found.\n",
    "Choose one of: ",
    paste(available_variables, collapse = ", ")
  )
}

# Select the chosen variable
co2_data <- one_basin_data[
  one_basin_data[[variable_column]] == target_variable,
  ,
  drop = FALSE
]

x <- co2_data[[value_column]]

if (length(x) == 0) {
  stop("No values were found for variable: ", target_variable)
}

# Summary statistics
statistics <- data.frame(
  basin_file = basename(filtered_file),
  variable = target_variable,
  n = length(x),
  missing_values = sum(is.na(x)),
  mean = mean(x, na.rm = TRUE),
  median = median(x, na.rm = TRUE),
  min = min(x, na.rm = TRUE),
  max = max(x, na.rm = TRUE),
  range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
  q25 = unname(quantile(x, 0.25, na.rm = TRUE)),
  q50 = unname(quantile(x, 0.50, na.rm = TRUE)),
  q75 = unname(quantile(x, 0.75, na.rm = TRUE))
)

print(statistics)

output_csv <- file.path(
  analysis_dir,
  paste0("statistics_", target_variable, "_one_basin.csv")
)

write.csv(statistics, output_csv, row.names = FALSE)

message("Statistics saved to: ", output_csv)

# Basin label for plot titles
basin_candidates <- names(one_basin_data)[
  grepl(
    "basin|catchment|gauge|station",
    names(one_basin_data),
    ignore.case = TRUE
  )
]

if (length(basin_candidates) > 0) {
  basin_label <- as.character(one_basin_data[[basin_candidates[1]]][1])
} else {
  basin_label <- basename(filtered_file)
}

# 1. Time-series plot
date_column <- "date"

if (date_column %in% names(co2_data)) {
  plot_data <- co2_data
  
  plot_data$plot_date <- if (inherits(plot_data[[date_column]], "Date")) {
    plot_data[[date_column]]
  } else {
    as.Date(as.character(plot_data[[date_column]]))
  }
  
  if (all(is.na(plot_data$plot_date))) {
    message(
      "Time-series plot skipped: '",
      date_column,
      "' could not be read as a date."
    )
  } else {
    p_time <- ggplot(
      plot_data,
      aes(x = plot_date, y = .data[[value_column]])
    ) +
      geom_line(
        colour = "steelblue",
        linewidth = 0.35,
        na.rm = TRUE
      ) +
      labs(
        title = paste(
          target_variable,
          "time series — basin",
          basin_label
        ),
        x = "Date",
        y = target_variable
      ) +
      theme_minimal()
    
    print(p_time)
    
    ggsave(
      file.path(plot_dir, "co2_timeseries.png"),
      p_time,
      width = 10,
      height = 5,
      dpi = 300
    )
  }
} else {
  message("Time-series plot skipped: no 'date' column was found.")
}

# 2. Scatterplot
# Leave NULL to use a second available variable automatically.
scatter_x_variable <- NULL

if (is.null(scatter_x_variable)) {
  other_variables <- setdiff(available_variables, target_variable)
  
  if (length(other_variables) > 0) {
    scatter_x_variable <- other_variables[1]
  }
}

if (!is.null(scatter_x_variable) &&
    scatter_x_variable %in% available_variables) {
  
  predictor_data <- one_basin_data[
    one_basin_data[[variable_column]] == scatter_x_variable,
    ,
    drop = FALSE
  ]
  
  join_columns <- intersect(
    c("date", "pet_method"),
    names(one_basin_data)
  )
  
  if (length(join_columns) > 0) {
    scatter_data <- merge(
      co2_data[, c(join_columns, value_column), drop = FALSE],
      predictor_data[, c(join_columns, value_column), drop = FALSE],
      by = join_columns,
      suffixes = c("_co2", "_x")
    )
    
    names(scatter_data)[
      names(scatter_data) == paste0(value_column, "_co2")
    ] <- "co2_value"
    
    names(scatter_data)[
      names(scatter_data) == paste0(value_column, "_x")
    ] <- "x_value"
    
    if (nrow(scatter_data) > 0) {
      p_scatter <- ggplot(
        scatter_data,
        aes(x = x_value, y = co2_value)
      ) +
        geom_point(
          alpha = 0.35,
          colour = "darkorange",
          na.rm = TRUE
        ) +
        geom_smooth(
          method = "lm",
          se = TRUE,
          colour = "black",
          na.rm = TRUE
        ) +
        labs(
          title = paste(
            scatter_x_variable,
            "versus",
            target_variable,
            "— basin",
            basin_label
          ),
          x = scatter_x_variable,
          y = target_variable
        ) +
        theme_minimal()
      
      print(p_scatter)
      
      ggsave(
        file.path(plot_dir, "co2_scatterplot.png"),
        p_scatter,
        width = 7,
        height = 5,
        dpi = 300
      )
    } else {
      message("Scatterplot skipped: no matching observations were found.")
    }
  } else {
    message("Scatterplot skipped: no shared date column was found.")
  }
} else {
  message("Scatterplot skipped: a second variable was not available.")
}

# 3. Fractional plot
co2_state <- ifelse(
  co2_data[[value_column]] > 0,
  "Positive",
  ifelse(co2_data[[value_column]] < 0, "Negative", "Zero")
)

co2_state[is.na(co2_data[[value_column]])] <- NA

fraction_data <- as.data.frame(
  prop.table(table(co2_state, useNA = "no"))
)

names(fraction_data) <- c("CO2_state", "Fraction")

if (nrow(fraction_data) > 0) {
  p_fraction <- ggplot(
    fraction_data,
    aes(x = "", y = Fraction, fill = CO2_state)
  ) +
    geom_col(width = 0.7) +
    scale_y_continuous(
      labels = function(x) paste0(round(x * 100), "%")
    ) +
    labs(
      title = paste(
        "Fraction of",
        target_variable,
        "states — basin",
        basin_label
      ),
      x = NULL,
      y = "Fraction of days",
      fill = NULL
    ) +
    theme_minimal()
  
  print(p_fraction)
  
  ggsave(
    file.path(plot_dir, "co2_fractional_stacked_bar.png"),
    p_fraction,
    width = 7,
    height = 5,
    dpi = 300
  )
}

# 4. Basin map if coordinates are present
longitude_candidates <- c("longitude", "lon", "long", "x")
latitude_candidates <- c("latitude", "lat", "y")

longitude_column <- longitude_candidates[
  longitude_candidates %in% names(one_basin_data)
][1]

latitude_column <- latitude_candidates[
  latitude_candidates %in% names(one_basin_data)
][1]

if (!is.na(longitude_column) &&
    !is.na(latitude_column) &&
    is.numeric(one_basin_data[[longitude_column]]) &&
    is.numeric(one_basin_data[[latitude_column]])) {
  
  map_data <- co2_data[
    !is.na(co2_data[[longitude_column]]) &
      !is.na(co2_data[[latitude_column]]),
    ,
    drop = FALSE
  ]
  
  if (nrow(map_data) > 0) {
    p_map <- ggplot(
      map_data,
      aes(
        x = .data[[longitude_column]],
        y = .data[[latitude_column]]
      )
    ) +
      geom_point(size = 3, colour = "red") +
      coord_equal() +
      labs(
        title = paste("Location of basin", basin_label),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal()
    
    print(p_map)
    
    ggsave(
      file.path(plot_dir, "co2_basin_map.png"),
      p_map,
      width = 7,
      height = 5,
      dpi = 300
    )
  }
} else {
  message("Map skipped: no numeric longitude/latitude columns were found.")
}

message("Plots saved in: ", plot_dir)
