# Annual, seasonal and daily-quantile trends for basin 6340620
# Focus: precipitation, AET and PM_CO2 PET

project_folder <- paste0(
  "C:/Users/Shwe Yee Win/",
  "OneDrive - CZU v Praze/Hydro_thesis"
)

basin_number <- "6340620"

if (!dir.exists(project_folder)) {
  stop("Project folder was not found:\n", project_folder)
}

setwd(project_folder)

cat("Working folder:\n", getwd(), "\n\n")

required_packages <- c(
  "data.table",
  "ggplot2",
  "quantreg",
  "trend",
  "Kendall"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

library(data.table)
library(ggplot2)
library(quantreg)
library(trend)
library(Kendall)


file_pattern <- paste0(
  "^basin_",
  basin_number,
  "_pet_co2_daily_water_balance[.]rds$"
)

basin_files <- list.files(
  path = project_folder,
  pattern = file_pattern,
  recursive = TRUE,
  full.names = TRUE
)

if (length(basin_files) == 0) {
  stop(
    "The daily water-balance file was not found for basin ",
    basin_number,
    "."
  )
}

# If duplicate copies exist, use the shortest path.
basin_files <- basin_files[order(nchar(basin_files))]
input_file <- basin_files[1]

cat("Input file:\n", input_file, "\n\n")

daily_data <- as.data.table(readRDS(input_file))

required_columns <- c(
  "date",
  "pre",
  "aet",
  "pet",
  "qsim",
  "q_surf",
  "tws",
  "delta_tws",
  "residual"
)

missing_columns <- setdiff(
  required_columns,
  names(daily_data)
)

if (length(missing_columns) > 0) {
  stop(
    "These columns are missing:\n",
    paste(missing_columns, collapse = ", "),
    "\n\nAvailable columns:\n",
    paste(names(daily_data), collapse = ", ")
  )
}

daily_data[, date := as.Date(date)]
setorder(daily_data, date)

cat("First date:", as.character(min(daily_data$date)), "\n")
cat("Last date: ", as.character(max(daily_data$date)), "\n")
cat("Number of daily records:", nrow(daily_data), "\n\n")

analysis_data <- daily_data[, .(
  date,
  P = pre,
  AET = aet,
  PM_CO2_PET = pet
)]

# The trend analysis focuses only on these three variables.
component_names <- c(
  "P",
  "AET",
  "PM_CO2_PET"
)

get_season <- function(month_number) {
  seasons <- c(
    "DJF", "DJF",
    "MAM", "MAM", "MAM",
    "JJA", "JJA", "JJA",
    "SON", "SON", "SON",
    "DJF"
  )
  
  seasons[month_number]
}

get_season_year <- function(date) {
  year_number <- as.integer(format(date, "%Y"))
  is_december <- format(date, "%m") == "12"
  
  year_number + is_december
}

analysis_data[, year := as.integer(format(date, "%Y"))]
analysis_data[, month := as.integer(format(date, "%m"))]
analysis_data[, season := get_season(month)]
analysis_data[, season_year := get_season_year(date)]

year_check <- analysis_data[, .(
  observed_days = .N,
  expected_days = as.integer(
    as.Date(paste0(year + 1, "-01-01")) -
      as.Date(paste0(year, "-01-01"))
  )
), by = year]

complete_years <- year_check[
  observed_days == expected_days,
  year
]

if (length(complete_years) == 0) {
  stop("No complete calendar years were found.")
}

analysis_data <- analysis_data[
  year %in% complete_years
]

cat(
  "Complete years:",
  min(complete_years),
  "to",
  max(complete_years),
  "\n\n"
)

long_data <- melt(
  analysis_data,
  id.vars = c(
    "date",
    "year",
    "month",
    "season",
    "season_year"
  ),
  measure.vars = component_names,
  variable.name = "component",
  value.name = "daily_value"
)

long_data[, component := as.character(component)]


# Annual totals

# P, AET and PET are daily fluxes in mm/day.
# Summing daily values gives annual totals in mm/year.

annual_data <- long_data[
  is.finite(daily_value),
  .(
    annual_value_mm = sum(daily_value)
  ),
  by = .(
    year,
    component
  )
]

setorder(annual_data, component, year)


# easonal totals

# December belongs to the following DJF season year.
# Remove incomplete seasons at the beginning and end of the series.

season_month_check <- unique(
  long_data[, .(
    season_year,
    season,
    month
  )]
)

complete_seasons <- season_month_check[
  ,
  .(number_of_months = uniqueN(month)),
  by = .(
    season_year,
    season
  )
][
  number_of_months == 3
]

seasonal_long_data <- merge(
  long_data,
  complete_seasons[, .(
    season_year,
    season
  )],
  by = c(
    "season_year",
    "season"
  )
)

seasonal_data <- seasonal_long_data[
  is.finite(daily_value),
  .(
    seasonal_value_mm = sum(daily_value)
  ),
  by = .(
    year = season_year,
    season,
    component
  )
]

setorder(
  seasonal_data,
  component,
  season,
  year
)

# Sen slope and Mann-Kendall function

calculate_sen_mk <- function(values, years) {
  
  valid <- is.finite(values) & is.finite(years)
  
  values <- values[valid]
  years <- years[valid]
  
  if (length(values) < 10) {
    return(data.table(
      number_of_years = length(values),
      sen_slope_per_year = NA_real_,
      sen_slope_per_decade = NA_real_,
      mann_kendall_p_value = NA_real_,
      significant = NA,
      direction = NA_character_
    ))
  }
  
  order_number <- order(years)
  values <- values[order_number]
  years <- years[order_number]
  
  sen_result <- trend::sens.slope(values)
  mk_result <- Kendall::MannKendall(values)
  
  slope_year <- as.numeric(sen_result$estimates)
  p_value <- as.numeric(mk_result$sl)
  
  direction_text <- if (
    slope_year > 0
  ) {
    "increasing"
  } else if (
    slope_year < 0
  ) {
    "decreasing"
  } else {
    "no change"
  }
  
  data.table(
    number_of_years = length(values),
    sen_slope_per_year = slope_year,
    sen_slope_per_decade = slope_year * 10,
    mann_kendall_p_value = p_value,
    significant = p_value < 0.05,
    direction = direction_text
  )
}

# Annual trends

annual_trends <- annual_data[
  ,
  calculate_sen_mk(
    values = annual_value_mm,
    years = year
  ),
  by = component
]

annual_trends[, `:=`(
  period = "annual",
  season = "all",
  units = "mm/year change per decade"
)]


# Seasonal trends

seasonal_trends <- seasonal_data[
  ,
  calculate_sen_mk(
    values = seasonal_value_mm,
    years = year
  ),
  by = .(
    component,
    season
  )
]

seasonal_trends[, `:=`(
  period = "seasonal",
  units = "mm/season change per decade"
)]


# Combine annual and seasonal trends

trend_table <- rbindlist(
  list(
    annual_trends,
    seasonal_trends
  ),
  fill = TRUE
)

trend_table[
  ,
  significance_result := fifelse(
    significant,
    "significant: p < 0.05",
    "not significant: p >= 0.05"
  )
]

setcolorder(
  trend_table,
  c(
    "period",
    "season",
    "component",
    "number_of_years",
    "units",
    "sen_slope_per_year",
    "sen_slope_per_decade",
    "mann_kendall_p_value",
    "significant",
    "direction",
    "significance_result"
  )
)

setorder(
  trend_table,
  period,
  component,
  season
)

quantile_results <- list()
result_number <- 1

for (component_name in component_names) {
  
  component_data <- long_data[
    component == component_name &
      is.finite(daily_value) &
      is.finite(year)
  ]
  
  for (quantile_level in c(0.10, 0.50, 0.90)) {
    
    quantile_model <- quantreg::rq(
      daily_value ~ year,
      data = component_data,
      tau = quantile_level
    )
    
    slope_year <- unname(
      coef(quantile_model)["year"]
    )
    
    quantile_results[[result_number]] <- data.table(
      component = component_name,
      quantile = quantile_level,
      slope_per_year = slope_year,
      slope_per_decade = slope_year * 10,
      units = "mm/day change per decade"
    )
    
    result_number <- result_number + 1
  }
}

quantile_table <- rbindlist(quantile_results)

table_folder <- file.path(
  project_folder,
  "results",
  "tables"
)

figure_folder <- file.path(
  project_folder,
  "results",
  "figures"
)

dir.create(
  table_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# generate ables

fwrite(
  annual_data,
  file.path(
    table_folder,
    paste0(
      "basin_",
      basin_number,
      "_annual_P_AET_PM_CO2.csv"
    )
  )
)

fwrite(
  seasonal_data,
  file.path(
    table_folder,
    paste0(
      "basin_",
      basin_number,
      "_seasonal_P_AET_PM_CO2.csv"
    )
  )
)

fwrite(
  trend_table,
  file.path(
    table_folder,
    paste0(
      "basin_",
      basin_number,
      "_Sen_Mann_Kendall_trends.csv"
    )
  )
)

fwrite(
  quantile_table,
  file.path(
    table_folder,
    paste0(
      "basin_",
      basin_number,
      "_daily_quantile_trends.csv"
    )
  )
)

# Plot annual values

annual_plot <- ggplot(
  annual_data,
  aes(
    x = year,
    y = annual_value_mm
  )
) +
  geom_line(
    colour = "#287C98",
    linewidth = 0.7
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = "#D97941",
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ component,
    scales = "free_y",
    ncol = 1
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste(
      "Annual trends for basin",
      basin_number
    ),
    subtitle = "Precipitation, AET and PM_CO2 PET",
    x = "Year",
    y = "Annual total (mm/year)"
  )

ggsave(
  filename = file.path(
    figure_folder,
    paste0(
      "basin_",
      basin_number,
      "_annual_P_AET_PM_CO2_trends.png"
    )
  ),
  plot = annual_plot,
  width = 9,
  height = 8,
  dpi = 300
)

# Print annual results

cat("\nAnnual trend results:\n\n")

print(
  trend_table[
    period == "annual",
    .(
      component,
      slope_mm_per_decade = round(
        sen_slope_per_decade,
        2
      ),
      p_value = signif(
        mann_kendall_p_value,
        4
      ),
      direction,
      significance_result
    )
  ]
)

cat("\nAnalysis completed successfully.\n")
cat("Tables were saved in:\n", table_folder, "\n")
cat("Figures were saved in:\n", figure_folder, "\n")
