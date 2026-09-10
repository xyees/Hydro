# COMPARE ANNUAL TOTALS WITH EXTREME
# Basin: 6340620
# PET method: pet_co2
# Period: 1961-2019

project_folder <- paste0(
  "C:/Users/Shwe Yee Win/",
  "OneDrive - CZU v Praze/Hydro_thesis"
)

input_file <- file.path(
  project_folder,
  "results/tables",
  "basin_6340620_pet_co2_daily_water_balance.rds"
)

table_folder <- file.path(
  project_folder,
  "results/tables"
)

figure_folder <- file.path(
  project_folder,
  "results/figures"
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

if (!file.exists(input_file)) {
  stop("Input file was not found: ", input_file)
}

data <- as.data.frame(
  readRDS(input_file)
)

data <- data[order(data$date), ]

data$basin <- "6340620"

data$year <- as.integer(
  format(data$date, "%Y")
)

# Wet day: precipitation is at least 1 mm/day.
# Dry day: precipitation is less than 1 mm/day.

data$wet_day <- data$pre >= 1

data$wet_dry_class <- ifelse(
  data$wet_day,
  "wet",
  "dry"
)

cat("Total wet days:", sum(data$wet_day), "\n")
cat("Total dry days:", sum(!data$wet_day), "\n")

components <- c(
  P = "pre",
  AET = "aet",
  PET_CO2 = "pet",
  Qsim = "qsim",
  Qsurf = "q_surf"
)

threshold_results <- list()

for (component_name in names(components)) {
  
  column_name <- components[[component_name]]
  
  # Use this component's values on precipitation wet days.
  
  wet_day_values <- data[
    data$wet_day,
    column_name
  ]
  
  threshold_results[[component_name]] <- data.frame(
    basin = "6340620",
    component = component_name,
    wet_day_Q50_mm_day = unname(
      quantile(
        wet_day_values,
        probabilities = 0.50,
        type = 7
      )
    ),
    wet_day_Q90_mm_day = unname(
      quantile(
        wet_day_values,
        probabilities = 0.90,
        type = 7
      )
    ),
    number_of_wet_days = length(wet_day_values)
  )
}

threshold_table <- do.call(
  rbind,
  threshold_results
)

rownames(threshold_table) <- NULL

print(threshold_table)

write.csv(
  threshold_table,
  file.path(
    table_folder,
    "basin_6340620_wet_day_Q50_Q90.csv"
  ),
  row.names = FALSE
)

for (component_name in names(components)) {
  
  column_name <- components[[component_name]]
  
  Q90 <- threshold_table$wet_day_Q90_mm_day[
    threshold_table$component == component_name
  ]
  
  extreme_column <- paste0(
    component_name,
    "_above_wet_day_Q90"
  )
  
  data[[extreme_column]] <- (
    data$wet_day &
      data[[column_name]] > Q90
  )
}

annual_results <- list()

result_number <- 1

for (component_name in names(components)) {
  
  column_name <- components[[component_name]]
  
  extreme_column <- paste0(
    component_name,
    "_above_wet_day_Q90"
  )
  
  Q90 <- threshold_table$wet_day_Q90_mm_day[
    threshold_table$component == component_name
  ]
  
  for (year_number in 1961:2019) {
    
    one_year <- data[
      data$year == year_number,
    ]
    
    values <- one_year[[column_name]]
    extreme <- one_year[[extreme_column]]
    
    annual_total <- sum(
      values,
      na.rm = FALSE
    )
    
    Q90_sum <- sum(
      values[extreme],
      na.rm = FALSE
    )
    
    non_extreme_sum <- sum(
      values[!extreme],
      na.rm = FALSE
    )
    
    annual_results[[result_number]] <- data.frame(
      basin = "6340620",
      year = year_number,
      component = component_name,
      wet_day_Q90_mm_day = Q90,
      
      annual_total_mm = annual_total,
      
      wet_day_above_Q90_sum_mm = Q90_sum,
      
      non_extreme_sum_mm = non_extreme_sum,
      
      wet_day_above_Q90_days = sum(
        extreme,
        na.rm = TRUE
      ),
      
      Q90_contribution_percent = (
        100 * Q90_sum / annual_total
      )
    )
    
    result_number <- result_number + 1
  }
}

annual_comparison <- do.call(
  rbind,
  annual_results
)

rownames(annual_comparison) <- NULL


annual_comparison$calculation_check <- (
  annual_comparison$annual_total_mm -
    annual_comparison$wet_day_above_Q90_sum_mm -
    annual_comparison$non_extreme_sum_mm
)

if (
  max(
    abs(annual_comparison$calculation_check),
    na.rm = TRUE
  ) > 0.000001
) {
  stop("The annual components do not add to the annual total.")
}

annual_comparison$calculation_check <- NULL

write.csv(
  annual_comparison,
  file.path(
    table_folder,
    "basin_6340620_annual_total_vs_wet_day_Q90.csv"
  ),
  row.names = FALSE
)


# CALCULATE SEN'S SLOPE 

calculate_sen_slope <- function(values, years) {
  
  valid <- is.finite(values) & is.finite(years)
  
  values <- values[valid]
  years <- years[valid]
  
  pairs <- combn(
    seq_along(values),
    2
  )
  
  slopes <- (
    values[pairs[2, ]] -
      values[pairs[1, ]]
  ) / (
    years[pairs[2, ]] -
      years[pairs[1, ]]
  )
  
  median(slopes)
}


# CALCULAE MANN-KENDALL P-VALUE

calculate_mk_p_value <- function(values, years) {
  
  valid <- is.finite(values) & is.finite(years)
  
  result <- cor.test(
    x = years[valid],
    y = values[valid],
    method = "kendall",
    exact = FALSE,
    continuity = TRUE
  )
  
  result$p.value
}


# SUMMARIZE EACH COMPONENT

comparison_summary <- list()

for (component_name in names(components)) {
  
  component_data <- annual_comparison[
    annual_comparison$component == component_name,
  ]
  
  # Trend in the complete annual total.
  
  total_slope <- calculate_sen_slope(
    component_data$annual_total_mm,
    component_data$year
  )
  
  total_p_value <- calculate_mk_p_value(
    component_data$annual_total_mm,
    component_data$year
  )
  
  # Trend in the annual sum above wet-day Q90.
  
  Q90_slope <- calculate_sen_slope(
    component_data$wet_day_above_Q90_sum_mm,
    component_data$year
  )
  
  Q90_p_value <- calculate_mk_p_value(
    component_data$wet_day_above_Q90_sum_mm,
    component_data$year
  )
  
  # Trend in percentage contribution.
  
  contribution_slope <- calculate_sen_slope(
    component_data$Q90_contribution_percent,
    component_data$year
  )
  
  contribution_p_value <- calculate_mk_p_value(
    component_data$Q90_contribution_percent,
    component_data$year
  )
  
  early_period <- component_data[
    component_data$year <= 1990,
  ]
  
  late_period <- component_data[
    component_data$year >= 1991,
  ]
  
  comparison_summary[[component_name]] <- data.frame(
    basin = "6340620",
    component = component_name,
    
    wet_day_Q90_mm_day = unique(
      component_data$wet_day_Q90_mm_day
    ),
    
    mean_annual_total_mm = mean(
      component_data$annual_total_mm
    ),
    
    mean_Q90_sum_mm = mean(
      component_data$wet_day_above_Q90_sum_mm
    ),
    
    mean_non_extreme_sum_mm = mean(
      component_data$non_extreme_sum_mm
    ),
    
    mean_Q90_days_per_year = mean(
      component_data$wet_day_above_Q90_days
    ),
    
    pooled_Q90_contribution_percent = (
      100 *
        sum(component_data$wet_day_above_Q90_sum_mm) /
        sum(component_data$annual_total_mm)
    ),
    
    early_1961_1990_Q90_sum_mm = mean(
      early_period$wet_day_above_Q90_sum_mm
    ),
    
    late_1991_2019_Q90_sum_mm = mean(
      late_period$wet_day_above_Q90_sum_mm
    ),
    
    Q90_period_change_mm = (
      mean(late_period$wet_day_above_Q90_sum_mm) -
        mean(early_period$wet_day_above_Q90_sum_mm)
    ),
    
    annual_total_change_per_decade = (
      total_slope * 10
    ),
    
    annual_total_p_value = total_p_value,
    
    annual_total_significance = ifelse(
      total_p_value < 0.05,
      "Significant",
      "Not significant"
    ),
    
    Q90_sum_change_per_decade = (
      Q90_slope * 10
    ),
    
    Q90_sum_p_value = Q90_p_value,
    
    Q90_sum_significance = ifelse(
      Q90_p_value < 0.05,
      "Significant",
      "Not significant"
    ),
    
    contribution_change_per_decade = (
      contribution_slope * 10
    ),
    
    contribution_p_value = contribution_p_value,
    
    contribution_significance = ifelse(
      contribution_p_value < 0.05,
      "Significant",
      "Not significant"
    )
  )
}

comparison_summary <- do.call(
  rbind,
  comparison_summary
)

rownames(comparison_summary) <- NULL

print(comparison_summary)

write.csv(
  comparison_summary,
  file.path(
    table_folder,
    "basin_6340620_Q90_component_comparison_summary.csv"
  ),
  row.names = FALSE
)

annual_TWS <- list()

for (year_number in 1961:2019) {
  
  one_year <- data[
    data$year == year_number,
  ]
  
  annual_TWS[[as.character(year_number)]] <- data.frame(
    basin = "6340620",
    year = year_number,
    mean_TWS_mm = mean(one_year$tws),
    minimum_TWS_mm = min(one_year$tws),
    maximum_TWS_mm = max(one_year$tws),
    year_end_TWS_change_mm = (
      tail(one_year$tws, 1) -
        head(one_year$tws, 1)
    )
  )
}

annual_TWS <- do.call(
  rbind,
  annual_TWS
)

rownames(annual_TWS) <- NULL

write.csv(
  annual_TWS,
  file.path(
    table_folder,
    "basin_6340620_annual_TWS.csv"
  ),
  row.names = FALSE
)

component_colours <- c(
  P = "#2C7FB8",
  AET = "#D95F0E",
  PET_CO2 = "#756BB1",
  Qsim = "#238B45",
  Qsurf = "#74C476"
)

png(
  file.path(
    figure_folder,
    "basin_6340620_annual_total_vs_Q90.png"
  ),
  width = 2400,
  height = 2500,
  res = 250
)

par(
  mfrow = c(3, 2),
  mar = c(4, 4, 3, 1)
)

for (component_name in names(components)) {
  
  component_data <- annual_comparison[
    annual_comparison$component == component_name,
  ]
  
  plot(
    component_data$year,
    component_data$annual_total_mm,
    type = "l",
    lwd = 2,
    col = "grey40",
    xlab = "Year",
    ylab = "Annual sum (mm)",
    main = component_name
  )
  
  lines(
    component_data$year,
    component_data$wet_day_above_Q90_sum_mm,
    lwd = 2,
    col = component_colours[component_name]
  )
  
  legend(
    "topright",
    legend = c(
      "Annual total",
      "Wet-day values above Q90"
    ),
    col = c(
      "grey40",
      component_colours[component_name]
    ),
    lwd = 2,
    bty = "n",
    cex = 0.8
  )
}

plot.new()

dev.off()

cat("\n")
cat("SIGNIFICANCE RULE\n")
cat("-----------------\n")
cat("p < 0.05: statistically significant trend\n")
cat("p >= 0.05: trend is not statistically significant\n\n")

print(
  comparison_summary[
    ,
    c(
      "component",
      "pooled_Q90_contribution_percent",
      "annual_total_change_per_decade",
      "annual_total_p_value",
      "annual_total_significance",
      "Q90_sum_change_per_decade",
      "Q90_sum_p_value",
      "Q90_sum_significance"
    )
  ],
  row.names = FALSE
)

message("Analysis completed successfully.")
message("Tables saved in: ", table_folder)
message("Figure saved in: ", figure_folder)
