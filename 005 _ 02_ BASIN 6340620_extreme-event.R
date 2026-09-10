# extreme-event
# Basin: 6340620
# PET method: pet_co2
# Analysis period: 1961–2019

rm(list = ls())

project_folder <- paste0(
  "C:/Users/Shwe Yee Win/",
  "OneDrive - CZU v Praze/Hydro_thesis"
)

analysis_folder <- file.path(
  project_folder,
  "results",
  "basin_6340620_extremes"
)

analysis_script <- file.path(
  analysis_folder,
  "calculate_extremes.R"
)

if (!dir.exists(project_folder)) {
  stop(
    "The project folder was not found:\n",
    project_folder
  )
}

if (!dir.exists(analysis_folder)) {
  stop(
    "The basin analysis folder was not found:\n",
    analysis_folder
  )
}

if (!file.exists(analysis_script)) {
  stop(
    "The analysis script was not found:\n",
    analysis_script
  )
}

cat("Project folder:\n")
cat(project_folder, "\n\n")

cat("Analysis folder:\n")
cat(analysis_folder, "\n\n")

cat("Analysis script:\n")
cat(analysis_script, "\n\n")

cat("Script exists:\n")
print(file.exists(analysis_script))

setwd(project_folder)

cat("\nWorking directory:\n")
cat(getwd(), "\n\n")

source(analysis_script)

cat("\nThe calculation script finished.\n\n")

find_result_file <- function(file_name) {
  
  possible_locations <- c(
    file.path(analysis_folder, file_name),
    file.path(analysis_folder, "figures", file_name)
  )
  
  existing_locations <- possible_locations[
    file.exists(possible_locations)
  ]
  
  if (length(existing_locations) == 0) {
    stop(
      "The following result was not found:\n",
      file_name,
      "\n\nLocations checked:\n",
      paste(
        possible_locations,
        collapse = "\n"
      )
    )
  }
  
  return(existing_locations[1])
}

threshold_file <- find_result_file(
  "thresholds_Q50_Q90.csv"
)

thresholds <- read.csv(
  threshold_file,
  stringsAsFactors = FALSE
)

cat("\nQ50 and Q90 thresholds:\n\n")
print(thresholds)

if (interactive()) {
  View(thresholds)
}


annual_file <- find_result_file(
  "annual_components_and_extremes.csv"
)

annual_values <- read.csv(
  annual_file,
  stringsAsFactors = FALSE
)

cat("\nFirst rows of the annual values:\n\n")
print(head(annual_values))

if (interactive()) {
  View(annual_values)
}

contribution_file <- find_result_file(
  "annual_extreme_nonextreme_contributions.csv"
)

contributions <- read.csv(
  contribution_file,
  stringsAsFactors = FALSE
)

cat("\nFirst rows of extreme and non-extreme contributions:\n\n")
print(head(contributions))

if (interactive()) {
  View(contributions)
}

trend_file <- find_result_file(
  "annual_trends_Sen_Mann_Kendall.csv"
)

trends <- read.csv(
  trend_file,
  stringsAsFactors = FALSE
)

cat("\nAll annual trend results:\n\n")

print(
  trends[
    ,
    c(
      "metric",
      "units",
      "sen_slope_per_decade",
      "p_value",
      "significant_p_lt_0_05",
      "direction"
    )
  ],
  row.names = FALSE
)

if (interactive()) {
  View(trends)
}

main_metrics <- c(
  "P_mm",
  "AET_mm",
  "PET_CO2_mm"
)

main_trends <- trends[
  trends$metric %in% main_metrics,
  c(
    "metric",
    "units",
    "sen_slope_per_decade",
    "p_value",
    "significant_p_lt_0_05",
    "direction"
  )
]

cat("\nMain trends for P, AET and PM_CO2 PET:\n\n")

print(
  main_trends,
  row.names = FALSE
)

if (interactive()) {
  View(main_trends)
}

significant_trends <- trends[
  is.finite(trends$p_value) &
    trends$p_value < 0.05,
]

cat("\nStatistically significant trends using p < 0.05:\n\n")

if (nrow(significant_trends) == 0) {
  
  cat("No statistically significant trends were found.\n")
  
} else {
  
  print(
    significant_trends[
      ,
      c(
        "metric",
        "units",
        "sen_slope_per_decade",
        "p_value",
        "direction"
      )
    ],
    row.names = FALSE
  )
}

if (interactive()) {
  View(significant_trends)
}

event_file <- find_result_file(
  "AET_extreme_spells_at_least_3_days.csv"
)

evaporation_events <- read.csv(
  event_file,
  stringsAsFactors = FALSE
)

cat("\nNumber of extreme AET events lasting at least 3 days:\n")
cat(nrow(evaporation_events), "\n\n")

cat("First extreme AET events:\n\n")
print(head(evaporation_events))

if (interactive()) {
  View(evaporation_events)
}

# comparison between components

comparison_summary_name <- paste0(
  "summary_total_vs_wet_day_",
  "Q90_by_component.csv"
)

comparison_summary_file <- find_result_file(
  comparison_summary_name
)

component_comparison <- read.csv(
  comparison_summary_file,
  stringsAsFactors = FALSE
)

cat(
  "\nComparison of annual totals and ",
  "wet-day Q90 totals:\n\n",
  sep = ""
)

print(
  component_comparison[
    ,
    c(
      "component",
      "wet_day_Q90_mm_day",
      "mean_annual_total_mm",
      "mean_wet_day_above_Q90_sum_mm",
      "pooled_contribution_pct",
      "annual_total_sen_change_per_decade",
      "annual_total_p_value",
      "Q90_sum_sen_change_per_decade",
      "Q90_sum_p_value"
    )
  ],
  row.names = FALSE
)

if (interactive()) {
  View(component_comparison)
}

annual_comparison_name <- paste0(
  "annual_total_vs_wet_day_",
  "Q90_by_component.csv"
)

annual_comparison_file <- find_result_file(
  annual_comparison_name
)

annual_component_comparison <- read.csv(
  annual_comparison_file,
  stringsAsFactors = FALSE
)

cat("\nFirst annual component-comparison rows:\n\n")
print(head(annual_component_comparison))

if (interactive()) {
  View(annual_component_comparison)
}

simple_comparison <- component_comparison[
  ,
  c(
    "component",
    "mean_annual_total_mm",
    "mean_wet_day_above_Q90_sum_mm",
    "pooled_contribution_pct",
    "Q90_sum_sen_change_per_decade",
    "Q90_sum_p_value"
  )
]

names(simple_comparison) <- c(
  "Component",
  "Mean_annual_total_mm",
  "Mean_Q90_sum_mm",
  "Q90_contribution_percent",
  "Q90_trend_mm_per_decade",
  "Q90_trend_p_value"
)

simple_comparison$Q90_significant <- ifelse(
  simple_comparison$Q90_trend_p_value < 0.05,
  "Significant",
  "Not significant"
)

# Round numbers for easier reading.
simple_comparison$Mean_annual_total_mm <- round(
  simple_comparison$Mean_annual_total_mm,
  1
)

simple_comparison$Mean_Q90_sum_mm <- round(
  simple_comparison$Mean_Q90_sum_mm,
  1
)

simple_comparison$Q90_contribution_percent <- round(
  simple_comparison$Q90_contribution_percent,
  1
)

simple_comparison$Q90_trend_mm_per_decade <- round(
  simple_comparison$Q90_trend_mm_per_decade,
  2
)

simple_comparison$Q90_trend_p_value <- signif(
  simple_comparison$Q90_trend_p_value,
  4
)

cat("\nSimplified component comparison:\n\n")

print(
  simple_comparison,
  row.names = FALSE
)

if (interactive()) {
  View(simple_comparison)
}

simple_comparison_output <- file.path(
  analysis_folder,
  "simple_component_comparison.csv"
)

write.csv(
  simple_comparison,
  simple_comparison_output,
  row.names = FALSE
)

cat("\nSimplified comparison saved as:\n")
cat(simple_comparison_output, "\n")
