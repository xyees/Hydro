# ANNUAL WATER BALANCE
# Basin: 6340620
# PET method: pet_co2
# Analysis period: 1961–2019

project_folder <- paste0(
  "C:/Users/Shwe Yee Win/",
  "OneDrive - CZU v Praze/Hydro_thesis"
)

basin_number <- "6340620"

data_file <- file.path(
  project_folder,
  "data/raw/zenodo_20479866/data",
  "hydrological_component_pre_aet_q_tws_1960_2019_daily_pet_co2.rds"
)

all_data <- as.data.frame(readRDS(data_file))

basin_data <- all_data[
  as.character(all_data$basin) == basin_number &
    all_data$pet_method == "pet_co2",
]

rm(all_data)

if (nrow(basin_data) == 0) {
  stop("No data found for this basin.")
}


if (anyDuplicated(basin_data[c("date", "variable")]) > 0) {
  stop("Duplicate dates were found for a component.")
}


daily <- data.frame(
  date = seq(
    as.Date("1960-01-01"),
    as.Date("2019-12-31"),
    by = "day"
  )
)

# pre    = precipitation
# aet    = actual evapotranspiration
# pet    = potential evapotranspiration using pet_co2
# qsim   = simulated total runoff
# q_surf = surface runoff
# tws    = total water storage

components <- c(
  "pre",
  "aet",
  "pet",
  "qsim",
  "q_surf",
  "tws"
)

if (!all(components %in% basin_data$variable)) {
  stop("A required component is missing.")
}

for (component in components) {
  
  component_data <- basin_data[
    basin_data$variable == component,
  ]
    
  daily[[component]] <- component_data$values[
    match(daily$date, component_data$date)
  ]
}

daily$delta_tws <- c(
  NA_real_,
  diff(daily$tws)
)

daily$residual <- (
  daily$pre -
    daily$aet -
    daily$qsim -
    daily$delta_tws
)

daily$year <- as.integer(
  format(daily$date, "%Y")
)

daily <- daily[daily$year >= 1961, ]

annual_total <- function(values) {
  
  if (any(!is.finite(values))) {
    return(NA_real_)
  }
  
  sum(values)
}

annual_mean <- function(values) {
  
  if (any(!is.finite(values))) {
    return(NA_real_)
  }
  
  mean(values)
}


# CALCULATE ANNUAL COMPONENTS

annual_results <- list()

for (year_number in 1961:2019) {
  
  one_year <- daily[
    daily$year == year_number,
  ]
  
  annual_results[[as.character(year_number)]] <- data.frame(
    
    basin = basin_number,
    
    year = year_number,
    
    pet_method = "pet_co2",
    
    precipitation_mm = annual_total(one_year$pre),
    
    aet_mm = annual_total(one_year$aet),
    
    pet_co2_mm = annual_total(one_year$pet),
    
    total_runoff_mm = annual_total(one_year$qsim),
    
    surface_runoff_mm = annual_total(one_year$q_surf),
        
    tws_mean_mm = annual_mean(one_year$tws),
        
    delta_tws_mm = annual_total(one_year$delta_tws),
    
    water_balance_residual_mm = annual_total(
      one_year$residual
    ),
    
    valid_balance_days = sum(
      is.finite(one_year$residual)
    )
  )
}

annual_balance <- do.call(
  rbind,
  annual_results
)

rownames(annual_balance) <- NULL

annual_balance$p_minus_pet_co2_mm <- (
  annual_balance$precipitation_mm -
    annual_balance$pet_co2_mm
)

output_folder <- file.path(
  project_folder,
  "results/tables"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  annual_balance,
  file.path(
    output_folder,
    "basin_6340620_pet_co2_annual_water_balance.csv"
  ),
  row.names = FALSE
)

saveRDS(
  daily,
  file.path(
    output_folder,
    "basin_6340620_pet_co2_daily_water_balance.rds"
  )
)

print(annual_balance)

message("Results saved in: ", output_folder)
