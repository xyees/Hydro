# Convert downloaded daily catchment time series to annual values.

library(data.table)

input_folder <- "data/raw"
output_folder <- "data/processed/annual"
table_folder <- "results/tables"

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(table_folder, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(input_folder)) { 
  stop(
    "Folder data/raw does not exist.\n",
    "Run R/01_download_zenodo.R before this script." 
  )
}

# Names associated with daily fluxes. These values are summed over each year.

flux_pattern <- paste(
  c(
    "^pre$", "precip", "rain", "snowfall",
    "^pet$", "potential.*evapo",
    "^aet$", "actual.*evapo", "evapotrans",
    "^q$", "^q_", "^qsim$", "^qsim_", "runoff", "discharge", "streamflow"
  ),
  collapse = "|"
)

# Names associated with states/storage. These values are averaged by year.
state_pattern <- paste(
  c(
    "^tws$", "storage", "soil.*moist", "groundwater",
    "water.*table", "snow.*water", "^swe$"
  ),
  collapse = "|"
)

# Candidate names used to retain catchment, PET-method, model, or scenario groups.

group_pattern <- paste(
  c(
    "basin", "catchment", "station", "site", "gauge",
    "(^|_)id$", "method", "pet.*method", "method.*pet", "model", "scenario",
    "^variable$", "^component$", "hydrological.*component"
  ),
  collapse = "|"
)

safe_name <- function(path, object_number = NULL) {
  relative_path <- substring(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    nchar(normalizePath(input_folder, winslash = "/", mustWork = FALSE)) + 2
  )
  
  name <- tools::file_path_sans_ext(relative_path)
  name <- gsub("[^A-Za-z0-9_-]+", "_", name)
  
  if (!is.null(object_number)) {
    name <- paste0(name, "_object_", object_number)
  }
  
  name
}

read_tabular_file <- function(path) {
  extension <- tolower(tools::file_ext(path))
  
  tryCatch({
    if (extension == "rds") {
      object <- readRDS(path)
      
      if (is.data.frame(object) || data.table::is.data.table(object)) {
        return(list(as.data.table(object)))
      }
      
      if (is.list(object)) {
        tables <- Filter(
          function(item) is.data.frame(item) || data.table::is.data.table(item),
          object
        )
        return(lapply(tables, as.data.table))
      }
      
      return(list())
    }
    
    if (extension %in% c("csv", "txt", "tsv")) {
      return(list(fread(path, showProgress = FALSE)))
    }
    
    list()
  }, error = function(error) {
    message("Could not read ", path, ": ", error$message)
    list()
  })
}

detect_date <- function(data) {
  column_names <- names(data)
  lower_names <- tolower(column_names)
  
  date_class <- vapply(
    data,
    function(column) inherits(column, c("Date", "POSIXct", "POSIXlt")),
    logical(1)
  )
  
  if (any(date_class)) {
    return(as.Date(data[[which(date_class)[1]]]))
  }
  
  date_candidates <- which(
    lower_names %in% c(
      "date", "datetime", "time", "timestamp", "day",
      "date_time", "observation_date"
    ) |
      grepl("(^|_)date($|_)", lower_names)
  )
  
  for (index in date_candidates) {
    values <- data[[index]]
    
    parsed <- suppressWarnings(as.Date(values))
    
    if (sum(!is.na(parsed)) >= max(1, floor(0.8 * length(parsed)))) {
      return(parsed)
    }
    
    parsed <- suppressWarnings(as.Date(as.character(values), format = "%Y%m%d"))
    
    if (sum(!is.na(parsed)) >= max(1, floor(0.8 * length(parsed)))) {
      return(parsed)
    }
  }
  
  year_column <- match("year", lower_names)
  month_column <- match("month", lower_names)
  day_column <- match("day", lower_names)
  
  if (!anyNA(c(year_column, month_column, day_column))) {
    return(
      as.Date(
        sprintf(
          "%04d-%02d-%02d",
          as.integer(data[[year_column]]),
          as.integer(data[[month_column]]),
          as.integer(data[[day_column]])
        )
      )
    )
  }
  
  NULL
}

aggregate_daily_table <- function(data, source_file) {
  data <- copy(as.data.table(data))
  
  if (nrow(data) == 0) {
    return(list(error = "table has no rows"))
  }
  
  observation_date <- detect_date(data)
  
  if (is.null(observation_date)) {
    return(list(error = "no daily date column could be detected"))
  }
  
  valid_date <- !is.na(observation_date)
  
  if (!any(valid_date)) {
    return(list(error = "all detected dates are missing"))
  }
  
  data <- data[valid_date]
  observation_date <- observation_date[valid_date]
  data[, observation_date__ := observation_date]
  data[, annual_year := as.integer(format(observation_date, "%Y"))]
  
  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_columns <- setdiff(
    numeric_columns,
    c("annual_year", "year", "month", "day")
  )
  numeric_columns <- numeric_columns[
    !grepl("date|datetime|timestamp|^time$", tolower(numeric_columns))
  ]
  
  if (length(numeric_columns) == 0) {
    return(list(error = "no numeric measurement columns were found"))
  }
  
  lower_names <- tolower(names(data))
  grouping_columns <- names(data)[
    grepl(group_pattern, lower_names, ignore.case = TRUE)
  ]
  
  grouping_columns <- unique(c("annual_year", grouping_columns))
  grouping_columns <- intersect(grouping_columns, names(data))
  measurement_columns <- setdiff(numeric_columns, grouping_columns)
  
  if (length(measurement_columns) == 0) {
    return(list(error = "no numeric measurements remained after grouping"))
  }
  
  # basin | variable | date | values | pet_method
  
  variable_columns <- names(data)[
    tolower(names(data)) %in% c(
      "variable", "component", "hydrological_component"
    )
  ]
  
  if (
    length(variable_columns) == 1 &&
    "values" %in% names(data) &&
    is.numeric(data$values)
  ) {
    variable_column <- variable_columns[1]
    
    annual <- data[
      , {
        component_name <- tolower(as.character(get(variable_column)[1]))
        use_sum <- grepl(flux_pattern, component_name, ignore.case = TRUE)
        
        annual_value <- if (all(is.na(values))) {
          NA_real_
        } else if (use_sum) {
          sum(values, na.rm = TRUE)
        } else {
          mean(values, na.rm = TRUE)
        }
        
        list(
          values = annual_value,
          days_available = uniqueN(observation_date__)
        )
      },
      by = grouping_columns
    ]
    
    setorderv(annual, grouping_columns)
    
    component_values <- unique(as.character(data[[variable_column]]))
    rules <- data.table(
      source_file = source_file,
      variable = component_values,
      annual_operation = ifelse(
        grepl(flux_pattern, tolower(component_values), ignore.case = TRUE),
        "sum",
        "mean"
      ),
      recognized_state_variable = grepl(
        state_pattern,
        tolower(component_values),
        ignore.case = TRUE
      )
    )
    
    return(list(
      annual = annual,
      rules = rules,
      first_date = min(observation_date),
      last_date = max(observation_date),
      daily_rows = nrow(data)
    ))
  }
  
  flux_columns <- measurement_columns[
    grepl(flux_pattern, tolower(measurement_columns), ignore.case = TRUE)
  ]
  state_columns <- measurement_columns[
    grepl(state_pattern, tolower(measurement_columns), ignore.case = TRUE)
  ]
  mean_columns <- setdiff(measurement_columns, flux_columns)
  
  annual <- data[
    , c(
      lapply(.SD[, ..flux_columns], function(values) {
        if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
      }),
      lapply(.SD[, ..mean_columns], function(values) {
        if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
      }),
      list(days_available = uniqueN(observation_date__))
    ),
    by = grouping_columns
  ]
  
  setorderv(annual, grouping_columns)
  
  rules <- data.table(
    source_file = source_file,
    variable = measurement_columns,
    annual_operation = ifelse(
      measurement_columns %in% flux_columns,
      "sum",
      "mean"
    ),
    recognized_state_variable = measurement_columns %in% state_columns
  )
  
  list(
    annual = annual,
    rules = rules,
    first_date = min(observation_date),
    last_date = max(observation_date),
    daily_rows = nrow(data)
  )
}

candidate_files <- list.files(
  input_folder,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(rds|csv|txt|tsv)$",
  ignore.case = TRUE
)

excluded_names <- c(
  "catchment_info_table.rds",
  "trend_annual_pre_aet_q_q_surf_tws.rds",
  "chamtent_elevation_area_biom_kg_claases_information.rds"
)
candidate_files <- candidate_files[
  !basename(candidate_files) %in% excluded_names
]

if (length(candidate_files) == 0) {
  stop(
    "No candidate daily RDS, CSV, TXT, or TSV files were found.\n",
    "Confirm that data.zip was extracted inside data/raw."
  )
}

manifest <- list()
all_rules <- list()
manifest_index <- 0L
rule_index <- 0L

for (source_file in candidate_files) {
  message("Checking: ", source_file)
  tables <- read_tabular_file(source_file)
  
  if (length(tables) == 0) {
    manifest_index <- manifest_index + 1L
    manifest[[manifest_index]] <- data.table(
      source_file = source_file,
      object_number = NA_integer_,
      status = "skipped",
      reason = "file did not contain a readable data frame",
      daily_rows = NA_integer_,
      annual_rows = NA_integer_,
      first_date = as.Date(NA),
      last_date = as.Date(NA),
      output_file = NA_character_
    )
    next
  }
  
  for (object_number in seq_along(tables)) {
    result <- aggregate_daily_table(tables[[object_number]], source_file)
    manifest_index <- manifest_index + 1L
    
    if (!is.null(result$error)) {
      manifest[[manifest_index]] <- data.table(
        source_file = source_file,
        object_number = object_number,
        status = "skipped",
        reason = result$error,
        daily_rows = nrow(tables[[object_number]]),
        annual_rows = NA_integer_,
        first_date = as.Date(NA),
        last_date = as.Date(NA),
        output_file = NA_character_
      )
      next
    }
    
    output_name <- paste0(
      safe_name(
        source_file,
        if (length(tables) > 1) object_number else NULL
      ),
      "_annual.csv"
    )
    output_file <- file.path(output_folder, output_name)
    fwrite(result$annual, output_file)
    
    manifest[[manifest_index]] <- data.table(
      source_file = source_file,
      object_number = object_number,
      status = "converted",
      reason = NA_character_,
      daily_rows = result$daily_rows,
      annual_rows = nrow(result$annual),
      first_date = result$first_date,
      last_date = result$last_date,
      output_file = output_file
    )
    
    rule_index <- rule_index + 1L
    all_rules[[rule_index]] <- result$rules
  }
}

manifest <- rbindlist(manifest, fill = TRUE)
fwrite(manifest, file.path(table_folder, "daily_to_annual_manifest.csv"))

if (length(all_rules) > 0) {
  aggregation_rules <- unique(rbindlist(all_rules, fill = TRUE))
  setorder(aggregation_rules, source_file, variable)
  fwrite(
    aggregation_rules,
    file.path(table_folder, "annual_aggregation_rules.csv")
  )
}

converted_count <- sum(manifest$status == "converted")
skipped_count <- sum(manifest$status == "skipped")

message("")
message("Daily-to-annual preparation complete.")
message("Tables converted: ", converted_count)
message("Tables skipped:   ", skipped_count)
message("Annual data:      ", output_folder)
message(
  "Conversion log:   ",
  file.path(table_folder, "daily_to_annual_manifest.csv")
)

if (converted_count == 0) {
  stop(
    "No daily tables were converted. Inspect ",
    file.path(table_folder, "daily_to_annual_manifest.csv"),
    " for the reason."
  )
}
      
