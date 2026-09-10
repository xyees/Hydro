# Inspect_downloads.R

raw_data_folder <- "data/raw"

if (!dir.exists(raw_data_folder)) {
  stop(
    "The data/raw folder does not exist.\n",
    "Run the download script first."
  )
}

report_folder <- "results/reports"

dir.create(
  report_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

report_file <- file.path(
  report_folder,
  "downloaded_data_summary.txt"
)

all_files <- list.files(
  path = raw_data_folder,
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_files) == 0) {
  stop("No downloaded files were found in data/raw.")
}

# Identify the extensions of all downloaded files.
file_extensions <- tolower(
  tools::file_ext(all_files)
)

file_type_summary <- table(file_extensions)

netcdf_files <- list.files(
  path = raw_data_folder,
  pattern = "\\.nc$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

rds_files <- list.files(
  path = raw_data_folder,
  pattern = "\\.rds$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

sink(report_file, split = TRUE)

cat("DOWNLOADED DATA SUMMARY\n")
cat("=======================\n\n")

cat("Project folder:\n")
cat(getwd(), "\n\n")

cat("Data folder:\n")
cat(file.path(getwd(), raw_data_folder), "\n\n")

cat("Total number of downloaded files:", length(all_files), "\n\n")

cat("FILE TYPES\n")
cat("----------\n")

print(file_type_summary)

cat("\n")

cat("Number of NetCDF files:", length(netcdf_files), "\n")
cat("Number of RDS files:", length(rds_files), "\n")

cat("\n\n")
cat("NETCDF FILES\n")
cat("============\n")

if (length(netcdf_files) == 0) {
  
  cat("\nNo NetCDF files were found.\n")
  
} else {
  
  # Install ncdf4 if it is not already installed.
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    
    sink()
    
    install.packages(
      "ncdf4",
      repos = "https://cloud.r-project.org"
    )
    
    sink(report_file, append = TRUE, split = TRUE)
  }
  
  for (file_number in seq_along(netcdf_files)) {
    
    file_path <- netcdf_files[file_number]
    
    cat("\n")
    cat("--------------------------------------------------\n")
    cat("NetCDF file number:", file_number, "\n")
    cat("File:", file_path, "\n")
    
    file_size_mb <- file.info(file_path)$size / 1024^2
    
    cat(
      "File size:",
      round(file_size_mb, 2),
      "MB\n"
    )
    
    netcdf_data <- ncdf4::nc_open(file_path)
    
    variable_names <- names(netcdf_data$var)
    
    cat(
      "Number of variables:",
      length(variable_names),
      "\n"
    )
    
    cat(
      "Variables:",
      paste(variable_names, collapse = ", "),
      "\n"
    )
    
    for (variable_name in variable_names) {
      
      variable_information <- netcdf_data$var[[variable_name]]
      
      units_information <- ncdf4::ncatt_get(
        netcdf_data,
        variable_name,
        "units"
      )
      
      if (isTRUE(units_information$hasatt)) {
        variable_units <- units_information$value
      } else {
        variable_units <- "Not specified"
      }
      
      dimension_names <- vapply(
        variable_information$dim,
        function(dimension) dimension$name,
        character(1)
      )
      
      dimension_lengths <- vapply(
        variable_information$dim,
        function(dimension) dimension$len,
        numeric(1)
      )
      
      cat("\n")
      cat("Variable:", variable_name, "\n")
      cat("Units:", variable_units, "\n")
      
      cat(
        "Dimensions:",
        paste(dimension_names, collapse = ", "),
        "\n"
      )
      
      cat(
        "Dimension lengths:",
        paste(dimension_lengths, collapse = " x "),
        "\n"
      )
      
      cat(
        "Number of values:",
        prod(dimension_lengths),
        "\n"
      )
    }
    
    # Print information about the time dimension when available.
    dimension_names <- names(netcdf_data$dim)
    
    if ("time" %in% dimension_names) {
      
      time_values <- netcdf_data$dim$time$vals
      time_units <- netcdf_data$dim$time$units
      
      cat("\nTime information:\n")
      cat("Time units:", time_units, "\n")
      cat("First time value:", min(time_values, na.rm = TRUE), "\n")
      cat("Last time value:", max(time_values, na.rm = TRUE), "\n")
      cat("Number of time steps:", length(time_values), "\n")
    }
    
    ncdf4::nc_close(netcdf_data)
  }
}

cat("\n\n")
cat("RDS FILES\n")
cat("=========\n")

if (length(rds_files) == 0) {
  
  cat("\nNo RDS files were found.\n")
  
} else {
  
  for (file_number in seq_along(rds_files)) {
    
    file_path <- rds_files[file_number]
    
    cat("\n")
    cat("--------------------------------------------------\n")
    cat("RDS file number:", file_number, "\n")
    cat("File:", file_path, "\n")
    
    file_size_mb <- file.info(file_path)$size / 1024^2
    
    cat(
      "File size:",
      round(file_size_mb, 2),
      "MB\n"
    )
    
    rds_data <- readRDS(file_path)
    
    cat(
      "Object class:",
      paste(class(rds_data), collapse = ", "),
      "\n"
    )
    
    if (
      is.data.frame(rds_data) ||
      data.table::is.data.table(rds_data)
    ) {
      
      cat("Number of rows:", nrow(rds_data), "\n")
      cat("Number of columns:", ncol(rds_data), "\n")
      
      cat(
        "Column names:",
        paste(names(rds_data), collapse = ", "),
        "\n"
      )
      
      # Print basin information.
      if ("basin" %in% names(rds_data)) {
        
        basin_values <- unique(
          as.character(rds_data$basin)
        )
        
        cat(
          "Number of basins:",
          length(basin_values),
          "\n"
        )
        
        cat(
          "First basin IDs:",
          paste(head(basin_values, 20), collapse = ", "),
          "\n"
        )
      }
      
      # Print variable information.
      if ("variable" %in% names(rds_data)) {
        
        variable_values <- sort(
          unique(as.character(rds_data$variable))
        )
        
        cat(
          "Hydrological variables:",
          paste(variable_values, collapse = ", "),
          "\n"
        )
      }
      
      # Print PET methods.
      if ("pet_method" %in% names(rds_data)) {
        
        pet_methods <- sort(
          unique(as.character(rds_data$pet_method))
        )
        
        cat(
          "PET methods:",
          paste(pet_methods, collapse = ", "),
          "\n"
        )
      }
      
      # Print the date range.
      if ("date" %in% names(rds_data)) {
        
        date_values <- as.Date(rds_data$date)
        
        cat(
          "First date:",
          as.character(min(date_values, na.rm = TRUE)),
          "\n"
        )
        
        cat(
          "Last date:",
          as.character(max(date_values, na.rm = TRUE)),
          "\n"
        )
      }
      
      cat("\nFirst six rows:\n")
      print(head(rds_data))
      
    } else {
      
      cat("\nStructure of the RDS object:\n")
      str(rds_data, max.level = 2)
    }
    
    # Remove the large object before reading the next file.
    rm(rds_data)
    gc()
  }
}

# Report

cat("\n\n")
cat("SUMMARY COMPLETED\n")
cat("=================\n")

cat("\nReport saved to:\n")
cat(file.path(getwd(), report_file), "\n")

sink()

cat("\nInspection completed successfully.\n")
cat("The report was saved here:\n")
cat(file.path(getwd(), report_file), "\n")
