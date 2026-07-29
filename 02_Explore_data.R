# 02_explore_data.R
# Explore downloaded RDS files

# Install once if needed:
# install.packages("data.table")

library(data.table)

# Folder created by 01_download_data.R
out_dir <- "data/raw/zenodo_20479866"

# Find all RDS files, including files inside subfolders
rds_files <- list.files(
  path = out_dir,
  pattern = "\\.rds$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(rds_files) == 0) {
  stop(
    "No RDS files found in: ", out_dir,
    ". Run 01_download_data.R first."
  )
}

message("Number of RDS files found: ", length(rds_files))

# Check contents of every RDS file
for (file in rds_files) {
  object <- readRDS(file)
  
  cat("\n-----------------------------\n")
  cat("File name:", basename(file), "\n")
  cat("Class:", class(object), "\n")
  cat("Type:", typeof(object), "\n")
  
  if (is.data.frame(object)) {
    cat("Rows:", nrow(object), "\n")
    cat("Columns:", ncol(object), "\n")
    cat("Column names:\n")
    print(names(object))
  } else if (is.list(object)) {
    cat("This file contains a list.\nList names:\n")
    print(names(object))
  } else {
    cat("Structure:\n")
    str(object)
  }
}

# Use the first RDS file for summary statistics
hydro_data <- as.data.table(readRDS(rds_files[1]))

# Identify numeric columns
numeric_cols <- names(hydro_data)[sapply(hydro_data, is.numeric)]

if (length(numeric_cols) == 0) {
  stop("The first RDS file has no numeric columns.")
}

# Create summary statistics
stats_table <- rbindlist(lapply(numeric_cols, function(col) {
  x <- hydro_data[[col]]
  
  data.table(
    variable = col,
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q50 = quantile(x, 0.50, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}))

print(stats_table)

# Optional: save statistics
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

write.csv(
  stats_table,
  "outputs/summary_statistics.csv",
  row.names = FALSE
)

message("Summary statistics saved in: outputs/summary_statistics.csv")
