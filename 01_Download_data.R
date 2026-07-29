# 01_download_data.R
# Download data from Zenodo

library(jsonlite)

# Allow enough time for large downloads
options(timeout = 3600)

# Folder where downloaded data will be saved
out_dir <- "data/raw/zenodo_20479866"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Zenodo DOI
doi <- "10.5281/zenodo.20479866"

# Get record ID from DOI
record_id <- sub(".*zenodo\\.", "", doi)

# Create Zenodo API URL
api_url <- paste0("https://zenodo.org/api/records/", record_id)

# Read information from Zenodo
record <- fromJSON(api_url, simplifyVector = FALSE)

# Get files from the record
files <- record$files

# Download each file
for (f in files) {
  file_name <- f$key
  download_url <- f$links$self
  out_path <- file.path(out_dir, file_name)
  
  # Create subfolders if needed
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  
  # Check whether the downloaded file is complete
  expected_size <- as.numeric(f$size)
  
  complete_file <- file.exists(out_path) &&
    !is.na(expected_size) &&
    file.info(out_path)$size == expected_size
  
  # Remove an incomplete download before trying again
  if (file.exists(out_path) && !complete_file) {
    message("Removing incomplete file: ", file_name)
    file.remove(out_path)
  }
  
  # Skip a file that has already downloaded completely
  if (complete_file) {
    message("Already downloaded: ", file_name)
    next
  }
  
  message("Downloading: ", file_name)
  message("This may take several minutes for large files.")
  
  download.file(
    url = download_url,
    destfile = out_path,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
  
  # Confirm that the final file is complete
  if (!file.exists(out_path) ||
      (!is.na(expected_size) && file.info(out_path)$size != expected_size)) {
    stop("Download was incomplete for: ", file_name, ". Please run the script again.")
  }
}

message("Done. Files saved in: ", out_dir)
