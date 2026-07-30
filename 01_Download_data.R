# 01_download_data.R
# Download data from Zenodo

library(jsonlite)

options(timeout = 360)

out_dir <- "data/raw/zenodo_20479866"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Zenodo DOI
doi <- "10.5281/zenodo.20479866"

record_id <- sub(".*zenodo\\.", "", doi)
api_url <- paste0("https://zenodo.org/api/records/", record_id)
record <- fromJSON(api_url, simplifyVector = FALSE)
files <- record$files

for (f in files) {
  file_name <- f$key
  download_url <- f$links$self
  out_path <- file.path(out_dir, file_name)
  
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  
  expected_size <- as.numeric(f$size)
  
  complete_file <- file.exists(out_path) &&
    !is.na(expected_size) &&
    file.info(out_path)$size == expected_size
  
  if (file.exists(out_path) && !complete_file) {
    message("Removing incomplete file: ", file_name)
    file.remove(out_path)
  }
  
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
  
  if (!file.exists(out_path) ||
      (!is.na(expected_size) && file.info(out_path)$size != expected_size)) {
    stop("Download was incomplete for: ", file_name, ". Please run the script again.")
  }
}

message("Done. Files saved in: ", out_dir)
