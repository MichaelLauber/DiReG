library(httr)


complete_df <- read.csv( "free_papers_w_pmc.csv")

download_pmc_files <- function(url, dest_folder) {
  # Extract the filename from the URL
  filename <- basename(url)
  
  # Create the full destination path
  dest_path <- file.path(dest_folder, filename)
  
  # Download the file
  response <- httr::GET(url, write_disk(dest_path, overwrite = TRUE))
  
  # Check if the download was successful
  if (response$status_code == 200) {
    message(paste("Downloaded:", filename))
  } else {
    message(paste("Failed:", filename, "Status Code:", response$status_code))
  }
}

# Define the folder where files should be saved
dest_folder <- "all_free_pdfs"  # Change this to your desired folder

# Create the folder if it doesn't exist
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}

download_pmc_files(complete_df$download_query[1], "all_free_pdfs" )

sapply(complete_df$download_query, function(url) download_pmc_files(url, dest_folder))

# Print completion message
message("All downloads completed!")