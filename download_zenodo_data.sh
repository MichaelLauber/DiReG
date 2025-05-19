#!/bin/sh

# Create a directory for the downloads
OUTPUT_DIR="zenodo_data"
mkdir -p "$OUTPUT_DIR"
cd "$OUTPUT_DIR" || { echo "Failed to change to directory $OUTPUT_DIR"; exit 1; }

echo "Downloading Zenodo data to: $OUTPUT_DIR"

# Download the files archive
ZENODO_URL="https://zenodo.org/api/records/15458798/files-archive"
echo "Downloading archive from $ZENODO_URL..."
wget -q --show-progress "$ZENODO_URL" -O files_archive.zip

# Check if download was successful
if [ $? -ne 0 ]; then
    echo "Failed to download the files archive"
    exit 1
fi

# Unzip the archive
echo "Extracting files_archive.zip..."
unzip -q files_archive.zip
rm files_archive.zip

# Find and extract all .tar.gz files
echo "Extracting all .tar.gz files..."
for tarfile in *.tar.gz; do
    if [ -f "$tarfile" ]; then
        echo "Extracting $tarfile..."
        tar -xzf "$tarfile"
        echo "Finished extracting $tarfile"
        
        # Delete the tar.gz file after extraction
        echo "Deleting $tarfile to save space..."
        rm "$tarfile"
    fi
done

echo "All archives have been downloaded and extracted."
echo "Files are located in: $PWD"

# List the extracted directories
echo "Extracted directories:"
ls -la