#!/bin/sh

# Set variables
GITHUB_REPO="https://github.com/MichaelLauber/DiReG.git"  
PROJECT_DIR="DiReG_App"
LOG_FILE="direg_setup.log"

# User configuration for authentication
DB_PASSPHRASE="alphaCentaury"
REGULAR_USERNAME="dummyUser"
REGULAR_PASSWORD="StrongPW!"
REGULAR_EMAIL="user@example.com"
ADMIN_USERNAME="dummyAdmin"
ADMIN_PASSWORD="StrongPWAdmin"
ADMIN_EMAIL="admin@example.com"
OPENAI_FALLBACK_KEY="sk-YOUR_FALLBACK_KEY_HERE"

# Default: run all steps
RUN_CLONE=true
RUN_REFGENOME=true
RUN_ZENODO=true
RUN_MOVEFILES=true
RUN_AUTH=true
RUN_DOCKER=true
RUN_CONFIG=true
RUN_START=true

# Process command line arguments
while [ "$#" -gt 0 ]; do
case "$1" in
--skip-clone) RUN_CLONE=false ;;
--skip-refgenome) RUN_REFGENOME=false ;;
--skip-zenodo) RUN_ZENODO=false ;;
--skip-movefiles) RUN_MOVEFILES=false ;;
--skip-auth) RUN_AUTH=false ;;
--skip-docker) RUN_DOCKER=false ;;
--skip-config) RUN_CONFIG=false ;;
--skip-start) RUN_START=false ;;
--only-movefiles) 
RUN_CLONE=false
RUN_REFGENOME=false
RUN_ZENODO=false
RUN_AUTH=false
RUN_DOCKER=false
RUN_CONFIG=false
RUN_START=false
RUN_MOVEFILES=true
;;
--help)
echo "Usage: $0 [options]"
echo "Options:"
echo "  --skip-clone       Skip repository cloning"
echo "  --skip-refgenome   Skip reference genome download"
echo "  --skip-zenodo      Skip Zenodo data download"
echo "  --skip-movefiles   Skip moving Zenodo files"
echo "  --skip-auth        Skip authentication setup"
echo "  --skip-docker      Skip Docker image building"
echo "  --skip-config      Skip configuration update"
echo "  --skip-start       Skip starting the application"
echo "  --only-movefiles   Only run the file movement step (useful for fixing Zenodo placement)"
echo "  --help             Show this help message"
exit 0
;;
*)
echo "Unknown option: $1"
echo "Use --help for available options"
exit 1
;;
esac
shift
done


# Initialize log file
echo "DiReG Setup Log - $(date)" > $LOG_FILE

log_message() {
  echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" | tee -a $LOG_FILE
}

log_error() {
  echo "$(date +"%Y-%m-%d %H:%M:%S") - ERROR: $1" | tee -a $LOG_FILE
}

handle_error() {
  log_error "$1"
  echo "Check $LOG_FILE for detailed error information."
}

# Main setup process
log_message "Setting up the direg project..."




# Step 1: Clone the repository
if $RUN_CLONE; then
log_message "Cloning the repository from GitHub..."
git clone $GITHUB_REPO $PROJECT_DIR >> $LOG_FILE 2>&1 || { handle_error "Failed to clone repository"; exit 1; }
cd $PROJECT_DIR || { handle_error "Failed to change to project directory"; exit 1; }
else
  log_message "Skipping repository clone (--skip-clone specified)"
if [ "$(basename $(pwd))" != "$PROJECT_DIR"  ]; then
log_message "Current working directory is not the project directory: $PROJECT_DIR"
handle_error "Working directory is not the project directory"; exit 1;
fi
fi

# Get absolute path of the current directory for volume mounting
ABSOLUTE_PATH=$(pwd)
log_message "Absolute path of project: $ABSOLUTE_PATH"

# Step 2: Download reference genomes and place them in app/refGenome
if $RUN_REFGENOME; then
log_message "Downloading reference genomes..."
mkdir -p app/refGenome || { handle_error "Failed to create app/refGenome directory"; exit 1; }
chmod +x download_refGenomes.sh || { handle_error "Failed to make download_refGenomes.sh executable"; exit 1; }
./download_refGenomes.sh app/refGenome >> $LOG_FILE 2>&1 || { handle_error "Reference genome download failed"; exit 1; }
else
  log_message "Skipping reference genome download (--skip-refgenome specified)"
fi

# Step 3: Download Zenodo data
if $RUN_ZENODO; then
log_message "Downloading Zenodo data..."
chmod +x download_zenodo_data.sh || { handle_error "Failed to make download_zenodo_data.sh executable"; exit 1; }
./download_zenodo_data.sh >> $LOG_FILE 2>&1 || { handle_error "Zenodo data download failed"; exit 1; }
else
  log_message "Skipping Zenodo data download (--skip-zenodo specified)"
fi

# Debug: Check zenodo_data structure and contents
if $RUN_MOVEFILES; then
log_message "Checking zenodo_data structure..."
if [ -d "zenodo_data" ]; then
log_message "zenodo_data directory exists"
ls -la zenodo_data/ >> $LOG_FILE 2>&1

# If there's a nested directory, try to fix
if [ $(ls -1 zenodo_data/ | wc -l) -eq 1 ] && [ -d "zenodo_data/$(ls -1 zenodo_data/)" ]; then
NESTED_DIR=$(ls -1 zenodo_data/)
log_message "Found single nested directory: $NESTED_DIR, moving files up one level"
mv zenodo_data/$NESTED_DIR/* zenodo_data/ >> $LOG_FILE 2>&1 || log_error "Failed to move nested files"
rmdir zenodo_data/$NESTED_DIR >> $LOG_FILE 2>&1 || log_error "Failed to remove nested directory"
fi
else
  log_error "zenodo_data directory does not exist after download"
log_message "Searching for alternative zenodo data location..."
find . -name "*zenodo*" -type d >> $LOG_FILE 2>&1
if [ "$RUN_ZENODO" = "false" ]; then
log_message "Note: You skipped the Zenodo download step but are trying to move files. Make sure data exists."
fi
fi

# Step 4: Move the extracted Zenodo files to their correct locations
log_message "Moving files to correct locations..."

# Create necessary directories first
mkdir -p services/paperqa/ || { handle_error "Failed to create services/paperqa directory"; exit 1; }
mkdir -p services/RAG/ || { handle_error "Failed to create services/RAG directory"; exit 1; }
mkdir -p app/data/ || { handle_error "Failed to create app/data directory"; exit 1; }

# Move each source and verify success
move_and_verify() {
  SOURCE=$1
  DEST=$2
  DESC=$3
  
  log_message "Moving $DESC from $SOURCE to $DEST"
  
  # Check if source exists
  if [ ! -e "$SOURCE" ]; then
  log_error "$DESC source does not exist: $SOURCE"
  log_message "Contents of parent directory:"
  ls -la $(dirname "$SOURCE") >> $LOG_FILE 2>&1
  return 1
  fi
  
  # Check if destination directory exists
  if [ ! -d "$(dirname "$DEST")" ]; then
  log_error "Destination directory for $DESC does not exist: $(dirname "$DEST")"
  mkdir -p "$(dirname "$DEST")" >> $LOG_FILE 2>&1 || { log_error "Failed to create destination directory"; return 1; }
  fi
  
  # Perform the move
  mv "$SOURCE" "$DEST" >> $LOG_FILE 2>&1
  
  # Verify move was successful
  if [ $? -eq 0 ]; then
  log_message "Successfully moved $DESC to $DEST"
  return 0
  else
    log_error "Failed to move $DESC to $DEST"
  return 1
  fi
}

# Move each file/directory and log results
move_and_verify "zenodo_data/all_free_pdfs_index" "services/paperqa/" "all_free_pdfs_index"
move_and_verify "zenodo_data/chromadb_w_openaiembedding_semantic_chuncking" "services/RAG/" "chromadb_w_openaiembedding_semantic_chuncking"
move_and_verify "zenodo_data/free_reprogramming_pdfs" "services/paperqa/" "free_reprogramming_pdfs"

# Check if data directory exists in zenodo_data
if [ -d "zenodo_data/data" ]; then
log_message "Found data directory in zenodo_data"
# Note: We can't use wildcards directly with move_and_verify as it expects a single file
log_message "Moving data files from zenodo_data/data to app/data/"
cp -R zenodo_data/data/* app/data/ >> $LOG_FILE 2>&1 || log_error "Failed to copy data files"

# Verify the files were copied
if [ $(ls -A app/data/ | wc -l) -gt 0 ]; then
log_message "Successfully copied files to app/data/"
# Clean up source directory if copy was successful
rm -rf zenodo_data/data >> $LOG_FILE 2>&1 || log_error "Failed to remove zenodo_data/data directory"
else
  log_error "No files were copied to app/data/"
fi
else
  log_error "data directory not found in zenodo_data"
log_message "Contents of zenodo_data:"
ls -la zenodo_data/ >> $LOG_FILE 2>&1

# Try to find any data files and move them directly
log_message "Searching for data files in zenodo_data..."
if [ $(find zenodo_data -type f | wc -l) -gt 0 ]; then
log_message "Found files in zenodo_data, moving directly to app/data/"
cp -R zenodo_data/* app/data/ >> $LOG_FILE 2>&1 || log_error "Failed to copy data files"
fi
fi

# Clean up zenodo_data if it exists and is empty
if [ -d "zenodo_data" ]; then
if [ $(ls -1A zenodo_data/ | wc -l) -eq 0 ]; then
log_message "Removing empty zenodo_data directory"
rmdir zenodo_data >> $LOG_FILE 2>&1 || log_error "Failed to remove zenodo_data directory"
else
  log_message "zenodo_data directory is not empty after moves, contents:"
ls -la zenodo_data/ >> $LOG_FILE 2>&1
fi
fi

# Create an empty login folder as a placeholder
log_message "Creating empty login folder as a place holder"
mkdir -p app/login/ || { handle_error "Failed to create app/login directory"; exit 1; }
else
  log_message "Skipping file movement steps (--skip-movefiles specified)"
fi

# Step 5: Create login folder and setup authentication files
if $RUN_AUTH; then
log_message "Setting up authentication system..."


cp setup_auth.R app/login/ || { handle_error "Failed to copy setup_auth.R"; exit 1; }

log_message "Creating authentication database and encryption keys..."
docker run --rm -v "$ABSOLUTE_PATH:$ABSOLUTE_PATH" -w "$ABSOLUTE_PATH/app/login" \
--entrypoint bash r-base:latest -c "
      # Install system dependencies
      apt-get update -qq && \
      apt-get install -y --no-install-recommends libssl-dev libsodium-dev && \
      
      # Install R packages
      R -e \"install.packages(c('DBI', 'RSQLite', 'sodium', 'openssl', 'shinymanager', 'bcrypt', 'optparse'), repos='https://cloud.r-project.org')\" && \
      
      # Run the script with parameters
      Rscript setup_auth.R \
    --passphrase=\"$DB_PASSPHRASE\" \
    --regular_user=\"$REGULAR_USERNAME\" \
    --regular_pwd=\"$REGULAR_PASSWORD\" \
    --regular_email=\"$REGULAR_EMAIL\" \
    --admin_user=\"$ADMIN_USERNAME\" \
    --admin_pwd=\"$ADMIN_PASSWORD\" \
    --admin_email=\"$ADMIN_EMAIL\" \
    --api_key=\"$OPENAI_FALLBACK_KEY\"
    " >> $LOG_FILE 2>&1 || { handle_error "Authentication setup failed"; exit 1; }
else
  log_message "Skipping authentication setup (--skip-auth specified)"
fi

# Step 6: Build Docker images
if $RUN_DOCKER; then
log_message "Building Docker images..."

# Create a directory for Docker build logs
mkdir -p docker_build_logs

# Build paperqa-endpoint image
log_message "Building paperqa-endpoint:V0..."
cd services/paperqa || { handle_error "Failed to change to paperqa directory"; exit 1; }
# Save complete Docker output to dedicated log file
docker build --no-cache -t paperqa-endpoint:V0 . > $ABSOLUTE_PATH/docker_build_logs/paperqa_build.log 2>&1
if [ $? -ne 0 ]; then
log_error "Failed to build paperqa-endpoint image. See docker_build_logs/paperqa_build.log for details"
exit 1
fi
log_message "paperqa-endpoint:V0 build completed successfully. Full log saved to docker_build_logs/paperqa_build.log"
cd ../../
  
  # Build rag_repro image
  log_message "Building rag_repro:V0..."
cd services/RAG || { handle_error "Failed to change to RAG directory"; exit 1; }
# Save complete Docker output to dedicated log file
docker build --no-cache -t rag_repro:V0 . > $ABSOLUTE_PATH/docker_build_logs/rag_build.log 2>&1
if [ $? -ne 0 ]; then
log_error "Failed to build rag_repro image. See docker_build_logs/rag_build.log for details"
exit 1
fi
log_message "rag_repro:V0 build completed successfully. Full log saved to docker_build_logs/rag_build.log"
cd ../../
  
  # Build direg image
  log_message "Building direg:V0..."
cd app || { handle_error "Failed to change to app directory"; exit 1; }
# Save complete Docker output to dedicated log file
docker build --no-cache -f dockerfile -t direg:V0 . > $ABSOLUTE_PATH/docker_build_logs/direg_build.log 2>&1
if [ $? -ne 0 ]; then
log_error "Failed to build direg image. See docker_build_logs/direg_build.log for details"
exit 1
fi
log_message "direg:V0 build completed successfully. Full log saved to docker_build_logs/direg_build.log"
cd ../
  
  log_message "All Docker build logs have been saved to the docker_build_logs directory"
else
  log_message "Skipping Docker image building (--skip-docker specified)"
fi

# Step 7: Update configuration and create Docker network
if $RUN_CONFIG; then
log_message "Updating ShinyProxy configuration with absolute paths..."
# Make a backup of the original file
cp deployment/shinyproxy/application.yml deployment/shinyproxy/application.yml.bak || { handle_error "Failed to backup application.yml"; exit 1; }

# Use sed to replace relative paths with absolute paths in application.yml
sed -i "s|\"../../app/login:/srv/shiny-server/login\"|\"$ABSOLUTE_PATH/app/login:/srv/shiny-server/login\"|g" deployment/shinyproxy/application.yml
sed -i "s|\"../../app/data:/srv/shiny-server/data\"|\"$ABSOLUTE_PATH/app/data:/srv/shiny-server/data\"|g" deployment/shinyproxy/application.yml
sed -i "s|\"../../app/refGenome:/srv/shiny-server/refGenome\"|\"$ABSOLUTE_PATH/app/refGenome:/srv/shiny-server/refGenome\"|g" deployment/shinyproxy/application.yml

# Changing permissions of application.yml
chmod 644 deployment/shinyproxy/application.yml || { handle_error "Failed to change permissions of application.yml"; exit 1; }

# Create Docker network
log_message "Creating Docker network direg-net..."
docker network create direg-net >> $LOG_FILE 2>&1 || log_error "Failed to create docker network (it might already exist)"
else
  log_message "Skipping configuration update and network creation (--skip-config specified)"
fi

log_message "Setup complete! Project structure is ready and Docker images are built."
log_message "Docker images created:"
log_message "  - paperqa-endpoint:V0"
log_message "  - rag_repro:V0"
log_message "  - direg:V0"
log_message "Docker network created: direg-net"
log_message "Absolute paths have been set in application.yml"

# Step 8: Starting the application
if $RUN_START; then
log_message "Starting the application"
log_message "App can be accessed at IP:8080/direg/"
cd deployment/shinyproxy/ || { handle_error "Failed to change to shinyproxy directory"; exit 1; }
docker compose up >> $LOG_FILE 2>&1
else
  log_message "Skipping application startup (--skip-start specified)"
log_message "To start the application manually, run: cd deployment/shinyproxy/ && docker compose up"
log_message "App can be accessed at IP:8080/direg/"
fi