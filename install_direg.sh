#!/bin/sh

# Set variables
GITHUB_REPO="https://github.com/MichaelLauber/DiReG.git"  
PROJECT_DIR="direg_project"
echo "Setting up the direg project..."

# Step 1: Clone the repository
echo "Cloning the repository from GitHub..."
git clone $GITHUB_REPO $PROJECT_DIR
cd $PROJECT_DIR || { echo "Failed to change to project directory"; exit 1; }

# Get absolute path of the current directory for volume mounting
ABSOLUTE_PATH=$(pwd)
echo "Absolute path of project: $ABSOLUTE_PATH"

# Step 2: Download reference genomes and place them in app/refGenome
echo "Downloading reference genomes..."
mkdir -p app/refGenome
chmod +x download_refGenomes.sh
./download_refGenomes.sh app/refGenome

# Step 3: Download Zenodo data
echo "Downloading Zenodo data..."
chmod +x download_zenodo_data.sh
./download_zenodo_data.sh

# Step 4: Move the extracted Zenodo files to their correct locations
echo "Moving files to correct locations..."
echo "Moving all_free_pdfs_index to services/paperqa"
mv zenodo_data/all_free_pdfs_index services/paperqa/
echo "Moving chromadb_w_openaiembedding_semantic_chuncking to services/RAG"
mv zenodo_data/chromadb_w_openaiembedding_semantic_chuncking services/RAG/
echo "Moving free_reprogramming_pdfs to services/paperqa"
mv zenodo_data/free_reprogramming_pdfs services/paperqa/
echo "Moving data to app/data"
mkdir -p app/data/
mv zenodo_data/data/* app/data
rmdir zenodo_data
echo "Creating empty login folder as a place holder"
mkdir -p app/login/

# Step 5: Create login folder and setup authentication files
echo "Setting up authentication system..."
mkdir -p app/login/

# User configuration for authentication
DB_PASSPHRASE="alphaCentaury"
REGULAR_USERNAME="dummyUser"
REGULAR_PASSWORD="StrongPW!"
REGULAR_EMAIL="user@example.com"
ADMIN_USERNAME="dummyAdmin"
ADMIN_PASSWORD="StrongPWAdmin"
ADMIN_EMAIL="admin@example.com"
OPENAI_FALLBACK_KEY="sk-YOUR_FALLBACK_KEY_HERE"

cp setup_auth.R app/login/

echo "Creating authentication database and encryption keys..."
docker run --rm -v "$ABSOLUTE_PATH/app/login:/data" -w /data r-base:latest Rscript setup_auth.R \
  --passphrase="$DB_PASSPHRASE" \
  --regular_user="$REGULAR_USERNAME" \
  --regular_pwd="$REGULAR_PASSWORD" \
  --regular_email="$REGULAR_EMAIL" \
  --admin_user="$ADMIN_USERNAME" \
  --admin_pwd="$ADMIN_PASSWORD" \
  --admin_email="$ADMIN_EMAIL" \
  --api_key="$OPENAI_FALLBACK_KEY"

# Step 6: Build Docker images
echo "Building Docker images..."


echo "Building paperqa-endpoint:V0..."
cd services/paperqa || { echo "Failed to change to paperqa directory"; exit 1; }
docker build --no-cache -t paperqa-endpoint:V0 .
cd ../../


echo "Building rag_repro:V0..."
cd services/RAG || { echo "Failed to change to RAG directory"; exit 1; }
docker build  --no-cache -t rag_repro:V0 .
cd ../../


echo "Building direg:V0..."
cd app || { echo "Failed to change to app directory"; exit 1; }
docker build  --no-cache -f dockerfile -t direg:V0 .
cd ../

echo "Updating ShinyProxy configuration with absolute paths..."
# Make a backup of the original file
cp deployment/shinyproxy/application.yml deployment/shinyproxy/application.yml.bak

# Use sed to replace relative paths with absolute paths in application.yml
sed -i "s|\"../../app/login:/srv/shiny-server/login\"|\"$ABSOLUTE_PATH/app/login:/srv/shiny-server/login\"|g" deployment/shinyproxy/application.yml
sed -i "s|\"../../app/data:/srv/shiny-server/data\"|\"$ABSOLUTE_PATH/app/data:/srv/shiny-server/data\"|g" deployment/shinyproxy/application.yml
sed -i "s|\"../../app/refGenome:/srv/shiny-server/refGenome\"|\"$ABSOLUTE_PATH/app/refGenome:/srv/shiny-server/refGenome\"|g" deployment/shinyproxy/application.yml

# Changing permissions of application.yml
chmod 644 deployment/shinyproxy/application.yml 

# Step 6: Create Docker network
echo "Creating Docker network direg-net..."
docker network create direg-net

echo "Setup complete! Project structure is ready and Docker images are built."
echo "Docker images created:"
echo "  - paperqa-endpoint:V0"
echo "  - rag_repro:V0"
echo "  - direg:V0"
echo "Docker network created: direg-net"
echo "Absolute paths have been set in application.yml"


# Step 7: Starting the application
echo "Starting the application"
echo "App can be accessed at IP:8080/direg/"
cd deployment/shinyproxy/
docker compose up