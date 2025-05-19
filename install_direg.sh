#!/bin/sh

# Set variables
GITHUB_REPO="https://github.com/MichaelLauber/DiReG_APP.git"  
PROJECT_DIR="direg_project"

echo "Setting up the direg project..."

# Step 1: Clone the repository
echo "Cloning the repository from GitHub..."
git clone $GITHUB_REPO $PROJECT_DIR
cd $PROJECT_DIR || { echo "Failed to change to project directory"; exit 1; }

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

# Move all_free_pdfs_index to services/paperqa
if [ -d "all_free_pdfs_index" ]; then
    echo "Moving all_free_pdfs_index to services/paperqa"
    mv all_free_pdfs_index services/paperqa/
fi

# Move chromadb_w_openaiembedding_semantic_chuncking to services/RAG
if [ -d "chromadb_w_openaiembedding_semantic_chuncking" ]; then
    echo "Moving chromadb_w_openaiembedding_semantic_chuncking to services/RAG"
    mv chromadb_w_openaiembedding_semantic_chuncking services/RAG/
fi

# Move free_reprogramming_pdfs to services/paperqa
if [ -d "free_reprogramming_pdfs" ]; then
    echo "Moving free_reprogramming_pdfs to services/paperqa"
    mv free_reprogramming_pdfs services/paperqa/
fi

# Move data directory to app (if it exists and isn't already there)
if [ -d "data" ] && [ ! -d "app/data/zenodo_data" ]; then
    echo "Moving data to app/data"
    mkdir -p app/data/
    mv data/* app/data
    rmdir data
fi

# Step 5: Build Docker images
echo "Building Docker images..."

# Build paperqa-endpoint:V0
echo "Building paperqa-endpoint:V0..."
cd services/paperqa || { echo "Failed to change to paperqa directory"; exit 1; }
docker build -t paperqa-endpoint:V0 .
cd ../../

# Build rag_repro:V0
echo "Building rag_repro:V0..."
cd services/RAG || { echo "Failed to change to RAG directory"; exit 1; }
docker build -t rag_repro:V0 .
cd ../../

# Build direg:V0
echo "Building direg:V0..."
cd app || { echo "Failed to change to app directory"; exit 1; }
docker build -f dockerfile -t direg:V0 .
cd ../

# Step 6: Create Docker network
echo "Creating Docker network direg-net..."
docker network create direg-net

echo "Setup complete! Project structure is ready and Docker images are built."
echo "Docker images created:"
echo "  - paperqa-endpoint:V0"
echo "  - rag_repro:V0"
echo "  - direg:V0"
echo "Docker network created: direg-net"