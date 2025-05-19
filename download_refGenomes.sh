#!/bin/sh

# Default output directory is current directory
OUTPUT_DIR="."

# Check if argument is provided
if [ $# -gt 0 ]; then
    OUTPUT_DIR="$1"
fi

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"
cd "$OUTPUT_DIR" || { echo "Failed to change to directory $OUTPUT_DIR"; exit 1; }

echo "Downloading reference genomes to: $OUTPUT_DIR"

# Function to download, unpack, and index a genome file
download_genome() {
    url="$1"
    filename=$(basename "$url")
    genome_name=$(echo "$filename" | sed 's/\.fa\.gz$//')

    echo "Downloading $genome_name..."
    
    # Check if file already exists
    if [ -f "${genome_name}.fa" ]; then
        echo "File ${genome_name}.fa already exists. Skipping download."
        return
    fi
    
    # Download the file
    wget -q --show-progress "$url" -O "$filename"
    
    if [ $? -ne 0 ]; then
        echo "Failed to download $url"
        return
    fi
    
    # Unpack the file
    echo "Unpacking $filename..."
    gunzip -c "$filename" > "${genome_name}.fa"
    
    # Create index file
    echo "Creating index for ${genome_name}.fa..."
    if command -v samtools >/dev/null 2>&1; then
        samtools faidx "${genome_name}.fa"
    else
        echo "Warning: samtools not found. Index file (.fai) not created for ${genome_name}.fa"
    fi
    
    # Remove the compressed file to save space
    rm "$filename"
    
    echo "Completed processing $genome_name"
    echo "-------------------------"
}

# Download each genome individually
echo "Downloading hg38..."
download_genome "https://hgdownload.soe.ucsc.edu/goldenPath/hg38/bigZips/hg38.fa.gz"

echo "Downloading hg19..."
download_genome "https://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/hg19.fa.gz"

echo "Downloading mm10..."
download_genome "https://hgdownload.soe.ucsc.edu/goldenPath/mm10/bigZips/mm10.fa.gz"

echo "Downloading mm39..."
download_genome "https://hgdownload.soe.ucsc.edu/goldenPath/mm39/bigZips/mm39.fa.gz"

echo "Downloading GRCh37 (Ensembl)..."
download_genome "https://ftp.ensembl.org/pub/release-75/fasta/homo_sapiens/dna/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz"

echo "Downloading GRCh38 (Ensembl)..."
download_genome "https://ftp.ensembl.org/pub/release-110/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.primary_assembly.fa.gz"

echo "Downloading GRCm38 (Ensembl)..."
download_genome "https://ftp.ensembl.org/pub/release-100/fasta/mus_musculus/dna/Mus_musculus.GRCm38.dna.primary_assembly.fa.gz"

echo "Downloading GRCm39 (Ensembl)..."
download_genome "https://ftp.ensembl.org/pub/release-110/fasta/mus_musculus/dna/Mus_musculus.GRCm39.dna.primary_assembly.fa.gz"

# Rename Ensembl files to match the requested names
if [ -f "Homo_sapiens.GRCh37.75.dna.primary_assembly.fa" ]; then
    echo "Renaming Homo_sapiens.GRCh37.75.dna.primary_assembly.fa to GRCh37.fa"
    mv "Homo_sapiens.GRCh37.75.dna.primary_assembly.fa" "GRCh37.fa"
    if [ -f "Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.fai" ]; then
        mv "Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.fai" "GRCh37.fa.fai"
    fi
fi

if [ -f "Homo_sapiens.GRCh38.dna.primary_assembly.fa" ]; then
    echo "Renaming Homo_sapiens.GRCh38.dna.primary_assembly.fa to GRCh38.fa"
    mv "Homo_sapiens.GRCh38.dna.primary_assembly.fa" "GRCh38.fa"
    if [ -f "Homo_sapiens.GRCh38.dna.primary_assembly.fa.fai" ]; then
        mv "Homo_sapiens.GRCh38.dna.primary_assembly.fa.fai" "GRCh38.fa.fai"
    fi
fi

if [ -f "Mus_musculus.GRCm38.dna.primary_assembly.fa" ]; then
    echo "Renaming Mus_musculus.GRCm38.dna.primary_assembly.fa to GRCm38.fa"
    mv "Mus_musculus.GRCm38.dna.primary_assembly.fa" "GRCm38.fa"
    if [ -f "Mus_musculus.GRCm38.dna.primary_assembly.fa.fai" ]; then
        mv "Mus_musculus.GRCm38.dna.primary_assembly.fa.fai" "GRCm38.fa.fai"
    fi
fi

if [ -f "Mus_musculus.GRCm39.dna.primary_assembly.fa" ]; then
    echo "Renaming Mus_musculus.GRCm39.dna.primary_assembly.fa to GRCm39.fa"
    mv "Mus_musculus.GRCm39.dna.primary_assembly.fa" "GRCm39.fa"
    if [ -f "Mus_musculus.GRCm39.dna.primary_assembly.fa.fai" ]; then
        mv "Mus_musculus.GRCm39.dna.primary_assembly.fa.fai" "GRCm39.fa.fai"
    fi
fi

echo "All reference genomes have been downloaded and processed."
echo "Files are located in: $OUTPUT_DIR"

# List all downloaded files
echo "Downloaded files:"
ls -lh "$OUTPUT_DIR"/*.fa
