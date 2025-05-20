# Use a lightweight Python 3.11 image
FROM python:3.11-slim

# Set the working directory inside the container
WORKDIR /app

# Install system dependencies (if needed by PaperQA)
RUN apt-get update && apt-get install -y \
    build-essential \
    libpoppler-dev \
    && rm -rf /var/lib/apt/lists/*

# Upgrade pip and install required Python packages
RUN pip install --upgrade pip
RUN pip install fastapi uvicorn paper-qa pydantic


# Copy your FastAPI app code into the container
COPY paperqa_endpoint.py /app/paperqa_endpoint.py

# Copy the existing index folder
COPY all_free_pdfs_index /app/all_free_pdfs_index

# Expose port 8000
EXPOSE 5555

# Command to run your FastAPI app
CMD ["uvicorn", "paperqa_endpoint:app", "--host", "0.0.0.0", "--port", "5555"]
