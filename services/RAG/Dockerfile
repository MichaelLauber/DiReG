FROM python:3.10-slim

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install Python dependencies
RUN pip install --upgrade pip
RUN pip install \
    fastapi \
    uvicorn \
    langchain \
    langchain-openai \
    langchain-chroma \
    chromadb \
    pydantic
    
    
# Copy the Chroma database directly into the container
# This assumes the openai_chroma_db directory exists in the build context
COPY chromadb_w_openaiembedding_semantic_chuncking /app/chromadb_w_openaiembedding_semantic_chuncking    

# Copy application files
COPY rag_pipeline_for_r.py /app/
COPY rag_pipeline_api.py /app/


# Expose the port the app runs on
EXPOSE 8008

# Command to run the application
CMD ["uvicorn", "rag_pipeline_api:app", "--host", "0.0.0.0", "--port", "8008"]