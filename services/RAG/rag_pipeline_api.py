from fastapi import FastAPI
from pydantic import BaseModel
import uvicorn

app = FastAPI()

# Import your existing functions
from rag_pipeline_for_r import process_query

class QueryRequest(BaseModel):
    question: str
    api_key: str  

@app.post("/process_query")
def process_query_endpoint(request: QueryRequest):
    # Pass both the question and API key to the process_query function
    result = process_query(request.question, request.api_key)
    return {"result": result}

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8008)
