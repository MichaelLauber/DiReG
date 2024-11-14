from fastapi import FastAPI
from pydantic import BaseModel
import uvicorn

app = FastAPI()

# Import your existing functions
from rag_pipeline_for_r import process_query

class QueryRequest(BaseModel):
    question: str

@app.post("/process_query")
def process_query_endpoint(request: QueryRequest):
    result = process_query(request.question)
    return {"result": result}

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
