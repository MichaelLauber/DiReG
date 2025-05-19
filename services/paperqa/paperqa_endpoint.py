#!/usr/bin/env python3
from fastapi import FastAPI
from pydantic import BaseModel
import os
from paperqa import Settings, ask

app = FastAPI()

class QueryRequest(BaseModel):
    question: str
    temperature: float
    rate_limit: str
    folder: str           # full path to folder containing your papers inside the container
    mode: str             # "fast" or "high_quality"
    llm: str
    summary_llm: str
    agent_llm: str
    max_answer_attempts: int
    api_key: str          # OpenAI API key

@app.post("/query")
def query_endpoint(req: QueryRequest):
    # Set the OPENAI_API_KEY environment variable from the request
    os.environ["OPENAI_API_KEY"] = req.api_key

    # Create and configure PaperQA Settings from the incoming request.
    settings = Settings(
        temperature=req.temperature,
        paper_directory=req.folder,
        llm=req.llm,
        summary_llm=req.summary_llm,
        llm_config={"rate_limit": {req.llm: req.rate_limit}},
        summary_llm_config={"rate_limit": {req.summary_llm: req.rate_limit}},
    )
    
    settings.agent.index.name = "all_free_pdfs_index"
    settings.agent.index.index_directory = "/app"
    settings.agent.index.sync_with_paper_directory = False  # Don't rebuild the index
    
    settings.agent.agent_llm = req.agent_llm
    settings.answer.max_answer_attempts = req.max_answer_attempts

    # Mode-specific configuration.
    if req.mode == "high_quality":
        settings.answer.evidence_k = 15
        settings.agent.agent_type = "ToolSelector"
    elif req.mode == "fast":
        settings.answer.evidence_k = 5

    # Run the PaperQA query.
    #settings = Settings(paper_directory="/app/reprogramming_papers")
    answer_response = ask(req.question, settings=settings)
    formatted_answer = answer_response.session.formatted_answer

    # Return the formatted answer as JSON.
    return {"formatted_answer": formatted_answer}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("paperqa_endpoint:app", host="0.0.0.0", port=8000, reload=True)
