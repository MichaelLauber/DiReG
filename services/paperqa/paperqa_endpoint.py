#!/usr/bin/env python3
from fastapi import FastAPI, BackgroundTasks
from pydantic import BaseModel
import os
from paperqa import Settings, ask
import threading
import queue
import time

app = FastAPI()

# Create a single threaded queue for processing requests sequentially
request_queue = queue.Queue()
worker_thread = None
worker_lock = threading.Lock()

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

# Flag to indicate if worker is running
is_worker_running = False

def process_queue_worker():
    """Worker function that processes requests from the queue one at a time"""
    global is_worker_running
    
    while True:
        try:
            # Get the next request from the queue (blocks until one is available)
            task = request_queue.get(block=True, timeout=60)  # 1 minute timeout
            
            # Process the request
            req = task["request"]
            callback = task["callback"]
            
            try:
                # Set the API key for this specific request
                os.environ["OPENAI_API_KEY"] = req.api_key
                
                # Create and configure PaperQA Settings
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
                settings.agent.index.sync_with_paper_directory = False
                
                settings.agent.agent_llm = req.agent_llm
                settings.answer.max_answer_attempts = req.max_answer_attempts
                
                # Mode-specific configuration
                if req.mode == "high_quality":
                    settings.answer.evidence_k = 15
                    settings.agent.agent_type = "ToolSelector"
                elif req.mode == "fast":
                    settings.answer.evidence_k = 5
                
                # Run the PaperQA query
                answer_response = ask(req.question, settings=settings)
                formatted_answer = answer_response.session.formatted_answer
                
                # Call the callback with the result
                callback({"formatted_answer": formatted_answer})
                
            except Exception as e:
                # Call the callback with the error
                callback({"error": str(e)})
            
            finally:
                # Mark the task as done
                request_queue.task_done()
        
        except queue.Empty:
            # If no tasks for 60 seconds, exit the worker thread
            with worker_lock:
                is_worker_running = False
                break
        
        except Exception as e:
            # Log any unexpected errors
            print(f"Unexpected error in worker thread: {str(e)}")
            time.sleep(1)  # Prevent tight loop on repeated errors

def ensure_worker_running():
    """Ensure the worker thread is running"""
    global worker_thread, is_worker_running
    
    with worker_lock:
        if not is_worker_running:
            is_worker_running = True
            worker_thread = threading.Thread(target=process_queue_worker, daemon=True)
            worker_thread.start()

@app.post("/query")
async def query_endpoint(req: QueryRequest, background_tasks: BackgroundTasks):
    # Create a dict to store the result
    result_container = {"status": "pending"}
    
    # Create an event to signal when the result is ready
    result_ready = threading.Event()
    
    # Define the callback function
    def on_complete(result):
        result_container.update(result)
        result_container["status"] = "completed"
        result_ready.set()
    
    # Add the task to the queue
    request_queue.put({
        "request": req,
        "callback": on_complete
    })
    
    # Ensure the worker is running
    ensure_worker_running()
    
    # Wait for the result (with timeout)
    # Note: In a real production app, you might want to use a streaming response or webhooks instead
    timeout_seconds = 300  # 5 minutes
    if not result_ready.wait(timeout=timeout_seconds):
        return {"error": "Request timed out"}
    
    # Return the result
    if "error" in result_container:
        return {"error": result_container["error"]}
    else:
        return {"formatted_answer": result_container["formatted_answer"]}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("paperqa_endpoint:app", host="0.0.0.0", port=8000, reload=True)
