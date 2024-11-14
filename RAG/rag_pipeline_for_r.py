import os
import json
import logging
import time
from typing import List, Dict
from functools import wraps

#from langchain.vectorstores import Chroma
#from langchain.embeddings import OpenAIEmbeddings
#from langchain.chat_models import ChatOpenAI
from langchain.prompts import PromptTemplate
from langchain.retrievers import MultiQueryRetriever
from langchain.chains import LLMChain
from langchain_core.output_parsers import StrOutputParser
from langchain.schema import Document

from langchain_openai import ChatOpenAI
from langchain_openai import OpenAIEmbeddings
from langchain_chroma import Chroma

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# DATA_DIR = os.getenv('DATA_DIR', '/home/michael/Documents/reprogramming_papers')
# if not os.path.isdir(DATA_DIR):
#     logger.error(f"Data directory {DATA_DIR} does not exist.")
#     exit(1)
# os.chdir(DATA_DIR)

# Ensure OpenAI API key is set
# OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
# if not OPENAI_API_KEY:
#     logger.error("OpenAI API key not found. Please set the OPENAI_API_KEY environment variable.")
#     exit(1)

OPENAI_API_KEY = "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"    
os.environ["OPENAI_API_KEY"] = OPENAI_API_KEY


def retry(exceptions, tries=4, delay=3, backoff=2):
    """
    Retry decorator with exponential backoff.
    """
    def deco_retry(f):
        @wraps(f)
        def f_retry(*args, **kwargs):
            mtries, mdelay = tries, delay
            while mtries > 0:
                try:
                    return f(*args, **kwargs)
                except exceptions as e:
                    logger.warning(f"{e}, Retrying in {mdelay} seconds...")
                    time.sleep(mdelay)
                    mtries -= 1
                    mdelay *= backoff
                raise Exception("Maximum retry attempts exceeded")
        return f_retry
    return deco_retry

@retry(Exception, tries=4, delay=2, backoff=2)
def predict_with_retry(llm: ChatOpenAI, prompt: str) -> str:
    """
    Calls the LLM's predict method with retry logic.
    """
    return llm.predict(prompt)

def extract_references(docs: List[Document]) -> str:
    """
    Extract unique source details from retrieved documents.
    """
    references = []
    seen = set()
    for i, document in enumerate(docs):
        title = document.metadata.get('title', 'Unknown')
        first_author = document.metadata.get('first_author', 'Unknown')
        doi = document.metadata.get('doi', 'Unknown')
        ref_key = (title, first_author, doi)
        if ref_key not in seen:
            seen.add(ref_key)
            ref = f"[{i+1}] Title: {title}, First Author: {first_author}, DOI: {doi}"
            references.append(ref)
    return "\n".join(references)

def prepare_context_with_references(docs: List[Document]) -> str:
    """
    Prepare the context by combining document content with metadata.
    """
    context_with_refs = []
    for i, document in enumerate(docs):
        title = document.metadata.get('title', 'Unknown Title')
        first_author = document.metadata.get('first_author', 'Unknown Author')
        doi = document.metadata.get('doi', 'Unknown DOI')
        context_with_refs.append(f"Content from Document [{i+1}] (Title: {title}, First Author: {first_author}, DOI: {doi}):\n{document.page_content}\n")
    return "\n".join(context_with_refs)

# Global variable to store the pipeline
_pipeline = None

def initialize_pipeline():
    """
    Initialize the pipeline components and return them.
    """
    global _pipeline
    if _pipeline is not None:
        return _pipeline

    # Initialize the language model
    language_model = ChatOpenAI(model_name="gpt-4o-mini", temperature=0)

    # Load the existing vector database from disk
    persist_directory = "./openai_chroma_db"
    embedding = OpenAIEmbeddings()

    vector_db = Chroma(
        collection_name="local-rag",
        persist_directory=persist_directory,
        embedding_function=embedding
    )

    # Create query prompt
    QUERY_PROMPT = PromptTemplate(
        input_variables=["question"],
        template="""You are an AI language model assistant. Your task is to generate five
different versions of the given user question to retrieve relevant documents from
a vector database. By generating multiple perspectives on the user question, your
goal is to help the user overcome some of the limitations of the distance-based
similarity search. Provide these alternative questions separated by newlines.
Original question: {question}"""
    )

    # Set up the retriever
    retriever = MultiQueryRetriever.from_llm(
        retriever=vector_db.as_retriever(),
        llm=language_model,
        prompt=QUERY_PROMPT
    )

    # Create RAG prompt
    rag_template = """Answer the question based ONLY on the following context, and include in-text citations using 
the format [1], [2], etc., matching each citation only to the sources provided in the reference list. Avoid adding citations that are not in the reference list. Ensure each referenced work includes "Author et al." in the authorship format.

Your response should conclude with a 'References' section, listing only the sources cited 
in the answer, and each entry should include the title, first author in "Author et al." 
format, and DOI.

{context}

Question: {question}


References:
{references}
"""

    rag_prompt = PromptTemplate(
        input_variables=["context", "references", "question"],
        template=rag_template
    )

    # Initialize the LLMChain
    chain = LLMChain(
        llm=language_model,
        prompt=rag_prompt,
        output_parser=StrOutputParser()
    )

    _pipeline = {
        'chain': chain,
        'retriever': retriever
    }

    return _pipeline

def process_query(user_question: str) -> str:
    """
    Process the user question and return the final result.
    """
    pipeline = initialize_pipeline()
    chain = pipeline['chain']
    retriever = pipeline['retriever']

    # Retrieve relevant documents
    retrieved_docs = retriever.get_relevant_documents(user_question)

    # Prepare the context and references
    context = prepare_context_with_references(retrieved_docs)
    references = extract_references(retrieved_docs)

    # Run the chain
    result = chain.run(context=context, references=references, question=user_question)

    return result
