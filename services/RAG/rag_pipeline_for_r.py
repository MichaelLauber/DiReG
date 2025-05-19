import os
import json
import logging
import time
from typing import List, Dict, Tuple
from functools import wraps
import re


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


def build_document_ref_mapping(docs: List[Document]) -> Tuple[Dict, List[Dict]]:
    """
    Build a mapping of document metadata to consistent reference numbers.
    Returns a tuple of (reference_mapping, unique_references)
    """
    ref_mapping = {}
    unique_refs = []
    ref_counter = 1
    
    for document in docs:
        title = document.metadata.get('Title', 'Unknown Title')
        pubmed_id = document.metadata.get('Pubmed_ID', 'Unknown Pubmed_ID')
        pmc_id = document.metadata.get('PMC_ID', 'Unknown PMC_ID')
        journal = document.metadata.get('Journal', 'Unknown Journal')
        
        ref_key = (title, journal, pubmed_id, pmc_id)
        if ref_key not in ref_mapping:
            ref_mapping[ref_key] = ref_counter
            unique_refs.append({
                'ref_num': ref_counter,
                'title': title,
                'journal': journal,
                'pubmed_id': pubmed_id,
                'pmc_id': pmc_id
            })
            ref_counter += 1
    
    return ref_mapping, unique_refs

def prepare_context_with_references(docs: List[Document], ref_mapping: Dict) -> str:
    """
    Prepare the context by combining document content with consistent reference numbers.
    """
    context_with_refs = []
    for document in docs:
        title = document.metadata.get('Title', 'Unknown Title')
        pubmed_id = document.metadata.get('Pubmed_ID', 'Unknown Pubmed_ID')
        pmc_id = document.metadata.get('PMC_ID', 'Unknown PMC_ID')
        journal = document.metadata.get('Journal', 'Unknown Journal')
        
        ref_key = (title, journal, pubmed_id, pmc_id)
        ref_num = ref_mapping[ref_key]
        
        context_with_refs.append(f"Content from Document [{ref_num}] (Title: {title}, Journal: {journal}, pubmed_id: {pubmed_id}, pmc_id: {pmc_id}):\n{document.page_content}\n")
    
    return "\n".join(context_with_refs)

def extract_references(unique_refs: List[Dict]) -> str:
    """
    Format the unique references with consistent numbering.
    """
    formatted_refs = []
    for ref in unique_refs:
        ref_str = f"[{ref['ref_num']}] Title: {ref['title']}, Journal: {ref['journal']}, pubmed_id: {ref['pubmed_id']}, pmc_id: {ref['pmc_id']}"
        formatted_refs.append(ref_str)
    
    return "\n".join(formatted_refs)




  


def process_query(user_question: str, api_key: str) -> str:
    """
    Process the user question and return the final result.
    """
    language_model = ChatOpenAI(
        model_name="gpt-4o", 
        temperature=0,
        api_key=api_key  # Pass API key directly to the model
    )
    
    embedding = OpenAIEmbeddings(
        api_key=api_key  # Pass API key directly here too
    )
    
    # Load the existing vector database with the user's embedding function
    persist_directory = "./chromadb_w_openaiembedding_semantic_chuncking"
    vector_db = Chroma(
        collection_name="free_pmc",
        persist_directory=persist_directory,
        embedding_function=embedding
    )
    
    # Set up retriever with user's language model
    QUERY_PROMPT = PromptTemplate(
        input_variables=["question"],
        template="""You are an AI language model assistant. Your task is to generate five
                  different versions of the given user question to retrieve relevant documents from
                  a vector database. By generating multiple perspectives on the user question, your
                  goal is to help the user overcome some of the limitations of the distance-based
                  similarity search. Provide these alternative questions separated by newlines.
                  Original question: {question}"""
    )
    
    retriever = MultiQueryRetriever.from_llm(
        retriever=vector_db.as_retriever(search_kwargs={"k": 10}),
        llm=language_model,
        prompt=QUERY_PROMPT
    )
    
    # RAG prompt remains the same
    rag_template = """
                  Answer the question based ONLY on the following context, and include in-text citations using 
                  the format [1], [2], etc., matching each citation only to the sources provided in the reference list. 
                  Avoid adding citations that are not in the reference list. Try to reference as many 
                  different sources as appropriate to provide a comprehensive answer.
                  If the same information comes from two multiple sources then cite both because the user might be interrested in all of them
                  
                  Your response should conclude with a 'References' section, listing ONLY the exact reference information provided 
                  without modification. Do not add author names or modify titles. For each reference, use ONLY the title, journal, 
                  pubmed_id and pmc_id as provided in the references section below. However, remove references that are not cited.
                  If you remove citation you have to adjust the numbering. The citations should be numerically ordered starting from 1 and should not have gaps.
                  This means that after [1], [2] should follow, then [3], and so on in sequence.
                  
                  Context:
                  {context}
                  
                  Question: {question}
                  
                  References:
                  {references}
                  """
    
    rag_prompt = PromptTemplate(
        input_variables=["context", "references", "question"],
        template=rag_template
    )
    
    # Initialize user-specific chain
    chain = LLMChain(
        llm=language_model,
        prompt=rag_prompt,
        output_parser=StrOutputParser()
    )

    # Retrieve relevant documents
    retrieved_docs = retriever.get_relevant_documents(user_question)

    # Build reference mapping and unique references
    ref_mapping, unique_refs = build_document_ref_mapping(retrieved_docs)
    
    # Prepare context and references with consistent numbering
    context = prepare_context_with_references(retrieved_docs, ref_mapping)
    references = extract_references(unique_refs)

    # Run the chain
    result = chain.run(context=context, references=references, question=user_question)
    
    return result
