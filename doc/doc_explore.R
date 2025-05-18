div(
    h2("Literature Exploration Systems"),
    
    p("We have integrated", a(href = "https://arxiv.org/abs/2312.07559/", "PaperQA"), 
      "alongside a custom-built RAG system, both powered by a carefully curated collection of 360 freely available publications on direct reprogramming protocols. 
    This dual approach to literature exploration helps minimize hallucinations common in Large Language Models (LLMs) 
    by ensuring answers are directly sourced from full-text scientific articles."),
    
    p("Our literature database is designed for seamless updates through an automated pipeline that can be run periodically 
    to incorporate newly published research without manual intervention. We utilize the NCBI E-utilities API 
    with a standardized search query targeting direct reprogramming publications while excluding reviews and other non-primary content. 
    The automated pipeline performs title-based filtration using a LLM to classify publications, 
    retaining only those relevant to cell biology."),
    
    p("To systematically identify primary research articles, we query the NCBI database using the search term: 
    \"", em("direct reprogramming"), "[Title/Abstract] OR ", em("direct conversion"), "[Title/Abstract]) 
    NOT (Review[Publication Type] OR Meta-Analysis[Publication Type] OR Systematic Review[Publication Type] 
    OR Editorial[Publication Type] OR Comment[Publication Type] OR Letter[Publication Type]) 
    AND ", em("free full text"), "[Filter]\"."),
    
    h3("Two Complementary Systems"),
    
    strong("PaperQA:"), p("This agent provides high-accuracy responses that surpass human performance in scientific literature research. 
    Users can register with their OpenAI account to query the literature using their preferred model. PaperQA offers two operational modes:"),
    
    tags$ul(
      tags$li(strong("Fast Mode:"), "Examines limited evidence (k=5) for rapid responses, suitable for most inquiries"),
      tags$li(strong("High Quality Mode:"), "Performs comprehensive analysis with expanded evidence retrieval (k=15), ideal for more complex questions")
    ),
    
    p("While a single PaperQA request costs several cents and may take minutes to process, 
    users can adjust the Temperature setting to moderate the creativity and potential hallucination in responses."),
    
    strong("Custom RAG System:"), p("We've also implemented a lightweight complementary RAG system that delivers responses 
    in under 30 seconds at less than 1 cent per query. This system:"),
    
    tags$ul(
      tags$li("Splits documents into 300+ character long chunks using contextual chuncking"),
      tags$li("Embeds content using OpenAI Embedding model and stores in a Chroma vector database"),
      tags$li("Employs multi-query retrieval to improve search coverage"),
    ),
    
    p("This dual-system approach provides both depth and efficiency, helping users quickly retrieve information 
    about existing protocols and experimental details while circumventing time-consuming literature searches."),
  
  
  img(src="explore_rag_doc.png", style="max-width:100%; height:auto; display:block; margin:auto;"),
  br(),
  
  h2("Computational Approaches for TF Prediction"),
  
  p("Several computational approaches have been proposed to infer sets of TF for transdifferentiation.
    While these methods are still far away from delivering predictions with a high accurracy, 
    they can provide a starting point for validation of various sets in the laboratory.
    Here we gatherred prediction from some of the most popular tools in the field 
    and can be now easily browsed by the user. 
    By clicking on one of the TF sets, the set will be loaded as input TFs 
    into our 'signature mining' approach and the user can perform exploration.
    For each tool we provide a short descirption all the computational methods. 
    For a detailed descirption have a look at the original publication:
    "),
  p(tags$b( "CellNet"), "developed by Cahan et al. (2014), is a network biology-based framework 
    that evaluates the similarity between reprogrammed and target cell types while identifying 
    candidate transcription factors (TFs) to enhance reprogramming. However, its reliance on 
    expression data and mutual information may lead to inaccurate TF associations, and it lacks 
    the ability to optimize TF combinations by considering their interactions."),
  
  p("The approach, developed by", tags$b("Alessio et al."), "classfies TF as core TFs based 
    on type-specific expression patterns and high expression levels.
    Around 500 expression datasets, representing 106 cell and tissues types 
    primarily from the Human Body Index were used to train the model. 
    Being one of the earliest tools, the method is also the most simple one and 
    does not incorporate the the many different data moadalieties which are available today"),
  

  p(tags$b("Mogrify,"), "introduced by Rackham et al., integrates 
    gene expression data with regulatory network information. The method calculates 
    differential expression scores for each TF compared to an background set. 
    It then employs STRING and MARA databases to estimate TF influence, 
    considering protein-protein and protein-DNA interactions. For each TF, 
    it calculates the weighted sum of the scores of local target nodes, 
    taking into account path length. Candidate TFs are identified based on their 
    ranks in the start and target cell types, with additional filtering to select 
    TFs with common targets. An recently published extension, named Epimogrify, 
    is also capable to integrate epigenomic data into the model."),
  

  p("Wang et al. introduced",tags$b( "Taiji-reprogram,"), " an adaptation of Taiji, with a 
    focus on predicting cellular reprogramming protocols. This method harnesses 
    transcriptomic and epigenomic data to generate cell-type specific gene regulatory networks (GRNs). 
    To evaluate TF influence, Taiji-reprogram employs a personalized PageRank algorithm. 
    It also defines active regulatory regions by incorporating ATAC-seq, DNase, 
    or H3K27ac ChIP-seq peaks and links these regions to interacting genes using EpiTensor. 
    The method further scans these regions for TF binding sites using motifs from the CIS-BP database, 
    ultimately determining candidate TFs through a careful evaluation of their influence on cellular reprogramming."),
  
  p(tags$b( "IRENE"), "proposed by Jung et al. (2021), is an Integrative Gene Regulatory Network Model that improves TF 
    prediction for cellular reprogramming by incorporating epigenomic and protein-protein interaction (PPI) data. 
    Unlike previous methods, IRENE identifies cooperative and competitive TF interactions, 
    constructing a core gene regulatory network (GRN) and predicting optimal TF sets based on their 
    ability to activate regulatory elements, making it the first algorithm to explicitly infer co-factor TFs."),
  
  img(src="explore_comp_doc.png", style="max-width:100%; height:auto; display:block; margin:auto;"),
)