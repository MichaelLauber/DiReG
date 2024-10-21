div(
  h2("Experimental"),
  
  p("Here we provide a manually curated list of transdifferentation protocols.
    The user can now easily search for a transdifferenataion protcols for a certain target and source cell type.
    Further, by clicking on one of the TF sets of the protocols, the set will be loaded as input TFs into our 'signature mining' 
    approach and the user can perform exploration of the TFs.
    The follwing table summarizes from which paper the protcols were derived. 
    "),
  DTOutput("mockTable"),
  br(),
  
  h2("Computational Approaches for TF Prediction"),
  
  p("Several computational approaches have been proposed to infer sets of TF for transdifferentiation.
    While these methods are still far away from delivering predictions with a high accurracy, 
    they can provide a starting point for validation of various sets in the laboratory.
    Here we gatherred prediction from some of the most popular tools in the field 
    and can be now easily browsed by the user. 
    Same as in the experimental section, by clicking on one of the TF sets, the set will be loaded as input TFs i
    nto our 'signature mining' approach and the user can perform exploration.
    For each tool we provide a short descirption all the computational methods. 
    For a detailed descirption have a look at the original publication:
    "),
  
  p("The approach, developed by", tags$b("Alessio et al."), "classfies TF as core TFs based 
    on type-specific expression patterns and high expression levels.
    Around 500 expression datasets, representing 106 cell and tissues types 
    primarily from the Human Body Index were used to train the model. 
    Being one of the earliest tools, the method is also the most simple one and 
    does not incorporate the the many different data moadalieties which are available today"),
  

  p(tags$b("Mogrify"), ", introduced by Rackham et al., integrates 
    gene expression data with regulatory network information. The method calculates 
    differential expression scores for each TF compared to an background set. 
    It then employs STRING and MARA databases to estimate TF influence, 
    considering protein-protein and protein-DNA interactions. For each TF, 
    it calculates the weighted sum of the scores of local target nodes, 
    taking into account path length. Candidate TFs are identified based on their 
    ranks in the start and target cell types, with additional filtering to select 
    TFs with common targets. An recently published extension, named Epimogrify, 
    is also capable to integrate epigenomic data into the model."),
  

  p("Wang et al. introduced",tags$b( "Taiji-reprogram"), ", an adaptation of Taiji, with a 
    focus on predicting cellular reprogramming protocols. This method harnesses 
    transcriptomic and epigenomic data to generate cell-type specific gene regulatory networks (GRNs). 
    To evaluate TF influence, Taiji-reprogram employs a personalized PageRank algorithm. 
    It also defines active regulatory regions by incorporating ATAC-seq, DNase, 
    or H3K27ac ChIP-seq peaks and links these regions to interacting genes using EpiTensor. 
    The method further scans these regions for TF binding sites using motifs from the CIS-BP database, 
    ultimately determining candidate TFs through a careful evaluation of their influence on cellular reprogramming."),
  
  p(tags$b( "CellNet"))
  
)