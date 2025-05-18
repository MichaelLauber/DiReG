div(
  h2("GSEA - Gene Set Enrichment Analysis"),
  
  p("Gene Set Enrichment Analysis (GSEA) is an analytical approach used to 
    determine whether a particular gene set is overrepresented within a given list 
    of genes. Unlike Overrepresentation Analysis (ORA), GSEA operates on a 
    rank-ordered list of genes, which allows it to incorporate the directionality 
    of gene expression changes."),
  
  p("One notable advantage of GSEA is that it does not require the arbitrary s
    election of a p-value cutoff. This makes GSEA particularly well-suited for 
    analyzing differential gene expression between two conditions.To delve 
    deeper into the fundamental principles of GSEA, 
    you can explore the seminal work of", 
    a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1239896/", "Subramanian et al., 2005")),
  
  p("In our specific context, we generate a rank-ordered list based on the 
    Dorothea network. Genes with shorter paths receive higher ranks, 
    while the rank size increases with the number of downstream targets and 
    the confidence level of the interaction. Additionally, the directionality 
    of the mode of regulation is incorporated into the ranking process.
    For the actual calculation of GSEA, we utilize the", 
    a(href = "https://bioconductor.org/packages/fgsea", "fgsea package"), 
    "This package is employed in conjunction with the", 
    a(href = "https://www.gsea-msigdb.org/gsea/msigdb", "Molecular Signatures Database (MSigDB)"),
    "collections 'Hallmark Gene Sets,' 'Canonical Pathways Gene Sets,' 
    'Gene Ontology Gene Sets,' and 'Cell Type Signature Gene Sets' for either humans or mice."),
  p("As in ORA, to further validate the result, if users log in and upload their OpenAI API key, 
  they can easily interact with ChatGPT to analyze their final gene sets and determine 
  whether they exhibit specificity for a particular cell type."),
  
  h3("Example Output"),
  img(src="gsea.png", alt="example gsea figure"),
  p("The plot shows the top 10 pathways based on their Normalized Enrichment Score (NES).
    A positive NES indicates that the gene set is overepresented in our data,
    and might be activated with the selection of our input TF. Vice versa a negatvie NES might supress indicates
    that this gene set or pathway might be surpressed by our input TFS.
    While the color code of the dots shows the adjusted p value of the result, the size of the dot correlates 
    with the number of genes that intersect with the the particular gene set and the genes in our network")
)