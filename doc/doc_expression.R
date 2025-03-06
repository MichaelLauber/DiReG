div(
  h2("Tissue Expression"),
  p(
    "The",
    a(href = "https://gtexportal.org/home", "Genotype-Tissue Expression (GTEx) database"),
    "is a publicly available resource that provides valuable insights into tissue-specific gene expression profiles.
     The database consists of samples were collected from 54 non-diseased tissue sites across nearly 1000 individuals,
     making the database a comprehensive representation of gene expression patterns across various tissues and individuals.
    In this analysis, we utilize data from the 'GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm' dataset."
  ),

  p(
    "Based on the GTEx expression data the user is able to compare the expression of TF candiadates 
    across various tissues and see how specific and variable expression levels are.
    Ideally, good candidates should exhibit a high gene expression count in their target tissue. 
    This indicates that the gene of interest is active and likely plays a significant 
    role in the desired cellular context.
    Conversely, a low gene expression count in the source cell is preferred. It suggests 
    that the TF's activity in the starting cell type is limited and overexpression of the TF
    should make the proctocol more likely to facilitate the desired transdifferentiation process."
  ),
  h3("Example Output"),
  img(src="gtex.png")
  
)