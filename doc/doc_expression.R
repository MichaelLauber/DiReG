div(
  h2("Tissue Expression"),
  p(
    "The",
    a(href = "https://tabula-sapiens.sf.czbiohub.org/", "Tabula Sapiens"),
    "and",
    a(href = "https://tabula-muris-senis.sf.czbiohub.org/", "Tabula Muris Senis"),
    "datasets provide a high-resolution view of gene expression across multiple tissues and organs at the single-cell level. 
    Tabula Sapiens is a human cell atlas based on over 1.1 million single cells collected from 28 organs of 24 healthy human donors, offering a unique perspective on cellular diversity and gene expression patterns in human tissues. 
    In contrast, Tabula Muris Senis is a single-cell transcriptomic atlas of",
    em("Mus musculus"),
    "spanning the entire life span and covering 23 tissues and organs, making it a valuable resource for studying age-related changes in gene expression. 
    
    Unlike bulk RNA-sequencing datasets, these single-cell transcriptomic datasets allow for the identification of cell-type-specific expression patterns, enabling a more granular understanding of gene activity in different biological contexts. 
    Both datasets were filtered to include only cell types that contain at least 50 cells in the selected tissues."
  ),

  p(
    "Based on the expression data the user is able to compare the expression of TF candiadates 
    across various cell types and see how specific and variable expression levels are.
    Ideally, good candidates should exhibit a high gene expression count in their target cell type 
    This indicates that the gene of interest is active and likely plays a significant 
    role in the desired cellular context.
    Conversely, a low gene expression count in the source cell is preferred. It suggests 
    that the TF's activity in the starting cell type is limited and overexpression of the TF
    should make the proctocol more likely to facilitate the desired transdifferentiation process."
  ),
  h3("Example Output"),
  img(src="expression_doc.png", style="max-width:100%; height:auto; display:block; margin:auto;")
  
)