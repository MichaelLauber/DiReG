div(
h2("OR Analysis - Overrepresentation Analysis"),

p("OR Analysis, short for Overrepresentation Analysis, displays the pathways and gene sets that may become 
  activated when overexpressing specific sets of transcription factors."),

p("Overrepresentation Analysis is a method employed to ascertain the presence 
  of predefined gene sets within a subset of 'interesting' genes. It aims to 
  identify gene sets that are more prevalent (over-represented) in this subset than 
  what would occur by chance. You can explore more about this method in",
  a(href = "https://academic.oup.com/nar/article/37/1/1/1026684", "Huang et al., 2009")),

p("Typically, this analysis involves conducting a statistical test 
  to assess the independence of a list of genes compared to a background set. 
  An enrichment p-value is calculated using methods like Fisher exact test, or Chi-square test.
  This approach is particularly suitable when there is no specific order within 
  the overrepresented genes."),

p("In our case, we focus on determining whether certain nodes are overrepresented w
  ithin a specific gene set when compared to all other genes within the organism. "),

p("For performing this calculation, we utilize the 'gost' function from the", 
  a(href = "https://biit.cs.ut.ee/gprofiler", "gprofiler package"), 
  "This function is configured with the following settings:"),
pre(
  "gprofiler2::gost(query = nodes,
                     organism = organism, ordered_query = FALSE,
                     multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
                     measure_underrepresentation = FALSE, evcodes = FALSE,
                     user_threshold = 0.05, correction_method = 'g_SCS',
                     domain_scope = 'annotated', custom_bg = NULL,
                     numeric_ns = '', sources = c('GO:MF', 'GO:CC', 'GO:BP', 'KEGG','REAC'), as_short_link = FALSE)"
),
p("Here, 'nodes' represent all nodes in the displayed Dorothea network which was created based on the input TFs. 
  GO stands for Geneontology and MF for Molecular function, Cellulular compartment, 
  BP for Biological function, while REAC, KEGG are the Reactome and KEGG databases."),
p("The final reault is a dotplot created with Plotly and a table of all results. 
  The user can either inspect the results by hovering through the plot or directly 
  search a specific geneset/pathway in the table."),
h3("Example output"),
  img(src = "ora.png", alt ="Image Description")
)