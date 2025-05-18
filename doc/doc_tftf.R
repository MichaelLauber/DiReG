div(
  h2("TF TF Interaction"),
  p("To construct a comprehensive and up-to-date database of TF-TF interactions, 
  we utilized the latest available data from", a(href = "https://thebiogrid.org/", "BioGRID"),
    ",",
    a(href = "https://www.ebi.ac.uk/intact/", "IntAct"),
    ", and",
    a(href = "https://reactome.org/", "Reactome"), 
  "The dataset was filtered to retain only interactions classified under MI:0915 (physical association) and MI:0407 (direct interaction), 
  ensuring that only well-supported, high-confidence interactions were included.

  The curated TF-TF interaction network provides a resource for identifying additional TFs that interact with the hypothesized 
    input TF. By exploring these interactions, users can uncover potential regulatory relationships, 
    cooperative binding events."
  ),
  h3("Example Output"),
  img(src="tftf_doc.png")
  
)