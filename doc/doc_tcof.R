div(
  h2("TF Cofactor Identification"),
  p("Transcription factor cofactors (Tcofs) are essential in direct reprogramming, 
    as they enhance the efficiency and specificity of converting somatic cells into desired cell types. 
    For instance, the transcriptional coactivator p300 interacts directly with MyoD, a master regulator of myogenesis, 
    facilitating the activation of muscle-specific genes.  Additionally, cofactors can recruit chromatin remodelers to alter chromatin accessibility, 
    making specific genomic regions more or less accessible to TFs. Also they might factilitatily Protein-Protein 
    interactions and influce the stability and localization of TFs."),
  p(
    "In 2016, Schmeier et al. developed the",
    a(href = "https://academic.oup.com/nar/article/45/D1/D145/2333914", "TcoF-DB v2"),
    "database, cataloging human and mouse transcription co-factors (TcoFs) and their interactions with transcription factors (TFs). 
    However, this resource has not been updated since its release. To address this, we have created an updated version using data available as of January 2025, adhering to the original criteria defined by Schmeier et al. for TcoF classification."
  ),
  p(
    "A gene is considered a TcoF if it satisfies all of the following conditions:",
    tags$ul(
      tags$li("It is not listed as a transcription factor."),
      tags$li("It has been experimentally shown to bind to a transcription factor, as referenced in the literature."),
      tags$li(
        "It is annotated in Gene Ontology with either:",
        tags$ul(
          tags$li("GO:0000988 (transcription factor activity, protein binding)"),
          tags$li("GO:1903506 (regulation of nucleic acid-templated transcription)"),
          tags$li("or one of their descendant terms.")
        )
      ),
      tags$li("It is annotated in Gene Ontology with GO:0005634 (nucleus) or one of its descendant terms.")
    )
  ),
  p(
    "Transcription co-factors are categorized into four confidence classes based on the type of evidence present in their Gene Ontology annotations:",
    tags$ul(
      tags$li(
        strong("High-confidence (HC):"),
        " TcoFs with experimental evidence for both involvement in transcriptional regulation and localization in the cell nucleus."
      ),
      tags$li(
        strong("Class 1:"),
        " TcoFs with experimental evidence for involvement in transcriptional regulation, but only non-experimental evidence (e.g., 'Inferred from Electronic Annotation') for nuclear localization."
      ),
      tags$li(
        strong("Class 2:"),
        " TcoFs with experimental evidence for nuclear localization, but only non-experimental evidence for involvement in transcriptional regulation."
      ),
      tags$li(
        strong("Class 3:"),
        " TcoFs with only non-experimental evidence for both involvement in transcriptional regulation and nuclear localization."
      )
    )
  ),
  p(
    "Interactions between TFs and TcoFs were sourced from databases such as",
    a(href = "https://thebiogrid.org/", "BioGRID"),
    ",",
    a(href = "https://www.ebi.ac.uk/intact/", "IntAct"),
    ", and",
    a(href = "https://reactome.org/", "Reactome"),
    ". We included only human-human and mouse-mouse interactions between protein-coding genes. Additionally, we filtered interactions based on specific PSI-MI identifiers to ensure relevance:",
    tags$ul(
      tags$li("MI:0914 – association"),
      tags$li("MI:0915 – physical association"),
      tags$li("MI:0407 – direct interaction"),
      tags$li("MI:0195 – covalent binding"),
      tags$li("MI:0408 – disulfide bond"),
      tags$li("MI:0556 – transglutamination reaction")
    )
  ),
  p(
    "Our updated version, significantly expanded the database. The updated human dataset now contains 1378 total TcoFs (from 958) for humans, 
    classified into 689 High-confidence (HC), 84 Class 1, 392 Class 2, and 213 Class 3 TcoFs. 
    Similarly, the updated mouse dataset includes 665 total TcoFs (from 418), with 264 High-confidence (HC), 67 Class 1, 140 Class 2, and 194 Class 3 TcoFs"
  ),
  p("By clicking on one of the Tcofs the user can also diretcly display their expression in different tissues and 
    see if the Tcof might be a candaidate for overexpression."),
  
  h3("Example Output"),
  img(src="tcof_doc.png")
  
)