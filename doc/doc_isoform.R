div(
  
  h2("Isoform Potential - Exploring Transcription Factor Splice Isoforms"),
  
  p("While the human genome possess approximately 1800 transcription factor (TF) genes, 
    it gives rise to over 3500 distinct isoforms.
    These isoforms arise from alternative splicing events and have the potential to significantly impact 
    various aspects of TF functionality.
    Tissue and cell state specific differential splicing of the TF can result in variations in DNA binding specificity, 
    affinity, production of activators/repressor functions, and modulation of 
    dimerization properties.Thus, understanding the isoform potential of TFs can 
    provide crucial insights into their role in cellular differentiation."),
  
  p("Current transdifferentiation protocols often treat each TF as a single entity, 
    overlooking the intricate landscape of TF splice isoforms. 
    Recently, Joung et al. presented an extensive library of open reading frame (ORF)
    constructs encompassing all 3,548 known human TF splice isoforms. 
    Their groundbreaking work, detailed in", 
    a(href = "https://www.cell.com/cell/pdf/S0092-8674(22)01470-2.pdf", "Joung et al., 2022"), 
    ", represents a pivotal resource for investigating TF isoform function.
    In their study, the authors transduced hESCs with their library of TF isoforms and 
    and perfromed single cell RNA sequencing on these cells.Then two compzutational approaches
    diffusion and RNA velocity, weree used to order TF-overexpressing cells along a 
    pseudotime trajectory, where the pseudotime serves as a measurement of 
    cellular differentiation progress."),
  
  p("We allow the user to display these result for all TF isoforms of their input TFs. 
    This is especially useful for researchers who plan to use hESC for differentiation experiment.
    They might consider expressing a certain TF isoform instead of the whole TF gene if there are 
    significant differences between the splice variants. Furthermore, TF which a negative differentation
    potential might not be could candiates to start with. Also we think that for users who have 
    a start cell other than hESC, might benefit from exploring the data. TFs withs with great variation 
    within its isoforms might make the isofrorms worth to be tested in seperate experiments."),
  
  h3("Example Output"),
  img(src="isoform.png")
  
  
)