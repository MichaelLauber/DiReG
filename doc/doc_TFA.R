div(

  h2("Transcription Factor Activity (TFA) analysis"),
  
  p("Transcription Factor Activity (TFA) quantifies how effectively a transcription factor regulates 
  its downstream target genes within a given cellular context. Hence, TFA might 
  serve as a valuable tool for assessing a transcription factor's 
  potential contribution to transdifferentiation. To learn more about computational approaches which facilitate TFA analysis, we suggest to 
    have a look at the review by",a("Hecker et al.", href="https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/10.1002/pmic.202200462")),
  
  p("In DiReG the TFA calculation is facilitated by the ", a("decoupleR package ", href="https://saezlab.github.io/decoupleR/articles/decoupleR.html") , 
  "for which we utilizes GTEx expression data (TPM) as counts and incorporates the Dorothea as underlying network. 
  For TFA calculation the 'Weighted Mean' method was chosen, a method represents a consensus score 
  across various methodologies known to exhibit superior performance based on the developer's", a( "benchmark.", href="https://pubmed.ncbi.nlm.nih.gov/36699385/")),
  
  p("The TFA analysis should identify TFs that exhibit strong TFA in the target cell while maintaining low TFA in the donor cell, making them promising candidates for 
  transdifferentiation protocols. In the resulting plot of the analysis, TFs situated above the diagonal line signify heightened activity in the target cell 
  compared to the source cell, indicating their potential relevance to the desired transdifferentiation process. 
  Additionally, TFs with p-values below 0.05 are highlighted in red, providing further insight into their statistical significance.
    Further, input TFs are distinctly marked as diamonds."),
  
  h3("Example Output"),
  img(src="tfa.png")


)

