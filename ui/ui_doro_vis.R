source(file.path("utils", "utils_popover.R"))

# titleWithPopover <- function(title, popover_title, popover_body) {
#   htmltools::span(
#     class = "popover-title",
#     title,
#     shiny::icon(
#       name = "circle-info",
#       style = "cursor: pointer;",
#       `data-bs-toggle` = "popover",
#       `data-bs-trigger` = "hover",
#       title = popover_title,
#       `data-bs-content` = popover_body
#     )
#   )
# }

fluidRow(
  column(12, div(
    shinyjs::hidden( div(id="expandButtonContainer", 
                         style = "margin: 20px auto; text-align: center; width: 50%;",
                         actionButton("expandButton", "Expand Network")) 
                     ),
    div(id="networkContainer", 
      conditionalPanel(
        condition = 'window.cond_visnet == "1"',
        div(h2("Explore a set of Transcription factors"),
            
            p(HTML("<strong>OR Analysis:</strong> Performs Overrepresentation Analysis based on the input TFs and their targets.")),
            p(HTML("<strong>GSEA:</strong> Performs Geneset Enrichment Analysis based on the input TFs and their targets. 
                    Analysis is performed on gene sets from (<a href='https://www.gsea-msigdb.org/gsea/msigdb' target='_blank'>MigSigDB.</a>); TFs, interaction with higher confidence, 
                    and genes more closely regulated by the input TF get a higher rank.")),
            p(HTML("<strong>GTEx Tissue Expression:</strong> Explore the expression of input TFs in different tissues based on (<a href='https://gtexportal.org/home/' target='_blank'>GTEx data</a>).")),
            
            p(HTML("<strong>TF Cofactors:</strong> Explore with your Input TFs interacting Cofactors  (<a href='https://academic.oup.com/nar/article/45/D1/D145/2333914' target='_blank'>Schemeier et al.</a>).")),
            
            p(HTML("<strong>TF TF Interactions:</strong> Find Interacting TFs based on PPI Data .")),
            
            
            p(HTML("<strong>Isoform Potential:</strong> Learn about the potential of the different isoforms of the input TFs to induce differentiations in hESC. 
                    Data is based on a publication from (<a href='https://www.cell.com/cell/pdf/S0092-8674(22)01470-2.pdf' target='_blank'>Joung et al.</a>) 
                    and measured via pseudotime (diffusion/RNA velocity) upon expression of isoform in hESC.")),
            p(HTML("<strong>TFA Analysis:</strong> Explore TF activities (TFA) in target and source cells. 
                    TFAs are calculated based on GTEx expression data and with decouplR, which calculates the consensus over multiple methods for TFA inference. 
                    To learn more about the method, visit <a href='https://saezlab.github.io/decoupleR/articles/decoupleR.html' target='_blank'>decoupleR documentation</a>.")),
            p(HTML("For more detailed explanations see <strong>Documentation</strong>"))
            
            )
        
      ),
    
      conditionalPanel(
        condition = 'window.cond_visnet == "0"',  
      visNetworkOutput("visNet_dorothea")
      ),
    
    hr(),
    fluidRow(column(12, hr())),
    
    #selection widgets for network
    
    fluidRow(
      column(3,
             checkboxGroupInput(
               "checkConfidence",
               titleWithPopover("Confidence Level", "Explanation", "A: Curated/high conficende; B: Likely confidence; C: Medium confidence; D: Low confidence"),
               choices = list(
                 "A" = "A",
                 "B" = "B",
                 "C" = "C",
                 "D" = "D"
               ),
               selected = "A",
               inline = T
             ),
      ),
      column(3,
             radioButtons(
               "radioOrgDorothea",
               "Organism",
               choices = list("human" = "human", "mouse" = "mouse"),
               selected = "mouse",
               inline = T
             ),
      ),
      column(3,
             sliderInput(
               "sliderDegDorothea",
               titleWithPopover("Radius", "Path length from input TF", "If you chose a radius > 1 indirect targets of the input TFs will also be calculated"),
               min = 1,
               max = 3,
               value = 1,
               ticks = T,
               width = "150px"
             ),
      ),
      column(3,uiOutput("tfFilter")),
      
    ),
    ),
    conditionalPanel(
      condition = "output.btnCreateDoroPressed",  # This checks the value of buttonPressed() in the output list
      column(2, offset = 10,
             downloadButton('btnDownloadDorothea', 'Download')
      )
    )

  )
  ))