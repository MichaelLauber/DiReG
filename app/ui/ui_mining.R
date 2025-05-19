tabPanel("Signature Mining",  
         div(id="container-mining-input", 
             div(
               style = "display: flex; align-items: center;",
               div(id = "inputTextTFs-container", 
               #textInput("inputTextTFs", "", placeholder = "Enter TFs")
               textInput("inputTextTFs", "", value = "HNF1A"),
               ),
               div(
                 style = "margin-left: 5px;",
                 titleWithPopover(
                   "",
                   "Supported Formats",
                   "Symbol and ENSG (can be mixed) separated by comma, semicolon or space"
                 )
               )
             ),
             
             actionButton("btnCreateDoro",
                          label = "RUN",
                          class= "bttn-minimal"
             ),
             actionButton("btnMiningExample",
                          label = "Load Example TFs",
                          class= "bttn-minimal"
             )
         ), 
         div(id="container-mining", 
                
             div(id="container-outer-sidebar",
                 source("ui/ui_sidebar.R")$value
             ),
             
             div( id="container-mining-main",
                  
                   
                  source("ui/ui_doro_vis.R")$value,
                  
                  # put conditional panels in a separate script !!!
                  conditionalPanel('output.cond_ora == "1"',
                                   hr(),
                                   hr(),
                                   div(
                                     style = "margin-top: 30px; border-top: 1px solid #ccc; padding-top: 20px; background-color: #f9f9f9; padding: 15px; border-radius: 8px;",
                                     div(
                                       style = "display: flex; align-items: center; gap: 10px;",  # Flexbox layout
                                       tags$label("Ask a LLM: Do the overrepresentated genesets show specifity for:", style = "margin: 0; white-space: nowrap; font-size: 20px;"),
                                       textInput("user_prompt_ora", label = NULL, placeholder = "liver/beta/.. cells", width = "100%"),
                                       actionButton("submit_prompt_ora_btn", "Ask")
                                     ),
                                       verbatimTextOutput("llm_response_ora")
                                   ),
                                   h3('Overrepresentation Analysis based on Dorothea Network', align="center"),
                                   div(
                                     plotlyOutput("enrichPlot"),
                                     uiOutput("sourceSelectionUI"),
                                     DTOutput("tbl_enrich"),
                                     downloadButton('downloadDataORA', 'Download Data')
                                   )),
                  
                  
                  conditionalPanel('output.cond_gsea == "1"', 
                                   hr(),
                                   hr(),
                                   div(
                                     style = "margin-top: 30px; border-top: 1px solid #ccc; padding-top: 20px; background-color: #f9f9f9; padding: 15px; border-radius: 8px;",
                                     div(
                                       style = "display: flex; align-items: center; gap: 10px;",  # Flexbox layout
                                       tags$label("Ask a LLM: Do the enriched genesets show specifity for:", style = "margin: 0; white-space: nowrap; font-size: 20px;"),
                                       textInput("user_prompt_gsea", label = NULL, placeholder = "liver/beta/.. cells", width = "100%"),
                                       actionButton("submit_prompt_gsea_btn", "Ask")
                                     ),
                                     verbatimTextOutput("llm_response_gsea")
                                   ),
                                   h3('GSEA Analysis', align="center"),
                                   div(class="vw70",
                                       #plotOutput("plot_gsea"),
                                       uiOutput("plot_gsea"),
                                       br(),
                                       #DTOutput("tbl_gsea"),
                                       downloadButton('downloadDataGSEA', 'Download Data')
                                   )
                  ),
                  
                  conditionalPanel('output.cond_gtex == "1"', 
                                   hr(),
                                   hr(),
                                   h3('Expression Comparision', align="center"),
                                   div(class="vw70", uiOutput("carousel"), actionButton("changeTissues", "Change Tissues"))
                  ),
                  conditionalPanel('output.cond_tfcof == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TF Cofactors', align="center"),
                                   div(class="vw70", uiOutput("carousel_tfcof")),
                                   p("Transcription factor cofactors (TcoFs) are proteins that, while not transcription factors themselves, physically interact with transcription factors to modulate gene expression. 
                                     These cofactors are characterized by evidence of involvement in transcriptional regulation and localization within the cell nucleus. 
                                     Based on the strength and type of evidence for these two key characteristics, TcoFs are classified into four distinct confidence categories.
                                     Clicking on a TcoF allows to explore its tissue expression.")
                  ),
                  conditionalPanel('output.cond_tftf == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TF-TF Interactions', align="center"),
                                   div(class="vw70", uiOutput("carousel_tftf")),
                                   p("TF-TF interactions are based on filtered data from BioGRID, IntAct, and Reactome, focusing exclusively on physical and direct interactions (MI:0915 and MI:0407) to show potential cooperative binding events. 
                                     Clicking on the TF allows you to explore the expression levels of the TF in chosen tissues and can give hints it might be a suitable target.")
                  ),
                  conditionalPanel('output.cond_isoforms == "1"', 
                                   hr(),
                                   hr(),
                                   h3('Differentiation Potential TF Isoforms', align="center"),
                                   div(class="vw70", plotlyOutput("isoforms_plot")),
                                   p("Transcription factors (TFs) exist in multiple isoforms due to alternative splicing, with approximately 1,800 TF genes producing over 3,500 distinct variants that can differ in DNA binding properties, activator/repressor functions, and dimerization capabilities. 
                                     The figure incorporates data from Joung et al. (2022), who created a comprehensive library of all human TF splice isoforms and measured their effects on cellular differentiation in hESCs through pseudotime trajectory analysis, 
                                     allowing users to identify specific isoforms that may be more effective for transdifferentiation experiments than using the canonical TF.")
                                   ##source("ui/ui_isoforms.R")$value,
                  ),
                  
                  conditionalPanel('output.cond_tfa == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TFA Analysis', align="center"),
                                   div(class="vw70",
                                       plotlyOutput("tfa_plot"),
                                       DTOutput("tbl_tfa"),
                                       br(),
                                       p("TFs with high activity in the target cell type but low activity in the starting cell type are ideal candidates to drive transdifferentiation via overexpression. 
                                         In contrast TFs with a high activity in the donor cell type , but low in the target cell type might be targets for repression of the donor cell programm.")
                                   )
                  ),
                  
             )
             
         ),
         
         
)