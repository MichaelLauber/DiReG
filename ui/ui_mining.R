tabPanel("Signature Mining",  
         div(id="container-mining-input", 
             textInput("inputTextTFs", 
                       "",
                       #value = "Enter Transcription Factors"
                       value = "HNF1A"
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
                                   h3('GTEx Analysis', align="center"),
                                   div(class="vw70", uiOutput("carousel"))
                  ),
                  conditionalPanel('output.cond_tfcof == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TF Cofactors', align="center"),
                                   div(class="vw70", uiOutput("carousel_tfcof"))
                  ),
                  conditionalPanel('output.cond_tftf == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TF-TF Interactions', align="center"),
                                   div(class="vw70", uiOutput("carousel_tftf"))
                  ),
                  conditionalPanel('output.cond_isoforms == "1"', 
                                   hr(),
                                   hr(),
                                   h3('Differentiation Potential TF Isoforms', align="center"),
                                   div(class="vw70", plotlyOutput("isoforms_plot")),
                                   ##source("ui/ui_isoforms.R")$value,
                  ),
                  
                  conditionalPanel('output.cond_tfa == "1"', 
                                   hr(),
                                   hr(),
                                   h3('TFA Analysis', align="center"),
                                   div(class="vw70",
                                       plotlyOutput("tfa_plot"),
                                       DTOutput("tbl_tfa")
                                   )
                  ),
                  
             )
             
         ),
         
         
)