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
                  
                  
                  conditionalPanel('output.cond_ora == "1"',
                                   hr(),
                                   hr(),
                                   div(
                                     style = "margin-top: 30px; border-top: 1px solid #ccc; padding-top: 20px;",
                                     h3("Ask a LLM Question"),
                                     textInput("user_prompt_ora","Type your question:", placeholder = "Are these Genesets specfic for ... cells"),
                                     actionButton("submit_prompt_ora_btn", "Ask"),
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