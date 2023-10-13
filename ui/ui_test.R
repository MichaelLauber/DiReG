tabPanel(
  
  "Test",
  #row 1
  fluidRow(
    column(2, textInput("inputTextTFs", "",
                        value = "Enter TFs...")),
    column(1, div("_"), actionButton("checkBtn", "check")) 
  ),
    
  #row2
  fluidRow(
    column(6, div(
      visNetworkOutput("visNet_dorothea"),
      fluidRow(column(12, hr())),
        
      #selection widgets for network
      fluidRow(
        column(4,
               checkboxGroupInput(
                 "checkConfidence",
                 ("Confidence Level"),
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
        column(4,
               radioButtons(
                 "radioOrgDorothea",
                 "Organism",
                 choices = list("human" = "human", "mouse" = "mouse"),
                 selected = "human",
                 inline = T
               ),
        ),
        column(4,
               sliderInput(
                 "sliderDegDorothea",
                 "Radius",
                 min = 1,
                 max = 3,
                 value = 1,
                 ticks = T,
                 width = "150px"
               ),
        )
      ),
      
      # btns for further analysis
      fluidRow(
        column(2, actionButton("btnEnrichDorothea", "OR Enrichment")),
        column(2, actionButton("btnGSEA", "GSEA")),
        column(2, actionButton("btnIsoforms", "Diff Potential")),
        column(2, actionButton("btnGTEx", "GTEx Expression")),
        column(2, actionButton("btnTFA", "TF Activity Analysis")),
        column(2, downloadLink("btnDownloadDorothea", "Download"))
      )
      
    )
    ),
    #OR Analysis
    column(6, 
           div(
             plotlyOutput("enrichPlot"),
             DTOutput("tbl_enrich")
           ),
    ),
    
  ),
  
  #row3
  fluidRow(
    column(6,
           DTOutput("tbl_gsea")),
  ),
  column(6, 
         plotOutput("plot_gsea")
         
  ),
  
  fluidRow(
    column(6, plotlyOutput("isoforms_plot")),
    column(6, plotlyOutput("plot_gtex"))
  ),
  
  fluidRow(
    column(6, DTOutput("tbl_tfa")),
  ),
  
  
)