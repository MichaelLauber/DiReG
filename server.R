#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 
  
# Define server logic required to draw a histogram
function(input, output, session) {
  
  cond_visnet <- reactiveVal(0)
  cond_visnet(1)
  
   observe({
   shinyjs::runjs(sprintf('window.cond_visnet = "%s"', cond_visnet())) # set to 0 in eventReactive(input$btnCreateDoro...
   })
  
  
  ####### mining  
   
  #loads example TFs in the input field within Signature mining
  observeEvent(input$btnMiningExample, {
    value <- "HNF1A HNF4A ONECUT1 ATF5 PROX1 CEBPA"
    updateTextInput(session, "inputTextTFs", value=value)
  })
  
  
  #All  performed analysis are hidden
  hideAll <- function(){
    cond_ora(0)
    cond_gsea(0)
    cond_gtex(0)
    cond_isoforms(0)
    cond_tfa(0)
  }
  
  
  resetBtns <- function(){
    btn_ids <- c("btnORA", "btnGSEA", "btnGTEx", "btnIsoforms", "btnTFA")
    for (btn_id in btn_ids) {
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-success')", btn_id))
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-primary')", btn_id))
    }
  }
  
  handleButtonClick <- function(buttonId, conditionSetter) {
    observeEvent(input[[buttonId]], {
      if (!networkCreated) {
        return()
      }

      hideAll()
      resetBtns()
      conditionSetter(1)
      print("Value:")
      print(conditionSetter())
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-primary')", buttonId))
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-success')", buttonId))
    })
  }


  cond_ora <- reactiveVal(0)
  cond_gsea <- reactiveVal(0)
  cond_gtex <- reactiveVal(0)
  cond_isoforms <- reactiveVal(0)
  cond_tfa <- reactiveVal(0)
  # 
  handleButtonClick("btnORA", cond_ora)
  handleButtonClick("btnGSEA", cond_gsea)
  handleButtonClick("btnGTEx", cond_gtex)
  handleButtonClick("btnIsoforms", cond_isoforms)
  handleButtonClick("btnTFA", cond_tfa)
  
  
  output$cond_ora = renderText({cond_ora()})
  output$cond_gsea = renderText({cond_gsea()})
  output$cond_gtex = renderText({cond_gtex()})
  output$cond_isoforms = renderText({cond_isoforms()})
  output$cond_tfa = renderText({cond_tfa()})

  observeEvent(input$ok, {
    cond_tfa(1)
  })
  
  outputOptions(output, 'cond_ora', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_gsea', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_gtex', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_isoforms', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_tfa', suspendWhenHidden=FALSE)

  #### End Mining
  
  
  ##Explore
  
  reprogramming_protocols <-
     read.csv(file.path("data","reprogramming_protocols.csv"))
  
  cond_exploreExp <- reactiveVal(1)
  cond_exploreComp <- reactiveVal(0)
  
  output$cond_exploreExp = renderText({cond_exploreExp()})
  output$cond_exploreComp = renderText({cond_exploreComp()})
  
  outputOptions(output, 'cond_exploreExp', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_exploreComp', suspendWhenHidden=FALSE)
  
     
  observeEvent(input$radioExploreTyp, 
               if(input$radioExploreTyp == "Experimental"){
                 cond_exploreExp(1)
                 cond_exploreComp(0)
               } else {
                 cond_exploreExp(0)
                 cond_exploreComp(1)
               }
               
               ) 
      
 
   
   
  source("server/server_explore.R", local=T)
  source("server/server_mining.R", local=T)
  source("server/server_discovery.R", local=T)     
  source("server/server_doc.R", local=T)
   
  
  # Documentation
  
  # activeButton <- reactiveVal(NULL)
  # 
  # # When actnBtnSignature is clicked, show the tabsetPanel
  # observeEvent(input$actnBtnSignature, {
  #   activeButton("actnBtnSignature")
  #   output$tabsetOutput <- renderUI({
  #     tabsetPanel(
  #       tabPanel("OR Analysis", source("doc/doc_ORA.R", local=T)),
  #       tabPanel("GSEA", source("doc/doc_GSEA.R", local=T)),
  #       tabPanel("GTex Tissue Expression", source("doc/doc_GTEx.R", local=T)),
  #       tabPanel("Isoform Potential", source("doc/doc_isoform.R", local=T)),
  #       tabPanel("TFA Analysis", source("doc/doc_TFA.R", local=T))
  #     )
  #   })
  # })
  #  
  # # When actnBtnExplore is clicked, update uiOutput with different text
  # observeEvent(input$actnBtnExplore, {
  #   activeButton("actnBtnExplore")
  #   output$sectionContent <- renderUI({
  #     source("doc/doc_explore.R", local=T)
  #   })
  #   output$tabsetOutput <- NULL
  # })
  #  
  # # When actnBtnDiscovery is clicked, update uiOutput with different text
  # observeEvent(input$actnBtnDiscovery, {
  #   activeButton("actnBtnDiscovery")
  #   output$sectionContent <- renderUI({
  #     source("doc/doc_discovery.R", local=T)
  #     # You can replace this with the content you want to display
  #   })
  #   output$tabsetOutput <- NULL
  # })
  
  
  # mock_data <- data.frame(
  #   Donor_Cell = c("Cell Type A", "Cell Type B", "Cell Type C", "Cell Type D", "Cell Type E",
  #                  "Cell Type F", "Cell Type G", "Cell Type H", "Cell Type I", "Cell Type J",
  #                  "Cell Type A", "Cell Type B", "Cell Type C", "Cell Type D", "Cell Type E",
  #                  "Cell Type F", "Cell Type G", "Cell Type H", "Cell Type I", "Cell Type J"),
  #   Target_Cell = c("Cell Type X", "Cell Type Y", "Cell Type Z", "Cell Type W", "Cell Type V",
  #                   "Cell Type U", "Cell Type T", "Cell Type S", "Cell Type R", "Cell Type Q",
  #                   "Cell Type X", "Cell Type Y", "Cell Type Z", "Cell Type W", "Cell Type V",
  #                   "Cell Type U", "Cell Type T", "Cell Type S", "Cell Type R", "Cell Type Q"),
  #   Title = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5",
  #             "Study 6", "Study 7", "Study 8", "Study 9", "Study 10",
  #             "Study 11", "Study 12", "Study 13", "Study 14", "Study 15",
  #             "Study 16", "Study 17", "Study 18", "Study 19", "Study 20"),
  #   Journal = c("Journal A", "Journal B", "Journal C", "Journal D", "Journal E",
  #               "Journal F", "Journal G", "Journal H", "Journal I", "Journal J",
  #               "Journal A", "Journal B", "Journal C", "Journal D", "Journal E",
  #               "Journal F", "Journal G", "Journal H", "Journal I", "Journal J"),
  #   Year = c(2020, 2019, 2022, 2018, 2021,
  #            2017, 2016, 2015, 2023, 2014,
  #            2020, 2019, 2022, 2018, 2021,
  #            2017, 2016, 2015, 2023, 2014)
  # )
  # 
  # # Render the table within the specified div
  # output$mockTable <- renderDT({
  #   datatable(mock_data, options = list(dom = 't'))
  # })


                
} 
  

