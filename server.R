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
  #example btns
  observeEvent(input$btnMiningExample, {
    value <- "HNF1A HNF4A ONECUT1 ATF5 PROX1 CEBPA"
    updateTextInput(session, "inputTextTFs", value=value)
  })
  
  
   
  hideAll <- function(){
    cond_ora(0)
    cond_gsea(0)
    cond_gtex(0)
    cond_isoforms(0)
    cond_tfa(0)
  }
  
  resetBtns <- function(){
    shinyjs::runjs('document.getElementById("btnORA").classList.remove("bttn-success")')
    shinyjs::runjs('document.getElementById("btnORA").classList.add("bttn-primary")')
    
    shinyjs::runjs('document.getElementById("btnGSEA").classList.remove("bttn-success")')
    shinyjs::runjs('document.getElementById("btnGSEA").classList.add("bttn-primary")')
    
    shinyjs::runjs('document.getElementById("btnGTEx").classList.remove("bttn-success")')
    shinyjs::runjs('document.getElementById("btnGTEx").classList.add("bttn-primary")')
    
    shinyjs::runjs('document.getElementById("btnIsoforms").classList.remove("bttn-success")')
    shinyjs::runjs('document.getElementById("btnIsoforms").classList.add("bttn-primary")')
    
    shinyjs::runjs('document.getElementById("btnTFA").classList.remove("bttn-success")')
    shinyjs::runjs('document.getElementById("btnTFA").classList.add("bttn-primary")')
  }
  
  #highLightBtn-function(id){}
   
  
  #ora
  cond_ora <- reactiveVal(0)
  
  observeEvent(input$btnORA,{
    
    if(!networkCreated){
      return()
    }
    
    hideAll()
    resetBtns()
    cond_ora(1)
    shinyjs::runjs('document.getElementById("btnORA").classList.remove("bttn-primary")')
    shinyjs::runjs('document.getElementById("btnORA").classList.add("bttn-success")')
    
    
    # print("ORA")
    # print(inputTFs())
    # print(is.null(inputTFs()))
    # 
    # waiter <- waiter::Waiter$new()
    # waiter$show()
    # on.exit(waiter$hide())
    # 
    # notification <- showNotification(glue::glue("Overrepresentation Analysis running. This can take a few seconds."), type = "message", duration = NULL, closeButton = TRUE)
    # on.exit(removeNotification(notification), add = TRUE)
    # 
    # if(input$radioOrgDorothea == "human"){
    #   organism <- "hsapiens"
    # } else {
    #   organism <- 'mmusculus'
    # }
    # 
    # enrichData <<-
    #   gprofiler2::gost(query = nodes()$id,
    #                    organism = organism, ordered_query = FALSE,
    #                    multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
    #                    measure_underrepresentation = FALSE, evcodes = FALSE,
    #                    user_threshold = 0.05, correction_method = "g_SCS",
    #                    domain_scope = "annotated", custom_bg = NULL,
    #                    numeric_ns = "", sources = c("GO:MF", "GO:CC", "GO:BP", "KEGG","REAC"), as_short_link = FALSE)
    # 
    # 
    # 
    # output$enrichPlot <- renderPlotly({
    #   gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
    # })
    # 
    # output$tbl_enrich <- renderDT(
    #   #data.table::as.data.table(enrichData$result[,c(11,3:6,9, 10)]),
    #   #DT::datatable(enrichData$result[,c(11,3:6,9, 10)]) #%>%
    #   enrichData$result[,c(11,3:6,9, 10)] #%>%
    #   #formatSignif(columns = c('p_value'), digits = 3),
    #   #mutate(p_value = round(p_value, 5))
    #   ,
    #   options = list(pageLength = 5)
    # )
    
  })
  output$cond_ora = renderText({cond_ora()})
  
  #gsea  
  cond_gsea <- reactiveVal(0)
  observeEvent(input$btnGSEA,{
    
    if(!networkCreated){
      return()
    }
    
    hideAll()
    resetBtns()
    shinyjs::runjs('document.getElementById("btnGSEA").classList.remove("bttn-primary")')
    shinyjs::runjs('document.getElementById("btnGSEA").classList.add("bttn-success")')
    cond_gsea(1)
  })
  output$cond_gsea = renderText({cond_gsea()})
  
  #gtex
  cond_gtex <- reactiveVal(0)
  observeEvent(input$btnGTEx,{
    
    if(!networkCreated){
      return()
    }
    
    hideAll()
    resetBtns()
    shinyjs::runjs('document.getElementById("btnGTEx").classList.remove("bttn-primary")')
    shinyjs::runjs('document.getElementById("btnGTEx").classList.add("bttn-success")')
    cond_gtex(1)
  })
  output$cond_gtex = renderText({cond_gtex()})
  
  
  ###
  

  
  
  
  observeEvent(input$ok, {
    cond_tfa(1)
  })
  


  
  
  ###
  
  
  #isoform
  cond_isoforms <- reactiveVal(0)
  observeEvent(input$btnIsoforms,{
    
    if(!networkCreated){
      return()
    }
    
    hideAll()
    resetBtns()
    shinyjs::runjs('document.getElementById("btnIsoforms").classList.remove("bttn-primary")')
    shinyjs::runjs('document.getElementById("btnIsoforms").classList.add("bttn-success")')
    cond_isoforms(1)
  })
  output$cond_isoforms = renderText({cond_isoforms()})
  
  
 
  
  #tfa
  cond_tfa <- reactiveVal(0)
  observeEvent(input$btnTFA,{
    
    if(!networkCreated){
      return()
    }
    
    hideAll()
    resetBtns()
    shinyjs::runjs('document.getElementById("btnTFA").classList.remove("bttn-primary")')
    shinyjs::runjs('document.getElementById("btnTFA").classList.add("bttn-success")')
    
    
    
    #cond_tfa(1)
  })
  output$cond_tfa = renderText({cond_tfa()})
  
  
  
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
  
  #observeEvent(input$radioExploreTyp, print(input$radioExploreTyp))
  
   
  source("server/server_explore.R", local=T)
  source("server/server_pred_tools.R", local=T)
  source("server/server_pred_ame.R", local=T)     
  source("server/server_test.R", local=T)     
                
} 
  

