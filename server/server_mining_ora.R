#OR analysis

previousNodes <- reactiveVal(NULL)

observeEvent(input$btnORA,{
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  currentNodes <- nodes()$id
  
  
  if (identical(currentNodes, previousNodes())) {
    
    return()
  } 
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Overrepresentation Analysis running. This can take a few seconds."), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  if(input$radioOrgDorothea == "human"){
    organism <- "hsapiens"
  } else {
    organism <- 'mmusculus'
  }
  
  enrichData <<-
    gprofiler2::gost(query = nodes()$id,
                     organism = organism, ordered_query = FALSE,
                     multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
                     measure_underrepresentation = FALSE, evcodes = FALSE,
                     user_threshold = 0.05, correction_method = "g_SCS",
                     domain_scope = "annotated", custom_bg = NULL,
                     numeric_ns = "", sources = c("GO:MF", "GO:CC", "GO:BP", "KEGG","REAC"), as_short_link = FALSE)
  
  
  
  output$enrichPlot <- renderPlotly({
    gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
  })
  
  output$tbl_enrich <- renderDT(
    enrichData$result[,c(11,3:6,9, 10)],
    options = list(pageLength = 5)
  )
  previousNodes(currentNodes)
})



output$downloadDataORA <- downloadHandler(
  filename = function() {
    paste("ORA-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(enrichData$result[,c(11,3:6,9, 10)], file)
  }
)