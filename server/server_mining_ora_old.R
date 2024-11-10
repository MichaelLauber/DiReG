#OR analysis

previousNodesOra <- reactiveVal(NULL)

previousSources <- reactiveVal(NULL)

# UI element for selecting sources next to the plot
output$sourceSelectionUI <- renderUI({
  # Render the checkbox group input for source selection
  checkboxGroupInput(
    inputId = "sourceSelectionInPlot",
    label = "Choose the data sources:",
    choices = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
    selected = previousSources() %||% c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC") # Use previous sources or default
  )
})


showSourceModal <- function() {
  # Get the previous selection or use the default selection
  selectedSources <- previousSources()
  if (is.null(selectedSources)) {
    selectedSources <- c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC")
  }
  
  showModal(modalDialog(
    title = "Select Sources for OR Analysis",
    checkboxGroupInput(
      inputId = "sourceSelection",
      label = "Choose the data sources:",
      choices = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
      selected = selectedSources  # Use the previous selection or the default
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmSources", "OK")
    )
  ))
}

# observeEvent(input$btnORA,{
#   
#   if(!networkCreated){
#     shinyalert::shinyalert("There are no Genes to Analyse",
#                            "Please input genes and press the RUN button before performing any analysis",
#                            type = "error")
#     return()
#     }
#   
#   # if (!checkNetworkCreated()) {
#   #   return()
#   # }
#   showSourceModal()
# })
# 
# # Run OR analysis when the user confirms their source selection
# observeEvent(input$confirmSources, {
#   # Get the selected sources
#   selectedSources <- input$sourceSelection
#   
#   currentNodesOra <- nodes()$id
#   
#   removeModal()
#   
#   if (identical(currentNodesOra, previousNodesOra()) &  identical(selectedSources, previousSources()) ) {
#     return()
#   }
#   
#   previousSources(selectedSources)
#   
#   waiter <- waiter::Waiter$new()
#   waiter$show()
#   on.exit(waiter$hide())
#   
#   notification <- showNotification(
#     glue::glue("Overrepresentation Analysis running. This can take a few seconds."),
#     type = "message", duration = NULL, closeButton = TRUE
#   )
#   on.exit(removeNotification(notification), add = TRUE)
#   
#   organism <- ifelse(input$radioOrgDorothea == "human", "hsapiens", "mmusculus")
#   
#   # Perform the OR analysis using the selected sources
#   enrichData <<- gprofiler2::gost(
#     query = nodes()$id,
#     organism = organism,
#     ordered_query = FALSE,
#     multi_query = FALSE,
#     significant = TRUE,
#     exclude_iea = FALSE,
#     measure_underrepresentation = FALSE,
#     evcodes = FALSE,
#     user_threshold = 0.05,
#     correction_method = "g_SCS",
#     domain_scope = "annotated",
#     custom_bg = NULL,
#     numeric_ns = "",
#     sources = selectedSources,  # Use the selected sources here
#     as_short_link = FALSE
#   )
#   
#   output$enrichPlot <- renderPlotly({
#     gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
#   })
#   
#   output$tbl_enrich <- renderDT(
#     enrichData$result[, c(11, 3:6, 9, 10)],
#     options = list(pageLength = 5)
#   )
#   
#   previousNodesOra(currentNodesOra)
# })

# Event when the ORA button is clicked
observeEvent(input$btnORA, {
  if (!networkCreated) {
    shinyalert::shinyalert(
      "There are no Genes to Analyse",
      "Please input genes and press the RUN button before performing any analysis",
      type = "error"
    )
    return()
  }
  currentNodesOra <- nodes()$id
  selectedSources <- previousSources()
  
  # Show the modal only if inputs have not been set yet
  if (is.null(previousNodesOra()) || is.null(selectedSources)) {
    showSourceModal()
  } else {
    # If inputs have already been set, use the UI element for selection
    updateCheckboxGroupInput(
      session,
      inputId = "sourceSelectionInPlot",
      selected = selectedSources
    )
  }
})


observeEvent(input$confirmSources, {
  selectedSources <- input$sourceSelection
  currentNodesOra <- nodes()$id
  
  removeModal()
  
  if (identical(currentNodesOra, previousNodesOra()) && identical(selectedSources, previousSources())) {
    return()
  }
  
  previousSources(selectedSources)
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(
    glue::glue("Overrepresentation Analysis running. This can take a few seconds."),
    type = "message", duration = NULL, closeButton = TRUE
  )
  on.exit(removeNotification(notification), add = TRUE)
  
  organism <- ifelse(input$radioOrgDorothea == "human", "hsapiens", "mmusculus")
  
  # Perform the OR analysis using the selected sources
  enrichData <<- gprofiler2::gost(
    query = currentNodesOra,
    organism = organism,
    ordered_query = FALSE,
    multi_query = FALSE,
    significant = TRUE,
    exclude_iea = FALSE,
    measure_underrepresentation = FALSE,
    evcodes = FALSE,
    user_threshold = 0.05,
    correction_method = "g_SCS",
    domain_scope = "annotated",
    custom_bg = NULL,
    numeric_ns = "",
    sources = selectedSources,
    as_short_link = FALSE
  )
  
  output$enrichPlot <- renderPlotly({
    gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
  })
  
  output$tbl_enrich <- renderDT(
    enrichData$result[, c(11, 3:6, 9, 10)],
    options = list(pageLength = 5)
  )
  
  previousNodesOra(currentNodesOra)
})

# Event when the user updates the selection using the UI element next to the plot
observeEvent(input$sourceSelectionInPlot, {
  selectedSources <- input$sourceSelectionInPlot
  currentNodesOra <- nodes()$id
  
  if (identical(currentNodesOra, previousNodesOra()) && identical(selectedSources, previousSources())) {
    return()
  }
  
  previousSources(selectedSources)
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(
    glue::glue("Overrepresentation Analysis running. This can take a few seconds."),
    type = "message", duration = NULL, closeButton = TRUE
  )
  on.exit(removeNotification(notification), add = TRUE)
  
  organism <- ifelse(input$radioOrgDorothea == "human", "hsapiens", "mmusculus")
  
  # Perform the OR analysis using the selected sources
  enrichData <<- gprofiler2::gost(
    query = currentNodesOra,
    organism = organism,
    ordered_query = FALSE,
    multi_query = FALSE,
    significant = TRUE,
    exclude_iea = FALSE,
    measure_underrepresentation = FALSE,
    evcodes = FALSE,
    user_threshold = 0.05,
    correction_method = "g_SCS",
    domain_scope = "annotated",
    custom_bg = NULL,
    numeric_ns = "",
    sources = selectedSources,
    as_short_link = FALSE
  )
  
  output$enrichPlot <- renderPlotly({
    gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
  })
  
  output$tbl_enrich <- renderDT(
    enrichData$result[, c(11, 3:6, 9, 10)],
    options = list(pageLength = 5)
  )
  
  previousNodesOra(currentNodesOra)
})



output$downloadDataORA <- downloadHandler(
  filename = function() {
    paste("ORA-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(enrichData$result[,c(11, 3:6, 9, 10)], file, row.names = FALSE)
  }
)