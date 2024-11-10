
`%||%` <- function(x, y) if (is.null(x)) y else x

  # Initialize reactive values for caching
  cache <- reactiveValues(
    previousNodesOra = NULL,
    previousSources = NULL,
    enrichData = NULL
  )
  
  # UI element for selecting sources next to the plot
  output$sourceSelectionUI <- renderUI({
    div(style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; margin: 5px;",
    fluidRow(
      column(
        width = 8,
        checkboxGroupInput(
          inputId = "sourceSelectionInPlot",
          label = "Choose the data sources:",
          choices = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
          selected = cache$previousSources %||% c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
          inline = TRUE 
        )
      ),
      column(
        width = 4,
        # Add the "Update Plot" button next to the checkbox group
        div(style = "margin-top: 25px",  # Adjust the margin to align with the checkboxes
            actionButton("btnUpdatePlot", "Update Plot", icon = icon("refresh"))
        )
      )
    )
    )
  })
  
  # Function to perform ORA
  performORA <- function(selectedSources, currentNodes) {
    # Update cache with current selections
    cache$previousSources <- selectedSources
    cache$previousNodesOra <- currentNodes
    
    # Show loading indicator
    waiter::waiter_show()
    on.exit(waiter::waiter_hide(), add = TRUE)
    
    # Show notification
    notification <- showNotification(
      "Overrepresentation Analysis running. This can take a few seconds.",
      type = "message", duration = NULL, closeButton = TRUE
    )
    on.exit(removeNotification(notification), add = TRUE)
    
    # Determine organism
    organism <- ifelse(input$radioOrgDorothea == "human", "hsapiens", "mmusculus")
    
    # Perform the OR analysis using gprofiler2
    cache$enrichData <- gprofiler2::gost(
      query = currentNodes,
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
    
    # Render the enrichment plot
    output$enrichPlot <- renderPlotly({
      req(cache$enrichData)
      gprofiler2::gostplot(cache$enrichData, capped = TRUE, interactive = TRUE)
    })
    
    # Render the enrichment table
    output$tbl_enrich <- renderDT({
      req(cache$enrichData)
      datatable(
        cache$enrichData$result[, c(11, 3:6, 9, 10)],
        options = list(pageLength = 5)
      )
    })
  }
  
  # Function to show the source selection modal
  showSourceModal <- function() {
    showModal(modalDialog(
      title = "Select Sources for OR Analysis",
      checkboxGroupInput(
        inputId = "sourceSelection",
        label = "Choose the data sources:",
        choices = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
        selected = cache$previousSources %||% c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmSources", "OK")
      )
    ))
  }
  
  # Event when the ORA button is clicked
  observeEvent(input$btnORA, {
    if (!networkCreated) {  
      shinyalert::shinyalert(
        title = "No Genes to Analyze",
        text = "Please input genes and press the RUN button before performing any analysis.",
        type = "error"
      )
      return()
    }
    
    currentNodesOra <- nodes()$id  
    selectedSources <- cache$previousSources
    
    # Show the modal if inputs have not been set yet
    if (is.null(cache$previousNodesOra) || is.null(selectedSources)) {
      showSourceModal()
    } else {
      # If inputs have already been set, ensure the UI element is updated
      updateCheckboxGroupInput(
        session,
        inputId = "sourceSelectionInPlot",
        selected = selectedSources
      )
    }
  })
  
  # Run OR analysis when the user confirms their source selection
  observeEvent(input$confirmSources, {
    selectedSources <- input$sourceSelection
    currentNodesOra <- nodes()$id  # Ensure 'nodes()$id' is defined in your context
    
    removeModal()
    
    # Check if the analysis needs to be re-run
    if (identical(currentNodesOra, cache$previousNodesOra) &&
        identical(selectedSources, cache$previousSources)) {
      showNotification("No changes detected in selections. ORA not re-run.", type = "message")
      return()
    }
    
    performORA(selectedSources, currentNodesOra)
  })
  
  # Event when the user updates the selection using the UI element next to the plot
  observeEvent(input$btnUpdatePlot, {
    selectedSources <- input$sourceSelectionInPlot
    currentNodesOra <- nodes()$id  # Ensure 'nodes()$id' is defined in your context
    
    if (is.null(selectedSources) || length(selectedSources) == 0) {
      shinyalert::shinyalert(
        title = "No Sources Selected",
        text = "Please select at least one data source to perform ORA.",
        type = "warning"
      )
      return()
    }
    
    # Check if the analysis needs to be re-run
    if (identical(currentNodesOra, cache$previousNodesOra) &&
        identical(selectedSources, cache$previousSources)) {
      showNotification("No changes detected in selections. ORA not re-run.", type = "message")
      return()
    }
    
    performORA(selectedSources, currentNodesOra)
  })
  
  # Download handler for ORA results
  output$downloadDataORA <- downloadHandler(
    filename = function() {
      paste0("ORA-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(cache$enrichData)
      write.csv(cache$enrichData$result[, c(11, 3:6, 9, 10)], file, row.names = FALSE)
    }
  )


