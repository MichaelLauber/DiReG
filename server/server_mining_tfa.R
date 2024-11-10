
if(!exists("gtexTPM")){
  gtexTPM <- readRDS(file.path("data","gtexTPM.rds"))
}
tissues <- colnames(gtexTPM)[-c(1:2)]

vals <- reactiveValues(data = NULL)


# Create reactive values to store previous state
prevState <- reactiveValues(
  celltype1 = NULL,
  celltype2 = NULL,
  data = NULL,
  res = NULL  # Store previously computed results
)


dataModal <- function(failed = FALSE) {
  modalDialog(
    selectizeInput("selectTissueTFAStart", h3("Start Tissue"),
                   choices = createChoices(tissues),
                   selected= "Cells - Cultured fibroblasts",
                   width = "230px"),
    
    selectizeInput("selectTissueTFATarget", h3("Target Tissue"),
                   choices = createChoices(tissues),
                   selected="Liver",
                   width = "230px"),
    
    span('Compare TF activities between two different tissues'),
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )}



observeEvent(input$selectTissueTFAStart, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
    updateSelectizeInput(session, "selectTissueTFATarget", choices = setdiff(createChoices(tissues), input$selectTissueTFAStart))
  }
})

observeEvent(input$selectTissueTFATarget, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
    updateSelectizeInput(session, "selectTissueTFAStart", choices = setdiff(createChoices(tissues), input$selectTissueTFATarget))
  }
})





observeEvent(input$btnTFA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  removeModal()
  
  showModal(dataModal())
  
})  

observeEvent(input$ok, {
  
  removeModal() 
  
  if (identical(input$selectTissueTFAStart, prevState$celltype1) &&
      identical(input$selectTissueTFATarget, prevState$celltype2) &&
      identical(data(), prevState$data)) {
    
    # If everything is the same, reuse the cached result
    res <- prevState$res
  } else {
  
  removeModal()
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Calcutlating TFAs"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  celltype1 <- input$selectTissueTFAStart #change to input$
  celltype2 <- input$selectTissueTFATarget #change to input$
  
  # Cache the current inputs
  prevState$celltype1 <- celltype1
  prevState$celltype2 <- celltype2
  prevState$data <- data()
  
  #for debugging
  # celltype1 <- "Bladder"
  # celltype2 <- "Liver" 
  
  counts <- gtexTPM %>%
    dplyr::filter(!duplicated(genes)) %>%
    dplyr::select(all_of(c(celltype1, celltype2))) %>%
    as.matrix()
  
  rownames(counts) <- gtexTPM %>% 
    dplyr::filter(!duplicated(genes)) %>% 
    dplyr::pull("genes")

  res <- decoupleR::decouple(mat=counts, #remove after debugging!
                             net=data(),
                             .source='from',
                             .target='to',
                             args = list(
                               # wsum = list(.mor = "mor"),
                               # ulm = list(.mor = "mor"),
                               mlm = list(.mor = "mor")
                             ),
                             minsize = 5) %>%
    dplyr::mutate(score = round(score, 2)) %>%
    dplyr::filter(statistic == "consensus") %>%
    dplyr::select(-run_id)
  
  prevState$res <- res

    output$tbl_tfa <- renderDT({
    res %>%
      dplyr::select(-statistic) %>% 
      DT::datatable() %>%
      formatSignif(columns = c('p_value'), digits = 3)
    
  })
  
  #enhancement: add column that indates if its input TF and another one that indicates if its downtream of input TF
  output$tfa_plot <-  renderPlotly({
    
    combined <- inner_join(
      res %>% dplyr::filter(statistic == "consensus") %>% split(.$condition) %>% `[[`(1),
      res %>% dplyr::filter(statistic == "consensus") %>% split(.$condition) %>% `[[`(2),
      by = c("source", "statistic")
    )
    
    combined$greaterPval <- 1 / pmax(combined$p_value.x, combined$p_value.y)
    combined$symbol <- ifelse(combined$source %in% inputTFs(), "diamond", "circle")
    combined$lineColor <- ifelse(combined$source %in% inputTFs(), "black", "white")
    
    max_val <- max(c(combined$score.x, combined$score.y))
    min_val <- min(c(combined$score.x, combined$score.y))
    
    plot_ly(combined,
            x = ~score.x, y = ~score.y,
            type = 'scatter',  
            mode = 'markers', 
            marker = list(size = 10, symbol = ~symbol, color = ~greaterPval,
                          line = list(color = ~lineColor, width = 2)),
            text = ~paste(source, "<br>", "p value in", condition.x, ": ", p_value.x,
                          "<br>", "p value in", condition.y, ": ", p_value.y)) %>%
      layout(shapes = list(
        list(type = "line", x0 = min_val, x1 = max_val, xref = "x",
             y0 = min_val, y1 = max_val, yref = "y",
             line = list(color = "black", width = 1, dash = "dash"))
      ),
      xaxis = list(title = paste0("TFA in ", unique(combined$condition.x))),
      yaxis = list(title = paste0("TFA in ", unique(combined$condition.y))),
      annotations = list(
        list(x = 0.05, y = 1, xref = "paper", yref = "paper",
             text = "Diamond: Input TF", showarrow = FALSE, align = "left"),
        list(x = 0.05, y = 0.95, xref = "paper", yref = "paper",
             text = "Color: p-value", showarrow = FALSE, align = "left")
      ))
    

    
  })
  }
})
