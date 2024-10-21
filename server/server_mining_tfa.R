
if(!exists("gtexTPM")){
  gtexTPM <- readRDS(file.path("data","gtexTPM.rds"))
}
tissues <- colnames(gtexTPM)[-c(1:2)]

vals <- reactiveValues(data = NULL)

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
  )
  
  
  
}


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
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Calcutlating TFAs"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  celltype1 <- input$selectTissueTFAStart #change to input$
  celltype2 <- input$selectTissueTFATarget #change to input$
  
  #for debugging
  # celltype1 <- "Bladder"
  # celltype2 <- "Liver" 
  
  dups <- duplicated(gtexTPM$genes)
  counts <- gtexTPM[!dups,] %>%
    select(celltype1, celltype2) %>%
    as.matrix()
  
  rownames(counts) <- gtexTPM[!dups,] %>% pull("genes")
  
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
    mutate(score = round(score, 2)) %>%
    filter(statistic == "consensus") %>%
    select(-run_id)
  
  
  output$tbl_tfa <- renderDT({
    
    res %>%
      DT::datatable() %>%
      formatSignif(columns = c('p_value'), digits = 3)
    
  })
  
  
  output$tfa_plot <-  renderPlotly({
    
    sample_acts <- res %>% filter(statistic == "consensus")
    splitted_by_tissue <- split(sample_acts, sample_acts$condition)
    combined <- inner_join(splitted_by_tissue[[1]], splitted_by_tissue[[2]], by=c("source", "statistic"))
    combined$greaterPval <- 1/(ifelse(combined$p_value.x < combined$p_value.y, combined$p_value.y, combined$p_value.x))
    combined$symbol <- ifelse(combined$source %in% inputTFs(),  'diamond', 'circle')
    combined$lineColor <- ifelse(combined$source %in% c("inputTFs()"),  'red', 'white')
    
    max <- max(c(combined$score.x, combined$score.y))
    min <- min(c(combined$score.x, combined$score.y))
    
    xtitle <- unique(combined$condition.x)
    ytitle <- unique(combined$condition.y)
    plot_ly(combined,
            x = ~score.x,
            y = ~score.y,
            marker = list(size = 10,
                          symbol = ~symbol,
                          color = ~greaterPval,
                          #color = 'rgba(255, 182, 193, .9)',
                          line = list(color = ~lineColor, width = 2)
            ),
            text = ~paste(combined$source,
                          "<br>",
                          "p value in", combined$condition.x,": ", combined$p_value.x,
                          '<br>',
                          "p value in", combined$condition.y,": ", combined$p_value.y
            )
    ) %>%
      layout(shapes =
               list(
                 list(
                   type = "line",
                   x0 = min,
                   x1 = max,
                   xref = "x",
                   y0 = min,
                   y1 = max,
                   yref = "y",
                   line = list(color = "black", width = 1, dash = 'dash')
                 )
               )
             ,
             xaxis = list(title = paste0("TFA in ", xtitle)),
             yaxis = list(title = paste0("TFA in ", ytitle)),
             annotations = list(
               list(x = 0.05, y = 1, xref = 'paper', yref = 'paper', text = 'Diamond: Input TF', showarrow = FALSE, align = 'left'),
               list(x = 0.05, y = 0.95, xref = 'paper', yref = 'paper', text = 'Color: p-value', showarrow = FALSE, align = 'left')
             )
      )
    
    
  })
  
})
