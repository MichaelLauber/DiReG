all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))


observe({
  input$checkGroupTools
  updateSelectInput(session, "selectStart_infered",
                    choices = updateStartCellInferred(input$checkGroupTools))
})

#update target cell when startcell was selected
observe({
  startcell <- input$selectStart_infered
  
  protocols <- all_infered_protocols %>%
    filter(Method %in% input$checkGroupTools)
  
  updateSelectInput(session, "selectTarget_infered",
                    choices = getTargetCells(startcell, protocols))
})

#render table when start and targetcell where chosen
observe( {
  req(input$selectTarget_infered)

  output$dt_inferred_protocols <- renderDT({
    getInferredProtocols(input$selectStart_infered, input$selectTarget_infered, input$checkGroupTools)
  })
  
})

getInferredProtocols <- function(start, target, tools, protocol = all_infered_protocols){
  protocol %>%
    filter(Start == start & Target == target, Method %in% tools)
}



updateStartCellInferred <- function(tools){
  startCells <- all_infered_protocols %>%
    filter(Method %in% tools) %>%
    dplyr::select(Start) %>% 
    unique()
  
  createChoices(startCells)
}

observeEvent(input$dt_inferred_protocols_cell_clicked, {
  if(length(input$dt_inferred_protocols_cell_clicked) > 0){
    info <- input$dt_inferred_protocols_cell_clicked
    
    # change here if input table gets changed
    if(info$col == 4 ){
      
      displayed_inf_protocols <- getInferredProtocols(input$selectStart_infered, input$selectTarget_infered, input$checkGroupTools)
      
      clicked_TFs <-  stringr::str_split(info$value, "\\|") %>% unlist()
      clicked_TFs_string <- paste(clicked_TFs, collapse = " ")
      
      
      updateTextInput(session, "inputTextTFs", value = clicked_TFs_string)
      shinyjs::runjs('$("#btnCreateDoro").click();')
      updateTabsetPanel(session, "menu", "Signature Mining")
    }
  }
})
