all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))

#update target cell when startcell was selected
observe({
  startcell <- input$selectStart_infered

  protocols <- all_infered_protocols %>%
    filter(Method %in% input$checkGroupTools)

  updateSelectInput(session, "selectTarget_infered",
                    choices = getTargetCells(startcell, protocols))
})

#update startcell when checkboxes were changed
observe({
  input$checkGroupTools
  updateSelectInput(session, "selectStart_infered",
                    choices = updateStartCellInferred(input$checkGroupTools))
})


#render table when start and targetcell where chosen
observe( {
  req(input$selectTarget_infered)

  output$tbl_inferred_protocols <- renderTable({
    getInferredProtocols(input$selectStart_infered, input$selectTarget_infered, input$checkGroupTools)
  })
})


updateStartCellInferred <- function(tools){
  startCells <- all_infered_protocols %>%
    filter(Method %in% tools) %>%
    select(Start) %>% 
    unique()
  
  createChoices(startCells)
}


getInferredProtocols <- function(start, target, tools, protocol = all_infered_protocols){
  protocol %>%
    filter(Start == start & Target == target, Method %in% tools)
}