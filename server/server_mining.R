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

  # output$tbl_inferred_protocols <- renderTable({
  #   getInferredProtocols(input$selectStart_infered, input$selectTarget_infered, input$checkGroupTools)
  # })
  output$dt_inferred_protocols <- renderDT({
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


observeEvent(input$dt_inferred_protocols_cell_clicked, {
  if(length(input$dt_inferred_protocols_cell_clicked) > 0){
    info <- input$dt_inferred_protocols_cell_clicked
    
    # change here if input table gets changed
    if(info$col == 4 ){
      
      displayed_inf_protocols <- getInferredProtocols(input$selectStart_infered, input$selectTarget_infered, input$checkGroupTools)
      
      #change if input get organism column
      
      # organism_of_TFset <- displayed_inf_protocols[info$row,] %>% 
      #   pull(Organism)
      # 
      # organism_of_TFset <- ifelse(organism_of_TFset == "human", "human", "mouse")
      # 
      # updateRadioButtons(session, "radioOrgDorothea",
      #                    selected = organism_of_TFset)
      
      
      clicked_TFs <-  stringr::str_split(info$value, "\\|") %>% unlist()
      clicked_TFs_string <- paste(clicked_TFs, collapse = " ")
      
      
      updateTextInput(session, "inputTextTFs", value = clicked_TFs_string)
      shinyjs::runjs('$("#btnCreateDoro").click();')
      updateTabsetPanel(session, "menu", "Signature Mining")
    }
  }
})


source("server/server_mining_network.R", local=T)
source("server/server_mining_ora.R", local=T)
source("utils/utils_enrichment.R")
source("server/server_mining_gsea.R", local=T)
source("server/server_mining_gtex.R", local=T)
source("server/server_mining_isoform.R", local=T)
source("server/server_mining_tfa.R", local=T)