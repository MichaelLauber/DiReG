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


all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))

all_infered_protocols$Method %>% table()
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
