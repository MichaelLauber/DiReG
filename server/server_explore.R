#updates input in explore tab
observe({
  startcell <- input$selectStart
  updateSelectInput(session, "selectTarget",
                    choices = getTargetCells(startcell, reprogramming_protocols))
})


# prints Table of known protocols on explore page
observeEvent(input$selectTarget, {
  req(input$selectTarget)
  shinyjs::show(id="dt_protocols")
  
  startcell <- input$selectStart
  targetcell <- input$selectTarget
  
  output$header_shown_protocols <- renderUI({
    tags$h4(HTML(paste(startcell, " -> ", targetcell)), align="center")
  })

  output$dt_validated_protocols <- renderDT({
    datatable(getProtocols(startcell, targetcell, input$radioExplore), selection = 'single', escape = FALSE)
  })
  
  output$info_validated_protocols <- renderText("click on a set of TFs for exploration through the Signature Mining procedure")
})


getProtocols <- function(start, target, organism="both", protocol = reprogramming_protocols){

  if(organism == "both"){
    dt <- protocol %>%
      filter(Start == start & Target == target) %>%
      select(Organism, Protocol, Ref)
  } else {
    dt <- protocol %>%
      filter(Start == start & Target == target & Organism %in% organism) %>%
      select(Organism, Protocol, Ref)
  }
  
  dt$Ref <- sapply(dt$Ref, function(link) {
    as.character(tags$a(href = link, target = "_blank", link))
  })
  
  dt
}

#updates the input TFs and organism within signature mining, executes btnCreateDoro and switches the tab
observeEvent(input$dt_validated_protocols_cell_clicked, {
  if(length(input$dt_validated_protocols_cell_clicked) > 0){
    info <- input$dt_validated_protocols_cell_clicked
    
    # change here if input table gets changed
    if(info$col ==2 ){
      
      displayed_protocols <- getProtocols(input$selectStart, input$selectTarget, input$radioExplore)
      
      organism_of_TFset <- displayed_protocols[info$row,] %>% 
        pull(Organism)
      
      organism_of_TFset <- ifelse(organism_of_TFset == "human", "human", "mouse")
      
      updateRadioButtons(session, "radioOrgDorothea",
                         selected = organism_of_TFset)
      
      
      clicked_TFs <-  stringr::str_split(info$value, "\\|") %>% unlist()
      clicked_TFs_string <- paste(clicked_TFs, collapse = " ")
      
      
      updateTextInput(session, "inputTextTFs", value = clicked_TFs_string)
      shinyjs::runjs('$("#btnCreateDoro").click();')
      updateTabsetPanel(session, "menu", "Signature Mining")
    }
  }
})