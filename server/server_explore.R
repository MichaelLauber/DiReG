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
  output$tbl_protocols <- renderTable({
    getProtocols(input$selectStart, input$selectTarget, input$radioExplore)
  })
})

getProtocols <- function(start, target, organism="both", protocol = reprogramming_protocols){

  if(organism == "both"){
    protocol %>%
      filter(Start == start & Target == target) %>%
      select(Organism, Protocol, Ref)
  } else {
    protocol %>%
      filter(Start == start & Target == target & Organism %in% organism) %>%
      select(Organism, Protocol, Ref)
  }
}