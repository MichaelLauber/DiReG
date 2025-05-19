activeButton <- reactiveVal(NULL)

# When actnBtnSignature is clicked, show the tabsetPanel
observeEvent(input$actnBtnSignature, {
  activeButton("actnBtnSignature")
  
  output$sectionContent <- renderUI({ NULL })
  
  output$tabsetOutput <- renderUI({
    tabsetPanel(
      tabPanel("OR Analysis", source("doc/doc_ORA.R", local=T)),
      tabPanel("GSEA", source("doc/doc_GSEA.R", local=T)),
      tabPanel("Tissue Expression", source("doc/doc_expression.R", local=T)),
      tabPanel("TF Cofactors", source("doc/doc_tcof.R", local=T)),
      tabPanel("TT-TF Interaction", source("doc/doc_tftf.R", local=T)),
      tabPanel("Isoform Potential", source("doc/doc_isoform.R", local=T)),
      tabPanel("TFA Analysis", source("doc/doc_TFA.R", local=T))
    )
  })
  
})

# When actnBtnExplore is clicked, update uiOutput with different text
observeEvent(input$actnBtnExplore, {
  activeButton("actnBtnExplore")
  output$sectionContent <- renderUI({
    source("doc/doc_explore.R", local=T)
  })
  output$tabsetOutput <- NULL
})

# When actnBtnDiscovery is clicked, update uiOutput with different text
observeEvent(input$actnBtnDiscovery, {
  activeButton("actnBtnDiscovery")
  output$sectionContent <- renderUI({
    source("doc/doc_discovery.R", local=T)
    # You can replace this with the content you want to display
  })
  output$tabsetOutput <- NULL
})



# Render the table within the specified div
output$mockTable <- renderDT({
  datatable(mock_data, options = list(dom = 't'))
})