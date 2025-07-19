activeButton <- reactiveVal(NULL)

# When actnBtnSignature is clicked, show the tabsetPanel
observeEvent(input$actnBtnSignature, {
  activeButton("actnBtnSignature")
  
  output$sectionContent <- renderUI({ NULL })
  
  output$tabsetOutput <- renderUI({
    tabsetPanel(
      tabPanel("OR Analysis", {
        source("doc/doc_ORA.R", local=TRUE)$value
      }),
      tabPanel("GSEA", {
        source("doc/doc_GSEA.R", local=TRUE)$value
      }),
      tabPanel("Tissue Expression", {
        source("doc/doc_expression.R", local=TRUE)$value
      }),
      tabPanel("TF Cofactors", {
        source("doc/doc_tcof.R", local=TRUE)$value
      }),
      tabPanel("TT-TF Interaction", {
        source("doc/doc_tftf.R", local=TRUE)$value
      }),
      tabPanel("Isoform Potential", {
        source("doc/doc_isoform.R", local=TRUE)$value
      }),
      tabPanel("TFA Analysis", {
        source("doc/doc_TFA.R", local=TRUE)$value
      })
    )
  })
})

# When actnBtnExplore is clicked, update uiOutput with different text
observeEvent(input$actnBtnExplore, {
  activeButton("actnBtnExplore")
  output$sectionContent <- renderUI({
    source("doc/doc_explore.R", local=TRUE)$value
  })
  output$tabsetOutput <- NULL
})

# When actnBtnDiscovery is clicked, update uiOutput with different text
observeEvent(input$actnBtnDiscovery, {
  activeButton("actnBtnDiscovery")
  output$sectionContent <- renderUI({
    source("doc/doc_discovery.R", local=TRUE)$value
  })
  output$tabsetOutput <- NULL
})

output$showWelcome <- reactive({
  is.null(activeButton())
})
outputOptions(output, "showWelcome", suspendWhenHidden = FALSE)