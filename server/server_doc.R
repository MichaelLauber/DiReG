activeButton <- reactiveVal(NULL)

# When actnBtnSignature is clicked, show the tabsetPanel
observeEvent(input$actnBtnSignature, {
  activeButton("actnBtnSignature")
  output$tabsetOutput <- renderUI({
    tabsetPanel(
      tabPanel("OR Analysis", source("doc/doc_ORA.R", local=T)),
      tabPanel("GSEA", source("doc/doc_GSEA.R", local=T)),
      tabPanel("GTex Tissue Expression", source("doc/doc_GTEx.R", local=T)),
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


mock_data <- data.frame(
  Donor_Cell = c("Cell Type A", "Cell Type B", "Cell Type C", "Cell Type D", "Cell Type E",
                 "Cell Type F", "Cell Type G", "Cell Type H", "Cell Type I", "Cell Type J",
                 "Cell Type A", "Cell Type B", "Cell Type C", "Cell Type D", "Cell Type E",
                 "Cell Type F", "Cell Type G", "Cell Type H", "Cell Type I", "Cell Type J"),
  Target_Cell = c("Cell Type X", "Cell Type Y", "Cell Type Z", "Cell Type W", "Cell Type V",
                  "Cell Type U", "Cell Type T", "Cell Type S", "Cell Type R", "Cell Type Q",
                  "Cell Type X", "Cell Type Y", "Cell Type Z", "Cell Type W", "Cell Type V",
                  "Cell Type U", "Cell Type T", "Cell Type S", "Cell Type R", "Cell Type Q"),
  Title = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5",
            "Study 6", "Study 7", "Study 8", "Study 9", "Study 10",
            "Study 11", "Study 12", "Study 13", "Study 14", "Study 15",
            "Study 16", "Study 17", "Study 18", "Study 19", "Study 20"),
  Journal = c("Journal A", "Journal B", "Journal C", "Journal D", "Journal E",
              "Journal F", "Journal G", "Journal H", "Journal I", "Journal J",
              "Journal A", "Journal B", "Journal C", "Journal D", "Journal E",
              "Journal F", "Journal G", "Journal H", "Journal I", "Journal J"),
  Year = c(2020, 2019, 2022, 2018, 2021,
           2017, 2016, 2015, 2023, 2014,
           2020, 2019, 2022, 2018, 2021,
           2017, 2016, 2015, 2023, 2014)
)

# Render the table within the specified div
output$mockTable <- renderDT({
  datatable(mock_data, options = list(dom = 't'))
})