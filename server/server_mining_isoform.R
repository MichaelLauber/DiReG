
observeEvent(input$btnIsoforms, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  if(input$radioOrgDorothea == "mouse"){
    shinyalert::shinyalert("Not Supported for Mouse data",
                           "We are sorry, but the Differention Potential calculations are based on human data only",
                           type = "error")
  }
  
  if(!exists("isoformTFs_pseudotimes")){
    isoformTFs_pseudotimes <- vroom::vroom(file.path("data", "isoformTFs_pseudotimes.tsv"))
  }
  
  TFs <- inputTFs()
  
  output$isoforms_plot <- renderPlotly({
    
    # change here
    
    subset <- isoformTFs_pseudotimes %>%
      filter(TFName %in% TFs)
    
    nrGroups <- length(unique(subset$TFName))
    repsGroup <- as.numeric(table(subset$TFName))
    
    traces1 <- seq(1,nrGroups*2,by=2)
    traces2 <- seq(2,nrGroups*2,by=2)
    
    subset %>%
      group_by(TFName) %>%
      do(p=plot_ly(., x = ~IsoForm, y = ~diffusionZscore,  type = "bar", name = 'Diffusion', showlegend = F,hovertext = ~ENST) %>%
           add_trace(y = ~velocityZscore, name = 'Velocity', showlegend = F) #%>% layout(showlegend = FALSE)
      ) %>%
      subplot(nrows = 1, shareX = TRUE, shareY = TRUE, titleX = FALSE) %>%
      style(marker = list(color = c("#4b4896")), traces = traces1, name = 'Diffusion') %>%
      style(marker = list(color = c("#fc7f03")), traces = traces2, name = 'Velocity') %>%
      style(traces = c(1,2), showlegend=TRUE) %>%
      layout(
        title =  list(text = '<b>Differention Potential</b>'),
        yaxis = list(
          title = "Potential Z-Score"
        ),
        margin = list(l = 50, r = 50,
                      b = 50, t = 50,
                      pad = 20)
      )
    
  })
})
