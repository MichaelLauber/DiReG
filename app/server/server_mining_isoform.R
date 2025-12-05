
observeEvent(input$btnIsoforms, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}

  # if (!checkNetworkCreated()) {
  #   return()
  # }
  
  if(input$radioOrgNetwork == "mouse"){
    shinyalert::shinyalert("Not Supported for Mouse data",
                           "We are sorry, but the Differention Potential calculations are based on human data only",
                           type = "error")
    return()
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

observeEvent(input$submit_prompt_iso_btn, {
  
  # 1. Check API Key
  if(!key_uploaded()){
    if(is.null(api_settings()$api_key) || api_settings()$api_key == "") {
      showModal(modalDialog("Please Upload an API Key", easyClose = TRUE))
      return()
    }
  }
  
  # 2. Validation
  # We don't check for networkCreated or organism here strictly, 
  # because general isoform knowledge applies even if the specific plot data is missing.
  req(inputTFs())
  
  start_cell <- input$llm_start_cell_iso
  target_cell <- input$llm_target_cell_iso
  
  if(is.null(start_cell) || start_cell == "" || is.null(target_cell) || target_cell == "") {
    showNotification("Please define both Start and Target cells.", type = "warning")
    return()
  }
  
  # 3. Prepare Data Strings (Just the cocktail)
  current_cocktail <- paste(inputTFs(), collapse = ", ")
  
  # 4. Construct Prompt
  # We ask specifically about biological isoform differences (canonical vs alternative)
  user_prompt <- paste0(
    "I am a scientist interested in direct reprogramming. ",
    "I am converting **", start_cell, "** to **", target_cell, "**. ",
    "My current Transcription Factor cocktail is: **", current_cocktail, "**. ",
    "Many TFs exist as multiple splice isoforms with different functions (e.g. activators vs repressors). ",
    "reason if there might be isoforms that show more potential than others for my direct reprogramming protocol",
    "Return Answer with Markdown Formatting. Keep it concise."
  )
  
  # 5. API Call (Robust ORA Style)
  shinyjs::runjs("$('#llm_response_iso').text('Generating response, please wait...');")
  
  url <- "https://api.openai.com/v1/chat/completions"
  
  data <- jsonlite::toJSON(list(
    model = api_settings()$preferred_model,
    messages = list(
      list(role = "user", content = user_prompt)
    ),
    max_completion_tokens = 10000 
  ), auto_unbox = TRUE)
  
  response <- httr::POST(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", api_settings()$api_key),
      `Content-Type` = "application/json"
    ),
    body = data,
    encode = "json"
  )
  
  # 6. Render Response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed_content <- jsonlite::fromJSON(content)
    
    response_to_display <- parsed_content$choices$message$content
    
    # Robust fallback for content extraction
    if(is.null(response_to_display)) {
      response_to_display <- parsed_content$choices[[1]]$message$content
    }
    
    output$llm_response_iso <- renderUI({
      div(
        style = "background-color: #fff; padding: 15px; border: 1px solid #eee; border-radius: 5px; margin-top: 10px;",
        HTML(markdown::markdownToHTML(text = response_to_display, fragment.only = TRUE))
      )
    })
  } else {
    response_to_display <- paste0("Error: Unable to retrieve a response. Status code:", httr::status_code(response))
    output$llm_response_iso <- renderUI({
      HTML(markdown::markdownToHTML(text = response_to_display, fragment.only = TRUE))
    })
  }
})
