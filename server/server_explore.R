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


### server_explore 

example_question <- "How can I differentiate Pancreatic duct cells into beta cells? Which transcription factors are necessary?"

# Load example question when the Example button is pressed
observeEvent(input$explore_example_btn, { 
  print("Example Pressed")
  updateTextAreaInput(session, "user_prompt_explore", value = example_question)
})

observeEvent(input$explore_prompt_btn, {
  
  if(!key_uploaded()){
    showModal(modalDialog("Please Upload an API Key [For test purposes an API Key is provided by us]", easyClose = TRUE))
  }
  
 
  # Get the user question from the text area input
  user_question <- input$user_prompt_explore

  
  # Only proceed if user question is not empty
  if (nzchar(user_question)) {
    
    shinyjs::runjs("$('#api_response_output').text('Generating response, please wait...');")
    
    url <- "http://localhost:5555/query"
    
    payload <- list(
      question = user_question,
      temperature = input$explore_temp,
      rate_limit = "30000 per 1 minute",
      folder = "/app/papers",  # adjust if you mounted the folder or copied it in Docker
      mode = input$explore_mode,
      llm = api_settings()$preferred_model,
      summary_llm = api_settings()$preferred_model,
      agent_llm = api_settings()$preferred_model,
      max_answer_attempts = 3,
      api_key = api_settings()$api_key
    )
    
    
    # Send POST request to the API
    response <- httr::POST(url, body = payload, encode = "json")
    result <- httr::content(res, "parsed")
    
    # Parse the response and update the output text area
    if (response$status_code == 200) {
      output$api_response_output <- renderText({ result$formatted_answer })
    } else {
      error_message <- paste("An error occurred. Status code:", response$status_code)
      print(error_message)
      output$api_response_output <- renderText({ error_message })
    }
  } else {
    # If the input is empty, prompt the user to enter a question
    showModal(modalDialog("Please enter a question.", easyClose = TRUE))
  }
})