function(input, output, session) {
  
  options(shiny.error = browser)   

   
  # Set your API key
  openai_key    <- reactiveVal(NULL)
  favorite_model <- reactiveVal(NULL)
  key_uploaded  <- reactiveVal(FALSE)
  
  api_settings <- reactive({
    if (key_uploaded()) {
      list(api_key = openai_key(), preferred_model = favorite_model())
    } else {
      list(
        api_key = readRDS("login/fllback_api_key.rds"),
        preferred_model = "gpt-4o-mini"
      )
    }
  })
  
  
  
  # Define the API endpoint
  url <- "https://api.openai.com/v1/chat/completions" 
  
  # ### server_explore 
  # 
  # example_question <- "How can I differentiate Pancreatic duct cells into beta cells? Which transcription factors are necessary?"
  # 
  # # Load example question when the Example button is pressed
  # observeEvent(input$explore_example_btn, { 
  #   print("Example Pressed")
  #   updateTextAreaInput(session, "user_prompt_explore", value = example_question)
  # })
  # 
  # observeEvent(input$explore_prompt_btn, {
  #   
  #   if(!key_uploaded()){
  #     showModal(modalDialog("Please Upload an API Key [For test purposes an API Key is provided by us]", easyClose = TRUE))
  #   }
  #   
  #   print("Ask Pressed")
  #   # Get the user question from the text area input
  #   user_question <- input$user_prompt_explore
  #   print("sending user question")
  #   
  #   shinyjs::runjs("$('#api_response_output').text('Generating response, please wait...');")
  #   
  #   # Only proceed if user question is not empty
  #   if (nzchar(user_question)) {
  #     
  #     url <- "http://localhost:5555/query"
  #     
  #     payload <- list(
  #       question = user_question,
  #       temperature = input$explore_temp,
  #       rate_limit = "30000 per 1 minute",
  #       folder = "/.",  # adjust if you mounted the folder or copied it in Docker
  #       mode = input$explore_mode,
  #       llm = api_settings()$preferred_model,
  #       summary_llm = api_settings()$preferred_model,
  #       agent_llm = api_settings()$preferred_model,
  #       max_answer_attempts = 3,
  #       api_key = api_settings()$api_key
  #     )
  #     
  #     
  #     # Send POST request to the API
  #     response <- httr::POST(url, body = payload, encode = "json")
  #     result <- httr::content(res, "parsed")
  #     
  #     # Parse the response and update the output text area
  #     if (response$status_code == 200) {
  #       output$api_response_output <- renderText({ result$formatted_answer })
  #     } else {
  #       error_message <- paste("An error occurred. Status code:", response$status_code)
  #       print(error_message)
  #       output$api_response_output <- renderText({ error_message })
  #     }
  #   } else {
  #     # If the input is empty, prompt the user to enter a question
  #     showModal(modalDialog("Please enter a question.", easyClose = TRUE))
  #     #output$api_response_output <- renderText("Please enter a question.")
  #   }
  # })
  

  
     
  source("server/server_explore.R", local=T)
  source("server/server_discovery.R", local=T)
  source("server/server_doc.R", local=T)

  source("utils/utils_enrichment.R")
  source("server/server_mining_set_cond.R", local=T)
  source("server/server_mining_network.R", local=T)
  source("server/server_mining_ora.R", local=T)
  source("server/server_mining_gsea.R", local=T)
  source("server/server_mining_gtex.R", local=T)
  source("server/server_mining_tfcof.R", local=T)
  source("server/server_mining_tftf.R", local=T)
  source("server/server_mining_isoform.R", local=T)
  source("server/server_mining_tfa.R", local=T)
  # 
  source("server/server_login.R", local=T)
  
  #remove after debugging
  shinyjs::runjs("$(document).ready(function() { $('#btnCreateDoro').click(); });")
  
  
    
   
} 
  

