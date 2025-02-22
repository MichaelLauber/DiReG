function(input, output, session) {
  
  options(shiny.error = browser)   

   
  # Set your API key
  api_key <- "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"
  
  # Define the API endpoint
  url <- "https://api.openai.com/v1/chat/completions" 
  
  
  ### server_explore 
  
  example_question <- "How can I differentiate Pancreatic duct cells into beta cells? Which transcription factors are necessary?"

  # Load example question when the Example button is pressed
  observeEvent(input$explore_example_btn, {
    print("Example Pressed")
    updateTextAreaInput(session, "user_prompt_explore", value = example_question)
  })

  observeEvent(input$explore_prompt_btn, {
    print("Ask Pressed")
    # Get the user question from the text area input
    user_question <- input$user_prompt_explore
    print("sending user question")
    
    shinyjs::runjs("$('#api_response_output').text('Generating response, please wait...');")
    
    # Only proceed if user question is not empty
    if (nzchar(user_question)) {
      # Send POST request to the API
      response <- httr::POST(
        url = "http://localhost:8000/process_query",
        body = list(question = user_question),
        encode = "json"
      )

      # Parse the response and update the output text area
      if (response$status_code == 200) {
        result <- httr::content(response)$result
        print(result)
        output$api_response_output <- renderText({ result })
      } else {
        error_message <- paste("An error occurred. Status code:", response$status_code)
        print(error_message)
        output$api_response_output <- renderText({ error_message })
      }
    } else {
      # If the input is empty, prompt the user to enter a question
      output$api_response_output <- renderText("Please enter a question.")
    }
  })
  
  
  
   
  #place in server_mining_ora
  
  observeEvent(input$submit_prompt_ora_btn, {
    
    user_prompt <- paste0("Do the overrepresentated genesets show specifity for:", input$user_prompt_ora, "-These are the enriched genesets: ", paste(cache$enrichData$result$term_name, collapse = ", "))
    output$llm_response_ora <- renderText("Generating response, please wait...")
    shinyjs::runjs("$('#llm_response_ora').text('Generating response, please wait...');")
    
    
    ####Using openaiAPI
    
    data <- jsonlite::toJSON(list(
      model = "gpt-4o-mini",  # Specify the model you want to use (e.g., "gpt-3.5-turbo" or "davinci")
      messages = list(
        list(role = "user", content = user_prompt)
      ),
      max_tokens = 1000
    ), auto_unbox = TRUE)
    
    response <- httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = data,
      encode = "json"
    )  
    
    print(response)
    
    # Check the response status
    if (httr::status_code(response) == 200) {
      # Parse the response
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed_content <- jsonlite::fromJSON(content)
      
      print("Total tokens:")
      print(parsed_content$usage$total_tokens)
      
      response_to_display <- parsed_content$choices$message$content
      # Extract and print the response text
      #cat("Response from OpenAI:\n")
      #cat(parsed_content$choices$message$content, "\n")
    } else {
      # Print an error message if the request fails
      cat("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
      response_to_display <- paste0("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
    }
    
    #####
    output$llm_response_ora <- renderText({
      response_to_display
    })
  })  
 
  #place in server_mining_gsea
  observeEvent(input$submit_prompt_gsea_btn, {
    
    user_prompt <- paste0("Do the enriched genesets show specifity for:", 
                          input$user_prompt_gsea, 
                          "-These are the enriched genesets: ", 
                          paste(cache_gsea$comb_gsea_res$pathway, collapse = ", "))
    output$llm_response_gsea <- renderText("Generating response, please wait...")
    shinyjs::runjs("$('#llm_response_gsea').text('Generating response, please wait...');")
    
    ####Using openaiAPI
    
    data <- jsonlite::toJSON(list(
      model = "gpt-4o-mini",  # Specify the model you want to use (e.g., "gpt-3.5-turbo" or "davinci")
      messages = list(
        list(role = "user", content = user_prompt)
      ),
      max_tokens = 1000
    ), auto_unbox = TRUE)
    
    response <- httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = data,
      encode = "json"
    )
    
    print(response)
    
    # Check the response status
    if (httr::status_code(response) == 200) {
      # Parse the response
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed_content <- jsonlite::fromJSON(content)
      
      print("Total tokens:")
      print(parsed_content$usage$total_tokens)
      
      response_to_display <- parsed_content$choices$message$content
      # Extract and print the response text
      #cat("Response from OpenAI:\n")
      #cat(parsed_content$choices$message$content, "\n")
    } else {
      # Print an error message if the request fails
      cat("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
      response_to_display <- paste0("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
    }
    
    #####
    output$llm_response_gsea <- renderText({
      response_to_display
    })
  })
  
  
  
  cond_visnet <- reactiveVal(0)
  cond_visnet(1)
  
   observe({
   shinyjs::runjs(sprintf('window.cond_visnet = "%s"', cond_visnet())) # set to 0 in eventReactive(input$btnCreateDoro...
   })
  
  
  ####### mining  
   
  #loads example TFs in the input field within Signature mining
  observeEvent(input$btnMiningExample, {
    value <- "HNF1A HNF4A ONECUT1 ATF5 PROX1 CEBPA"
    updateTextInput(session, "inputTextTFs", value=value)
  })
  
  
  #All  performed analysis are hidden
  hideAll <- function(){
    cond_ora(0)
    cond_gsea(0)
    cond_gtex(0)
    cond_tfcof(0)
    cond_tftf(0)
    cond_isoforms(0)
    cond_tfa(0)
  }   
  
  
  resetBtns <- function(){
    btn_ids <- c("btnORA", "btnGSEA", "btnGTEx", "btnTfcof", "btnTFTF", "btnIsoforms", "btnTFA")
    for (btn_id in btn_ids) {
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-success')", btn_id))
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-primary')", btn_id))
    }
  }
    
  handleButtonClick <- function(buttonId, conditionSetter) {
    observeEvent(input[[buttonId]], {
      
      if (!networkCreated) {
        return()
      }

      hideAll()
      resetBtns()
      conditionSetter(1)
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-primary')", buttonId))
      shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-success')", buttonId))
      
      shinyjs::toggle(id = 'networkContainer', condition = FALSE)
      shinyjs::toggle(id = 'expandButtonContainer', condition = TRUE) 
    })
  }
  
  observeEvent(input$expandButton, {
    shinyjs::toggle(id = "networkContainer", condition = TRUE)  # Show the network container
    shinyjs::toggle(id = "expandButtonContainer", condition = FALSE)  # Hide the button container
  })
 
 
  cond_ora <- reactiveVal(0)
  cond_gsea <- reactiveVal(0)
  cond_gtex <- reactiveVal(0)
  cond_tfcof <- reactiveVal(0)
  cond_tftf <- reactiveVal(0)
  cond_isoforms <- reactiveVal(0)
  cond_tfa <- reactiveVal(0)

  handleButtonClick("btnORA", cond_ora)
  handleButtonClick("btnGSEA", cond_gsea)
  handleButtonClick("btnGTEx", cond_gtex)
  handleButtonClick("btnTfcof", cond_tfcof)
  handleButtonClick("btnTFTF", cond_tftf)
  handleButtonClick("btnIsoforms", cond_isoforms)
  handleButtonClick("btnTFA", cond_tfa)
  
  
  output$cond_ora = renderText({cond_ora()})
  output$cond_gsea = renderText({cond_gsea()})
  output$cond_gtex = renderText({cond_gtex()})
  output$cond_tfcof = renderText({cond_tfcof()})
  output$cond_tftf = renderText({cond_tftf()})
  output$cond_isoforms = renderText({cond_isoforms()})
  output$cond_tfa = renderText({cond_tfa()})

  observeEvent(input$ok, {
    cond_tfa(1)
  })
  
  outputOptions(output, 'cond_ora', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_gsea', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_gtex', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_tfcof', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_tftf', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_isoforms', suspendWhenHidden=FALSE)
  outputOptions(output, 'cond_tfa', suspendWhenHidden=FALSE)

  #### End Mining
  
   
  ##Explore
  
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
      
 
             
     
  source("server/server_explore.R", local=T)
  source("server/server_discovery.R", local=T)     
  source("server/server_doc.R", local=T)
    
  source("utils/utils_enrichment.R") 
  source("server/server_mining_network.R", local=T)
  source("server/server_mining_ora.R", local=T)
  source("server/server_mining_gsea.R", local=T)
  source("server/server_mining_gtex.R", local=T)
  source("server/server_mining_tfcof.R", local=T)
  source("server/server_mining_tftf.R", local=T)
  source("server/server_mining_isoform.R", local=T)
  source("server/server_mining_tfa.R", local=T)
  
  #remove after debugging
  shinyjs::runjs("$(document).ready(function() { $('#btnCreateDoro').click(); });")
  
    
   
} 
  

