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
  source("utils/utils_login.R") 
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
  
  csrf_token <- paste0(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
  session$userData$csrf_token <- csrf_token
  
  # Provide the token to the UI
  updateTextInput(session, "csrf_token", value = csrf_token)
  
  # key <- readRDS("key_passphrase.rds")
  # encrypted_passphrase  <- readRDS("encrypted_passphrase.rds") 
  # db_passphrase <- rawToChar(sodium::data_decrypt(encrypted_passphrase, key))
  # aes_key_openai_api <- readRDS("aes_key_openai_api.rds")
  
  login_mode <- reactiveVal("login")
  logged_in_user <- reactiveVal(NULL)
  login_attempts <- reactiveValues()
  openai_key <- reactiveVal(NULL)
  favorite_model <- reactiveVal(NULL)
  key_uploaded <- reactiveVal(FALSE)
  favorite_model <- reactiveVal(NULL)
  
  output$login_tabset <- renderUI({
    if (is.null(logged_in_user())) {
      tabsetPanel(id = "main_tabs",
                  tabPanel("Login", value = "login_tab", uiOutput("login_ui"))
      )
    } else {
      tabsetPanel(id = "main_tabs",
                  tabPanel("Login", value = "login_tab", uiOutput("login_ui")),
                  tabPanel("Manage OpenAI Key", value = "manage_key_tab", uiOutput("manage_key_ui"))
      )
    }
  })
  
  output$login_ui <- renderUI({
    if (is.null(logged_in_user())) {
      if (login_mode() == "login") {
        tagList(
          textInput("login_user", "Username"),
          passwordInput("login_password", "Password"),
          actionButton("login_button", "Login"),
          br(),
          actionLink("show_register", "Register")
        )
      } else {
        tagList(
          textInput("register_user", "Username"),
          passwordInput("register_password", "Password"),
          passwordInput("confirm_password", "Confirm Password"),
          textInput("email", "Email"),
          actionButton("register_button", "Register"),
          br(),
          actionLink("show_login", "Back to Login")
        )
      }
    } else {
      # Show Logout button when logged in
      tagList(
        h4(paste("Welcome,", logged_in_user())),
        actionButton("logout_button", "Logout")
      )
    }
  })
  
  # Switch to registration mode
  observeEvent(input$show_register, {
    login_mode("register")
  })
  
  # Switch back to login mode
  observeEvent(input$show_login, {
    login_mode("login")
  })
  
  observeEvent(input$register_button, {
    
    tryCatch({
      validate_csrf(session$userData$csrf_token, input$csrf_token)
    }, error = function(e) {
      showNotification("An error occurred!", type = "error")
    })
    
    # Check for empty fields
    if (input$register_user == "" || input$register_password == "" || 
        input$confirm_password == "" || input$email == "") {
      showModal(modalDialog("All fields must be filled.", easyClose = TRUE))
      return()
    }
    
    # Check if passwords match
    if (input$register_password != input$confirm_password) {
      showModal(modalDialog("Passwords do not match.", easyClose = TRUE))
      return()
    }
    # In your registration logic
    password_check <- isStrongPassword(input$register_password)
    if (password_check != TRUE) {
      showModal(modalDialog(password_check, easyClose = TRUE))
      return()
    }
    
    # Check if username or email already exists
    tryCatch({
      #con <- dbConnect(RSQLite::SQLite(), dbname = "./database.sqlite")
      #on.exit(dbDisconnect(con))
      
      credentials <- read_db_decrypt(
        conn = con, 
        name = "credentials", 
        passphrase = db_passphrase
      )
      
      if (input$register_user %in% credentials$user) {
        showModal(modalDialog(
          "Username is already registered.",
          easyClose = TRUE
        ))
        return()
      }
      
      if (input$email %in% credentials$email) {
        showModal(modalDialog(
          "Email is already registered.",
          easyClose = TRUE
        ))
        return()
      }
      
      new_user <- data.frame(
        user = input$register_user,
        password = bcrypt::hashpw(input$register_password),
        # password will automatically be hashed
        start = NA,
        expire = NA,
        admin = c(FALSE),
        email = input$email,
        api_key = c(""),
        is_active = TRUE,
        is_hashed_password = T,
        stringsAsFactors = FALSE
      )
      
      credentials <- rbind(credentials, new_user)
      write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = db_passphrase)
      
      showModal(modalDialog("Registration successful!", easyClose = TRUE))
      login_mode("login")
    }, error = function(e) {
      showModal(modalDialog("Error during registration.", easyClose = TRUE))
    }
    )
  })
  
  observeEvent(input$login_button, {
    
    tryCatch({
      validate_csrf(session$userData$csrf_token, input$csrf_token)
    }, error = function(e) {
      showNotification("An error occurred!", type = "error")
    })
    
    user <- input$login_user
    print(user)
    submitted_password <- input$login_password
    print(submitted_password)
    
    #con <- dbConnect(RSQLite::SQLite(), dbname = "./database.sqlite")
    #on.exit(dbDisconnect(con))
    
    credentials <- read_db_decrypt(conn = con,  name = "credentials",  passphrase = db_passphrase)
    
    if (!user %in% credentials$user) {
      showModal(modalDialog("Username does not exist."))
      return()
    }
    
    user_info <- credentials[credentials$user == user, ]
    stored_hash <- user_info$password
    
    
    if (!bcrypt::checkpw(submitted_password, stored_hash)) {
      login_attempts[[user]] <- (login_attempts[[user]] %||% 0) + 1
      print(login_attempts)
      if (login_attempts[[user]] >= 3) {
        Sys.sleep(5)  # Delay for 5 seconds
      }
      if (login_attempts[[user]] >= 5) {
        lock_account(user, credentials, con, db_passphrase )
        showModal(modalDialog("Too many failed login attempts. Your account has been locked."))
      } else {
        showModal(modalDialog("Invalid password."))
      }
      return()
    }
    print(user_info$is_active)
    if (!as.logical(user_info$is_active)) {
      showModal(modalDialog("Your account is inactive. Please contact support."))
      return()
    }
    
    logged_in_user(user)
    login_attempts[[user]] <- 0
    js$idleTimer(300)
    
    # Check if the API key exists
    
    if (!is.null(user_info$api_key) && user_info$api_key != "") {
      print("yes here")
      
      encrypted_key_base64 <- user_info$api_key
      encrypted_data_with_iv <- base64_decode(encrypted_key_base64)
      iv <- encrypted_data_with_iv[1:16]
      encrypted_data <- encrypted_data_with_iv[-(1:16)]
      # Decrypt
      decrypted_key_raw <- aes_cbc_decrypt(encrypted_data, key = aes_key_openai_api, iv = iv)
      decrypted_key <- rawToChar(decrypted_key_raw)
      # Set openai_key()
      openai_key(decrypted_key)
      key_uploaded(TRUE)  # Update reactive value
      
      # Set placeholder and update button text
      updateTextInput(session, "openai_key", value = "", placeholder = "OpenAI key already loaded")
      updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
      showModal(modalDialog("Successfully logged in and stored openai api key loaded!"))
    } else {
      print("No here")
      # Reset to default
      openai_key(NULL)
      key_uploaded(FALSE)
      updateTextInput(session, "openai_key", value = "", placeholder = "Enter your OpenAI key")
      updateActionButton(session, "upload_key_button", label = "Upload OpenAI Key")
      showModal(modalDialog("Successfully logged in!"))
    }
    
    
  })
  
  observeEvent(input$logout_button, {
    
    tryCatch({
      validate_csrf(session$userData$csrf_token, input$csrf_token)
    }, error = function(e) {
      showNotification("An error occurred!", type = "error")
    })
    
    openai_key(NULL) # Remove OpenAI key if not saved
    logged_in_user(NULL)
    login_mode("login")
    favorite_model(NULL)
    key_uploaded(FALSE) 
    enable("openai_key_input")
    js$resetIdleTimer()
    showModal(modalDialog("You have been logged out.", easyClose = TRUE))
  })
  
  observeEvent(input$save_model_button, {
    favorite_model(input$model_dropdown)
    showModal(modalDialog("Favorite model preference saved!", easyClose = TRUE))
  })
  
  output$manage_key_ui <- renderUI({
    if (!is.null(logged_in_user())) {
      tagList(
        textInput("openai_key", "Enter OpenAI Key", value = NULL, placeholder = "Enter your OpenAI key"),
        actionButton("upload_key_button", ifelse(key_uploaded(), "Update OpenAI Key", "Upload OpenAI Key")),
        actionButton("delete_key_button", "Delete API Key"),
        checkboxInput("save_key", "Save key in database", FALSE),
        br(),
        selectInput("model_dropdown", "Select Favorite OpenAI Model",
                    choices = c("gpt-4o-mini", "gpt-4o",  "o1", "o1-mini"),
                    selected = favorite_model()),
        actionButton("save_model_button", "Save Model Preference")
      )
    }
  })
  
  previous_model <- reactiveVal(NULL)
  
  observeEvent(input$model_dropdown, {
    # Check if the selection has genuinely changed
    if (!is.null(previous_model()) && previous_model() != input$model_dropdown) {
      favorite_model(input$model_dropdown)  # Update reactive value
      
      if (isTRUE(key_uploaded())) {  # Check if the key is uploaded
        showModal(modalDialog(
          sprintf("%s will be used for all calculations!", input$model_dropdown), 
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          "An OpenAI API key needs to be loaded!", 
          easyClose = TRUE
        ))
      }
    }
    
    # Update the previous selection to the current value
    previous_model(input$model_dropdown)
  }, ignoreInit = TRUE)  # Prevent triggering on initialization
  
  # Handle uploading OpenAI key
  observeEvent(input$upload_key_button, {
    
    tryCatch({
      validate_csrf(session$userData$csrf_token, input$csrf_token)
    }, error = function(e) {
      showNotification("An error occurred!", type = "error")
    })
    
    if (is.null(input$openai_key) || input$openai_key == "") {
      showModal(modalDialog("Please enter a valid OpenAI Key before uploading.", easyClose = TRUE))
      return()
    }
    
    # Validate the OpenAI key
    is_valid <- validate_openai_key(input$openai_key)
    
    if (!is_valid) {
      showModal(modalDialog("The provided OpenAI Key is invalid or not working.", easyClose = TRUE))
      return()
    }
    
    openai_key(input$openai_key)
    key_uploaded(TRUE)  # Update reactive value
    
    
    if (input$save_key) {
      tryCatch({
        #con <- dbConnect(RSQLite::SQLite(), dbname = "./database.sqlite")
        #on.exit(dbDisconnect(con))
        
        credentials <- read_db_decrypt(conn = con,  name = "credentials",  passphrase = db_passphrase)
        
        # Encrypt the key
        encrypted_test_key_raw <- aes_cbc_encrypt(charToRaw(input$openai_key), key = aes_key_openai_api)
        iv <- attr(encrypted_test_key_raw, "iv")  # Retrieve the IV used for encryption
        encrypted_data_with_iv <- c(iv, encrypted_test_key_raw)  # Prepend IV to the encrypted data
        
        # Encode to Base64 for storage
        encrypted_test_key_base64 <- base64_encode(encrypted_data_with_iv)
        update_api_key(logged_in_user(), credentials, con, db_passphrase, encrypted_test_key_base64)
        
        showModal(modalDialog("OpenAI Key has been securely saved in the database.", easyClose = TRUE))
        #disable("openai_key_input")     # Disable the input field for safety
        updateTextInput(session, "openai_key", value = "",  placeholder = "OpenAI key already loaded")
        updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
      }, error = function(e) {
        showModal(modalDialog("Failed to save OpenAI Key.", easyClose = TRUE))
      })
    } else {
      showModal(modalDialog("OpenAI Key uploaded but not saved.", easyClose = TRUE))
      #disable("openai_key_input")     # Disable the input field for safety
      updateTextInput(session, "openai_key", value = "",  placeholder = "OpenAI key already loaded")
      updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
    }
    
  })
  
  output$show_manage_key <- reactive({
    !is.null(logged_in_user())
  })
  outputOptions(output, "show_manage_key", suspendWhenHidden = FALSE)
  
  observeEvent(input$delete_key_button, {
    tryCatch({
      #con <- dbConnect(RSQLite::SQLite(), dbname = "./database.sqlite")
      #on.exit(dbDisconnect(con))
      
      credentials <- read_db_decrypt(conn = con, name = "credentials", passphrase = db_passphrase)
      credentials[credentials$user == logged_in_user(), "api_key"] <- ""
      write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = db_passphrase)
      
      openai_key(NULL)
      key_uploaded(FALSE)
      updateTextInput(session, "openai_key", value = "", placeholder = "Enter your OpenAI key")
      updateActionButton(session, "upload_key_button", label = "Upload OpenAI Key")
      showModal(modalDialog("API key deleted successfully.", easyClose = TRUE))
    }, error = function(e) {
      showModal(modalDialog("Failed to delete API key.", easyClose = TRUE))
    })
  })
    
   
} 
  

