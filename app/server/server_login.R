source("utils/utils_login.R") 

path_to_db <- "login/database.sqlite"

get_credentials <- function(passphrase) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_to_db)
  on.exit(DBI::dbDisconnect(con))
  shinymanager::read_db_decrypt(conn = con, name = "credentials", passphrase = passphrase)
}

save_credentials <- function(credentials, passphrase) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_to_db)
  on.exit(DBI::dbDisconnect(con))
  shinymanager::write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = passphrase)
}

csrf_token <- sodium::bin2hex(sodium::random(32))
session$userData$csrf_token <- csrf_token
updateTextInput(session, "csrf_token", value = csrf_token)

key <- readRDS("login/key_passphrase.rds")
encrypted_passphrase <- readRDS("login/encrypted_passphrase.rds") 
db_passphrase <- rawToChar(sodium::data_decrypt(encrypted_passphrase, key))
aes_key_openai_api <- readRDS("login/aes_key_openai_api.rds")

login_mode    <- reactiveVal("login")
logged_in_user <- reactiveVal(NULL)
login_attempts <- reactiveValues()


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
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          p("Login to access your personal OpenAI API key management system. Once logged in, you can:"),
          tags$ul(
            tags$li("Securely store your OpenAI API key in our encrypted database"),
            tags$li("Select your preferred AI model (GPT-4o, GPT-4o-mini, o1, o3-mini)"),
            tags$li("Use your API key for all LLM requests within the application"),
            tags$li("Update or delete your stored API key at any time")
          ),
          p(strong("Your API key is encrypted and stored securely. We never share or misuse your credentials."))
        ),
        textInput("login_user", "Username"),
        passwordInput("login_password", "Password"),
        actionButton("login_button", "Login", class = "btn-primary"),
        br(), br(),
        actionLink("show_register", "Don't have an account? Register here", 
                   style = "color: #007bff; text-decoration: none;")
      )
    } else {
      tagList(
        div(
          style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          h4("Create Your Account", style = "color: #2c3e50; margin-top: 0;"),
          h5("Password Requirements:", style = "color: #495057; margin-bottom: 10px;"),
          tags$ul(
            style = "margin-bottom: 15px;",
            tags$li("At least 8 characters long"),
            tags$li("At least one uppercase letter (A-Z)"),
            tags$li("At least one lowercase letter (a-z)"),
            tags$li("At least one number (0-9)"),
            tags$li("At least one special character (!@#$%^&*)")
          ),
          p(em("Note: All passwords are securely hashed using bcrypt encryption."))
        ),
        textInput("register_user", "Username", placeholder = "Choose a unique username"),
        passwordInput("register_password", "Password", placeholder = "Enter a strong password"),
        passwordInput("confirm_password", "Confirm Password", placeholder = "Re-enter your password"),
        textInput("email", "Email", placeholder = "your.email@example.com"),
        actionButton("register_button", "Create Account", class = "btn-success"),
        br(), br(),
        actionLink("show_login", "Already have an account? Login here", 
                   style = "color: #007bff; text-decoration: none;")
      )
    }
  } else {
    tagList(
      h4(paste("Welcome,", logged_in_user())),
      actionButton("logout_button", "Logout")
    )
  }
})

observeEvent(input$show_register, {
  login_mode("register")
})

observeEvent(input$show_login, {
  login_mode("login")
})

observeEvent(input$register_button, {
  tryCatch({
    validate_csrf(session$userData$csrf_token, input$csrf_token)
  }, error = function(e) {
    showNotification("An error occurred! CSRF Tokens do not match!", type = "error")
  })
  
  if (input$register_user == "" || input$register_password == "" || 
      input$confirm_password == "" || input$email == "") {
    showModal(modalDialog("All fields must be filled.", easyClose = TRUE))
    return()
  }
  
  if (input$register_password != input$confirm_password) {
    showModal(modalDialog("Passwords do not match.", easyClose = TRUE))
    return()
  }
  
  password_check <- isStrongPassword(input$register_password)
  if (password_check != TRUE) {
    showModal(modalDialog(password_check, easyClose = TRUE))
    return()
  }
  
  tryCatch({
    credentials <- get_credentials(db_passphrase)
    
    if (input$register_user %in% credentials$user) {
      showModal(modalDialog("Username is already registered.", easyClose = TRUE))
      return()
    }
    
    if (input$email %in% credentials$email) {
      showModal(modalDialog("Email is already registered.", easyClose = TRUE))
      return()
    }
    
    new_user <- data.frame(
      user = input$register_user,
      password = bcrypt::hashpw(input$register_password),
      start = NA,
      expire = NA,
      admin = FALSE,
      email = input$email,
      api_key = "",
      is_active = TRUE,
      is_hashed_password = TRUE,
      stringsAsFactors = FALSE
    )
    
    credentials <- rbind(credentials, new_user)
    save_credentials(credentials, db_passphrase)
    
    showModal(modalDialog("Registration successful!", easyClose = TRUE))
    login_mode("login")
  }, error = function(e) {
    showModal(modalDialog("Error during registration.", easyClose = TRUE))
  })
})

observeEvent(input$login_button, {
  tryCatch({
    validate_csrf(session$userData$csrf_token, input$csrf_token)
  }, error = function(e) {
    showNotification("An error occurred! CSRF Tokens do not match!", type = "error")
  })
  
  user <- input$login_user
  submitted_password <- input$login_password
  credentials <- get_credentials(db_passphrase)
  
  if (!user %in% credentials$user) {
    showModal(modalDialog("Username does not exist."))
    return()
  }
  
  user_info <- credentials[credentials$user == user, ]
  stored_hash <- user_info$password
  
  if (!bcrypt::checkpw(submitted_password, stored_hash)) {
    login_attempts[[user]] <- (login_attempts[[user]] %||% 0) + 1
    if (login_attempts[[user]] >= 3) Sys.sleep(5)
    if (login_attempts[[user]] >= 5) {
      lock_account(user, credentials, NULL, db_passphrase)
      showModal(modalDialog("Too many failed login attempts. Your account has been locked."))
    } else {
      showModal(modalDialog("Invalid password."))
    }
    return()
  }
  
  if (!as.logical(user_info$is_active)) {
    showModal(modalDialog("Your account is inactive. Please contact support."))
    return()
  }
  
  logged_in_user(user)
  login_attempts[[user]] <- 0
  shinyjs::js$idleTimer(300)
  
  if (!is.null(user_info$api_key) && user_info$api_key != "") {
    encrypted_key_base64 <- user_info$api_key
    encrypted_data_with_iv <- openssl::base64_decode(encrypted_key_base64)
    iv <- encrypted_data_with_iv[1:16]
    encrypted_data <- encrypted_data_with_iv[-(1:16)]
    decrypted_key_raw <- openssl::aes_cbc_decrypt(encrypted_data, key = aes_key_openai_api, iv = iv)
    decrypted_key <- rawToChar(decrypted_key_raw)
    
    openai_key(decrypted_key)
    key_uploaded(TRUE)
    
    updateTextInput(session, "openai_key", value = "", placeholder = "OpenAI key already loaded")
    updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
    showModal(modalDialog("Successfully logged in and stored OpenAI API key loaded!"))
  } else {
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
    showNotification("An error occurred! CSRF Tokens do not match!", type = "error")
  })
  
  openai_key(NULL)
  logged_in_user(NULL)
  login_mode("login")
  favorite_model(NULL)
  key_uploaded(FALSE) 
  shinyjs::enable("openai_key_input")
  shinyjs::js$resetIdleTimer()
  showModal(modalDialog("You have been logged out.", easyClose = TRUE))
})

output$manage_key_ui <- renderUI({
  if (!is.null(logged_in_user())) {
    tagList(
      div(
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        p("Manage your OpenAI API key and model preferences for all LLM requests in this application."),
        tags$ul(
          tags$li(strong("Security:"), "Your API key is encrypted using AES-256 encryption before storage"),
          tags$li(strong("Privacy:"), "Keys are stored locally and never transmitted to third parties"),
          tags$li(strong("Flexibility:"), "You can update or delete your key at any time"),
          tags$li(strong("Convenience:"), "Saved keys are automatically loaded when you login")
        )
      ),
      
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        h5("How to get your OpenAI API Key:", style = "color: #495057; margin-top: 0;"),
        tags$ol(
          tags$li("Visit ", tags$a("platform.openai.com", href = "https://platform.openai.com", target = "_blank")),
          tags$li("Sign in to your OpenAI account"),
          tags$li("Navigate to API → API Keys"),
          tags$li("Click 'Create new secret key'"),
          tags$li("Copy the key and paste it below")
        ),
        p(em("Note: API keys start with 'sk-' and are typically 51 characters long."))
      ),
      
      textInput("openai_key", "Enter OpenAI API Key", 
                value = NULL, 
                placeholder = if(key_uploaded()) "OpenAI key already loaded" else "sk-..."),
      
      div(
        style = "margin-bottom: 15px;",
        checkboxInput("save_key", 
                      "Save Key in encrypted database (recommended for frequent use)", 
                      FALSE),
        tags$small("Uncheck this if you prefer to enter your key each session.", 
                   style = "color: #6c757d;")
      ),
      
      actionButton("upload_key_button", 
                   ifelse(key_uploaded(), "Update OpenAI Key", "Upload OpenAI Key"),
                   class = "btn-primary",
                   style = "margin-right: 10px;"),
      
      actionButton("delete_key_button", "Delete Key From Database", 
                   class = "btn-outline-danger"),
      
      br(), br(),
      
      div(
        style = "background-color: #e7f3ff; padding: 15px; border-radius: 5px;",
        h5("Model Selection", style = "color: #004085; margin-top: 0;"),
        p("Choose your preferred OpenAI model for all LLM requests:"),
        tags$ul(
          tags$li(strong("GPT-4o:"), "Most capable model, best for complex tasks"),
          tags$li(strong("GPT-4o-mini:"), "Faster and more cost-effective, good for most tasks"),
          tags$li(strong("o1:"), "Advanced reasoning model for complex problem-solving"),
          tags$li(strong("o3-mini:"), "Latest reasoning model, optimized for efficiency")
        ),
        selectInput("model_dropdown", "Select Favorite OpenAI Model",
                    choices = c("gpt-4o-mini", "gpt-4o", "o1", "o3-mini"),
                    selected = favorite_model()),
        tags$small("This model will be used for all AI requests in the application.", 
                   style = "color: #6c757d;")
      )
    )
  }
})

previous_model <- reactiveVal(NULL)

observeEvent(input$model_dropdown, {
  if (!is.null(previous_model()) && previous_model() != input$model_dropdown) {
    favorite_model(input$model_dropdown)
    if (isTRUE(key_uploaded())) {
      showModal(modalDialog(sprintf("%s will be used for all calculations!", input$model_dropdown), easyClose = TRUE))
    } else {
      showModal(modalDialog("An OpenAI API key needs to be loaded!", easyClose = TRUE))
    }
  }
  previous_model(input$model_dropdown)
}, ignoreInit = TRUE)

observeEvent(input$upload_key_button, {
  tryCatch({
    validate_csrf(session$userData$csrf_token, input$csrf_token)
  }, error = function(e) {
    showNotification("An error occurred! CSRF Tokens do not match!", type = "error")
  })
  
  if (is.null(input$openai_key) || input$openai_key == "") {
    showModal(modalDialog("Please enter a valid OpenAI Key before uploading.", easyClose = TRUE))
    return()
  }
  
  is_valid <- validate_openai_key(input$openai_key)
  
  if (!is_valid) {
    showModal(modalDialog("The provided OpenAI Key is invalid or not working.", easyClose = TRUE))
    return()
  }
  
  openai_key(input$openai_key)
  key_uploaded(TRUE)
  
  if (input$save_key) {
    tryCatch({
      credentials <- get_credentials(db_passphrase)
      encrypted_test_key_raw <- openssl::aes_cbc_encrypt(charToRaw(input$openai_key), key = aes_key_openai_api)
      iv <- attr(encrypted_test_key_raw, "iv")
      encrypted_data_with_iv <- c(iv, encrypted_test_key_raw)
      encrypted_test_key_base64 <- openssl::base64_encode(encrypted_data_with_iv)
      credentials[credentials$user == logged_in_user(), "api_key"] <- encrypted_test_key_base64
      save_credentials(credentials, db_passphrase)
      
      showModal(modalDialog("OpenAI Key has been securely saved in the database.", easyClose = TRUE))
      updateTextInput(session, "openai_key", value = "", placeholder = "OpenAI key already loaded")
      updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
    }, error = function(e) {
      showModal(modalDialog("Failed to save OpenAI Key.", easyClose = TRUE))
    })
  } else {
    showModal(modalDialog("OpenAI Key uploaded but not saved.", easyClose = TRUE))
    updateTextInput(session, "openai_key", value = "", placeholder = "OpenAI key already loaded")
    updateActionButton(session, "upload_key_button", label = "Update OpenAI Key")
  }
})

output$show_manage_key <- reactive({
  !is.null(logged_in_user())
})
outputOptions(output, "show_manage_key", suspendWhenHidden = FALSE)

observeEvent(input$delete_key_button, {
  tryCatch({
    credentials <- get_credentials(db_passphrase)
    credentials[credentials$user == logged_in_user(), "api_key"] <- ""
    save_credentials(credentials, db_passphrase)
    
    openai_key(NULL)
    key_uploaded(FALSE)
    updateTextInput(session, "openai_key", value = "", placeholder = "Enter your OpenAI key")
    updateActionButton(session, "upload_key_button", label = "Upload OpenAI Key")
    showModal(modalDialog("API key deleted successfully.", easyClose = TRUE))
  }, error = function(e) {
    showModal(modalDialog("Failed to delete API key.", easyClose = TRUE))
  })
})
