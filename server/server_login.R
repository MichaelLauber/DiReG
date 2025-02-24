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
openai_key    <- reactiveVal(NULL)
favorite_model <- reactiveVal(NULL)
key_uploaded  <- reactiveVal(FALSE)

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
      textInput("openai_key", "Enter OpenAI Key", value = NULL, placeholder = "Enter your OpenAI key"),
      actionButton("upload_key_button", ifelse(key_uploaded(), "Update OpenAI Key", "Upload OpenAI Key")),
      actionButton("delete_key_button", "Delete Key From DB"),
      checkboxInput("save_key", "Save Key in local DB", FALSE),
      br(),
      selectInput("model_dropdown", "Select Favorite OpenAI Model",
                  choices = c("gpt-4o-mini", "gpt-4o",  "o1", "o3-mini"),
                  selected = favorite_model())
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
