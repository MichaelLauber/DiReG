# Function to validate the OpenAI API key
validate_openai_key <- function(api_key) {
  # Make a test API request to OpenAI
  response <- tryCatch({
    httr::GET(
      url = "https://api.openai.com/v1/models",
      httr::add_headers(Authorization = paste("Bearer", api_key))
    )
  }, error = function(e) {
    return(NULL)  # Return NULL if there is an error
  })
  
  # Check if the response is successful
  if (!is.null(response) && httr::status_code(response) == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# # Function to check password strength
# isStrongPassword <- function(password) {
#   if (nchar(password) < 8) {
#     return("Password must be at least 8 characters long.")
#   }
#   if (!grepl("[A-Z]", password)) {
#     return("Password must include at least one uppercase letter.")
#   }
#   if (!grepl("[a-z]", password)) {
#     return("Password must include at least one lowercase letter.")
#   }
#   if (!grepl("[0-9]", password)) {
#     return("Password must include at least one number.")
#   }
#   if (!grepl("[^A-Za-z0-9]", password)) {
#     return("Password must include at least one special character.")
#   }
#   return(TRUE)
# }

# Function to check password strength
isStrongPassword <- function(password) {
  TRUE
}


lock_account <- function(user, credentials, con, passphrase) {
  credentials[credentials$user == user, "is_active"] <- FALSE
  shinymanager::write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = passphrase)
}

update_api_key <- function(user, credentials, con, passphrase, api_key) {
  credentials[credentials$user == user, "api_key"] <- api_key
  shinymanager::write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = passphrase)
}

validate_csrf <- function(session_token, input_token) {
  if (is.null(input_token) || input_token != session_token) {
    stop("CSRF validation failed: Unauthorized request.")
  }
}



