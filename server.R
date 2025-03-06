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
  
  #docker run -p 8000:8000 -v /host/path/to/papers:/app/papers paperqa-endpoint
  
     
  #source("server/server_explore.R", local=T)
  source("server/server_discovery.R", local=T)
   source("server/server_doc.R", local=T)
  # source("utils/utils_enrichment.R")
  # source("server/server_mining_set_cond.R", local=T) 
  # source("server/server_mining_network.R", local=T)
  # source("server/server_mining_ora.R", local=T)
  # source("server/server_mining_gsea.R", local=T)
  # source("server/server_mining_exp.R", local=T)
  # source("server/server_mining_tfcof.R", local=T)
  # source("server/server_mining_tftf.R", local=T)
  # source("server/server_mining_isoform.R", local=T)
  # source("server/server_mining_tfa.R", local=T)
  # source("server/server_login.R", local=T)
  
  #remove after debugging
  #shinyjs::runjs("$(document).ready(function() { $('#btnCreateDoro').click(); });")
  #shinyjs::runjs("$(document).ready(function() { $('#btnGTEx').click(); });")
  
  
    
   
} 
  

