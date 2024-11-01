library(dplyr)
library(visNetwork)
library(plotly)
library(DT)
library(shinyglide) 
#library(ollamar)    
   
#source("utils/utils_selectizeInput.R")
# reprogramming_protocols <-
#   read.csv(file.path("data","reprogramming_protocols.csv"))
# startInputList <- createChoices(reprogramming_protocols$Start)
                       
            
fluidPage(
         
  tags$head(
    #for fileinput in AME?
     tags$script(src = "load-example.js"),
  ),
         
  includeCSS("css/style.css"),
  waiter::use_waiter(),
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
                    
  navbarPage(
    id = "menu",
    htmltools::includeScript("www/popover.js"),
    theme = bslib::bs_theme(),
    windowTitle = "DiReG",
    title = "DiReG", #wrap in div() when not using bslib
    collapsible=T,
    footer = column(12, align="center", 
                        "DiReG-App 2024 (v1.0.0)",
                        ),
    source("ui/test_llm.R")$value,      
    source("ui/ui_home.R")$value,
      
    source("ui/ui_explore.R")$value,
       
    source("ui/ui_mining.R")$value,
      
    source("ui/ui_pred_ame.R")$value,
    
    source("ui/ui_documentation.R")$value,
     
       
    hr()
  )
                         
)  
