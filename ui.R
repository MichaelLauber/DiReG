library(dplyr)
library(visNetwork)
library(plotly)
library(DT)
library(shinyglide) 
source("utils/utils_popover.R")             
                             
fluidPage(            
             
  tags$head(
    #for fileinput in AME?  
     tags$script(src = "load-example.js"), 
     
     tags$style(HTML("
      pre {
        white-space: pre-wrap;
        word-break: break-word;
      }
    "))      
  ),  
          
  includeCSS("css/style.css"),
  waiter::use_waiter(),
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "
    var timeout;
    shinyjs.idleTimer = function(time) {
      clearTimeout(timeout); 
      timeout = setTimeout(function(){Shiny.onInputChange('idle', true);}, time*1000);
      $(document).on('mousemove keypress', function(e) {
        clearTimeout(timeout);  
        timeout = setTimeout(function(){Shiny.onInputChange('idle', true);}, time*1000);
      });
    };
    shinyjs.resetIdleTimer = function() {
      clearTimeout(timeout);
    };
  ", functions = c("idleTimer", "resetIdleTimer")),
                     
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
    shinyjs::hidden(textInput("csrf_token", "CSRF Token")),
      
    source("ui/ui_pred_ame.R")$value,
    source("ui/ui_mining.R")$value,         
    source("ui/ui_explore.R")$value,              
    tabPanel("Login", uiOutput("login_tabset") ),
    source("ui/ui_home.R")$value,
      
    
    source("ui/ui_documentation.R")$value,
    
              
         
    hr()  
  )        
                            
)  
