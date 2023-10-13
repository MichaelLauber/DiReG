library(dplyr)
library(visNetwork)
library(plotly)
library(DT)
library(shinyglide) 
    
#source(file.path("utils", "utils_popover.R")) 
     

source("utils/utils_selectizeInput.R")
reprogramming_protocols <-
  read.csv(file.path("data","reprogramming_protocols.csv"))
startInputList <- createChoices(reprogramming_protocols$Start)
   
            
# source("utils/utils_selectizeInput.R")
# all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))
# startInputList_inf <- createChoices(all_infered_protocols$Start)
        
# Define UI for application that draws a histogram
fluidPage(
         
  tags$head(
     tags$script(src = "load-example.js"),
    #following lines an be removed as drugstone is not used anymore 
    # tags$script(src="https://cdn.drugst.one/latest/drugstone.js"),
    # tags$link(rel="stylesheet", href="https://cdn.drugst.one/latest/styles.css", type = "text/css"),
    # tags$style(":root {--drgstn-panel:#ccccff;--drgstn-height:400px;}"),
    # #tags$script(src = "task-listener.js"),
    # #tags$script(src = "set-color.js"),
  ),
         
  includeCSS("css/style.css"),
  waiter::use_waiter(),
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
         
  navbarPage(
    id = "menu",
    htmltools::includeScript("www/popover.js"),
    theme = bslib::bs_theme(),
         
    # theme = bslib::bs_theme(
    #   #version = 5
    #   
    #   bg = "#101010", 
    #   fg = "#FDF7F7", 
    #   primary = "#ED79F9", 
    #   base_font = bslib::font_google("Prompt"),
    #   code_font = bslib::font_google("JetBrains Mono")
    #   
    #   ), 
    windowTitle = "DiReG",
    title = "DiReG", #wrap in div() when not using bslib
    collapsible=T,
    footer = column(12, align="center", 
                        "DiReG-App 2023 (v1.0.0)",
                        ),
      
       
    source("ui/ui_mining.R")$value,
    source("ui/ui_explore.R")$value,
                
    # tabPanel(
    #   "Explore",
    #   div(
    #     id = "exploreHeader",
    #     h2(class = "center", "Find experimental validated and inferred Directed Differentiation Protocols"),
    #     #h3(class = "center", "All Protocols are wet lab established"),
    #     hr()
    #   ),
    #       
    #   sidebarLayout(sidebarPanel(
    #     div(
    #       id = "large",
    #       align = "center",
    #       shinyWidgets::radioGroupButtons(
    #         inputId = "radioExploreTyp",
    #         label = "Modus",
    #         choices = c("Experimental", 
    #                     "Computational"),
    #         justified = TRUE
    #       ),
    #       
    #       conditionalPanel(
    #         'output.cond_exploreExp == "1"',
    #         startcellSelection("selectStart", "Start Cell", startInputList),
    #         targetcellSelection('selectTarget', 'Target Cell'),
    #         radioButtons(
    #           "radioExplore",
    #           "Organism",
    #           inline = TRUE,
    #           choices = list(
    #             "Mouse" = "mouse",
    #             "Human" = "human",
    #             "Both" = "both"
    #           ),
    #           selected = "both"
    #         )
    #       ), 
    #         
    #     conditionalPanel('output.cond_exploreComp == "1"',
    #                      div(
    #                        startcellSelection("selectStart_infered", "Start Cell", startInputList_inf),
    #                        targetcellSelection('selectTarget_infered', 'Target Cell')
    #                      ),
    #                      checkboxGroupInput("checkGroupTools",
    #                                         h5("Tools"),
    #                                         inline = TRUE,
    #                                         choices = list("Mogrify" = "Mogrify",
    #                                                        "JSD"= "JSD",
    #                                                        "Taiji" = "Taiji",
    #                                                        #"IRENE" = "irene",
    #                                                        "CellNet" = "CellNet"
    #                                         ),
    #                                         selected = c("Mogrify"))
    #                      )
    #       
    #     )
    #   )
    #   ,
    #   mainPanel(
    #     
    #     conditionalPanel(
    #       'output.cond_exploreExp == "1"',
    #     div(id = "tblExplore",
    #                 tableOutput("tbl_protocols"))
    #     ),
    # 
    #     conditionalPanel('output.cond_exploreComp == "1"',
    #                      div(
    #                        h4(align = "center", "Find and compare inferred TF sets"),
    #                        p(align = "center", "The collection contains:",  br(),
    #                          "230 predictions from", a("Mogrify,", href="https://www.nature.com/articles/ng.3487"), br(),
    #                          "225 from",  a("Alessio et al", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/"), "(Here refred as JSD)", br(),
    #                          "25 from", a("Cellnet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/"), br(),
    #                          "25k from", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166"), "(Arrow indicates up/down regualtion)")
    #                      ),
    #                      tableOutput("tbl_inferred_protocols")
    #                      )
    # 
    #     )
    # ) 
    # ), 
            
     
    
    source("ui/ui_home.R")$value,
    
    source("ui/ui_pred_ame.R")$value,
    
    
    hr()
  )
                         
)  
