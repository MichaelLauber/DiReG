source("utils/utils_selectizeInput.R")
all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))
startInputList_inf <- createChoices(all_infered_protocols$Start)


reprogramming_protocols <-
  read.csv(file.path("data","reprogramming_protocols.csv"))
startInputList <- createChoices(reprogramming_protocols$Start)



tabPanel(
  "Explore",
  div(
    id = "exploreHeader",
    h2(class = "center", "Find experimental validated and inferred Directed Differentiation Protocols"),
    hr()
  ),
  
  sidebarLayout(sidebarPanel(
    div(
      id = "large",
      align = "center",
      shinyWidgets::radioGroupButtons(
        inputId = "radioExploreTyp",
        label = "Modus",
        choices = c("Experimental", 
                    "Computational"),
        justified = TRUE
      ),
      
      conditionalPanel(
        'output.cond_exploreExp == "1"',
        startcellSelection("selectStart", "Start Cell", startInputList),
        targetcellSelection('selectTarget', 'Target Cell'),
        radioButtons(
          "radioExplore",
          "Organism",
          inline = TRUE,
          choices = list(
            "Mouse" = "mouse",
            "Human" = "human",
            "Both" = "both"
          ),
          selected = "both"
        )
      ), 
      
      conditionalPanel('output.cond_exploreComp == "1"',
                       div(
                         startcellSelection("selectStart_infered", "Start Cell", startInputList_inf),
                         targetcellSelection('selectTarget_infered', 'Target Cell')
                       ),
                       checkboxGroupInput("checkGroupTools",
                                          h5("Tools"),
                                          inline = TRUE,
                                          choices = list("Mogrify" = "Mogrify",
                                                         "JSD"= "JSD",
                                                         "Taiji" = "Taiji",
                                                         #"IRENE" = "irene",
                                                         "CellNet" = "CellNet"
                                          ),
                                          selected = c("Mogrify"))
      )
      
    )
  )
  ,
  mainPanel(
    
    conditionalPanel(
      'output.cond_exploreExp == "1"',
      div(id = "tblExplore",
          uiOutput("header_shown_protocols"),
          DTOutput("dt_validated_protocols"),
          textOutput("info_validated_protocols")
          )
    ),
    
    conditionalPanel('output.cond_exploreComp == "1"',
                     div(
                       h4(align = "center", "Find and compare inferred TF sets"),
                       p(align = "center", "The collection contains:",  br(),
                         # "230 predictions from", a("Mogrify,", href="https://www.nature.com/articles/ng.3487"), br(),
                         # "225 from",  a("Alessio et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/"), "(Here refred as JSD),", br(),
                         # "25 from", a("Cellnet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/"), br(),
                         # "25k from", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166"), "(Arrow indicates up/down regualtion)"
                         "230 predictions from", a("Mogrify,", href="https://www.nature.com/articles/ng.3487"), 
                         "225 from",  a("Alessio et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/"), "(Here refred as JSD),", 
                         "25 from", a("Cellnet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/"), ", ",
                         "25k from", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166"), "(Arrow indicates up/down regualtion)"
                         )
                     ),
                     DTOutput("dt_inferred_protocols")
    )
    
  )
  ) 
)
