source("utils/utils_selectizeInput.R")
all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))
startInputList_inf <- createChoices(all_infered_protocols$Start)



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
        choices = c("Literature", 
                    "Computational"),
        justified = TRUE
      ),
      
      conditionalPanel(
        'output.cond_exploreExp == "1"',
        h4("Explore Experimental Reprogramming Literature via PaperQA or your Custom RAG"),
        p("PaperQA can be more accurate, while our Custom RAG Model is very cheap ( <1 cent per request) and faster"),
        
        # Add radio buttons to select between PaperQA and Custom RAG
        shinyWidgets::radioGroupButtons(
          inputId = "exploreMethodType",
          label = "Method",
          choices = c( "Custom RAG", "PaperQA"),
          justified = TRUE,
          selected = "Custom RAG"
        ),
        
        # Conditional panel for PaperQA
        conditionalPanel(
          'input.exploreMethodType == "PaperQA"',
          textAreaInput("user_prompt_explore", label = NULL,
                        placeholder = "Type your question",
                        width = "100%", height = "150px"),
          tags$style(HTML("
            #user_prompt_explore {
                font-size: 1em;
                line-height: 1.5em;
                white-space: pre-wrap;
            }
          ")),
          radioButtons("explore_mode", titleWithPopover("Select Mode", "Mode", 
                                                        "Fast: Answers cheaply and quickly; High Quality: Highly performant but relatively expensive and slow"),
                       choices = list("Fast" = "fast", "High Quality" = "high_quality"), 
                       selected = "fast", inline = TRUE),
          sliderInput(
            "explore_temp",
            titleWithPopover("Temperature", "Controls the randomness", "High values lead to more creative but less deterministic and focused answers"),
            min = 0,
            max = 1,
            value = 0.5,
            ticks = T,
            width = "150px"
          ),
          actionButton("explore_prompt_btn", "Ask"),
          actionButton("explore_example_btn", "Example")
        ),
        
        # Conditional panel for Custom RAG
        conditionalPanel(
          'input.exploreMethodType == "Custom RAG"',
          textAreaInput("user_prompt_explore_rag", label = NULL,
                        placeholder = "Type your question (Custom RAG is faster but less accurate)",
                        width = "100%", height = "150px"),
          tags$style(HTML("
            #user_prompt_explore_rag {
                font-size: 1em;
                line-height: 1.5em;
                white-space: pre-wrap;
            }
          ")),
          actionButton("explore_prompt_rag_btn", "Ask"),
          actionButton("explore_example_rag_btn", "Example")
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
                                                         "IRENE" = "IRENE",
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
      div(
        verbatimTextOutput("api_response_output")
      )
    ),
    
    conditionalPanel('output.cond_exploreComp == "1"',
                     div(
                       h4(align = "center", "Find and compare inferred TF sets"),
                       p(align = "center", "The collection contains:",  br(),
                         "230 predictions from", a("Mogrify,", href="https://www.nature.com/articles/ng.3487"), 
                         "225 from",  a("Alessio et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/"), "(Here refred as JSD),",
                         "317 from",  a("IRENE", href="https://www.nature.com/articles/s41467-021-21801-4"), ", ",
                         "25 from", a("Cellnet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/"), ", ",
                         "25k from", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166"), "(Arrow indicates up/down regualtion)"
                       )
                     ),
                     DTOutput("dt_inferred_protocols")
    )
    
  )
  ) 
)
