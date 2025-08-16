source("utils/utils_selectizeInput.R")
all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))
startInputList_inf <- createChoices(all_infered_protocols$Start)



tabPanel(
  "Explore",
  div(
    id = "exploreHeader",
    h2(class = "center", "Find experimental validated and inferred Direct Reprogramming Protocols"),
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
                       
                       checkboxGroupInput("checkGroupTools",
                                          h5("Tools"),
                                          inline = TRUE,
                                          choices = list("Mogrify" = "Mogrify",
                                                         "JSD"= "JSD",
                                                         "Taiji" = "Taiji",
                                                         "IRENE" = "IRENE",
                                                         "CellNet" = "CellNet"
                                          ),
                                          selected = c("Mogrify")),
                       div(
                         startcellSelection("selectStart_infered", "Start Cell", startInputList_inf),
                         targetcellSelection('selectTarget_infered', 'Target Cell')
                       )
      )
      
    )
  )
  ,
  mainPanel(
    
    conditionalPanel(
      'output.cond_exploreExp == "1"',
      
      conditionalPanel(
        'output.show_explore_explanation == true',
        div(
          id = "explore_explanation",
          class = "well",
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 20px; margin-bottom: 20px;",
          
          h4("Welcome to Literature Exploration", style = "color: #495057; margin-bottom: 15px;"),
          
          p(strong("What is PaperQA?"), 
            "An AI-powered research assistant that searches through 360+ scientific papers on direct reprogramming. 
          It provides accurate, citation-backed answers by analyzing full-text articles.",
            style = "margin-bottom: 10px;"),
          
          p(strong("What is Custom RAG?"), 
            "RAG (Retrieval-Augmented Generation) is our faster alternative that searches the same paper collection 
          but trades some accuracy for speed. Perfect for quick lookups.",
            style = "margin-bottom: 10px;"),
          
          div(
            style = "background-color: #e9ecef; padding: 15px; border-radius: 5px; margin: 15px 0;",
            p(icon("info-circle"), strong(" Cost Information:"), style = "margin-bottom: 5px;"),
            tags$ul(
              style = "margin-bottom: 0;",
              tags$li(strong("PaperQA:"), " Uses your OpenAI API key. Costs ~$0.05-0.20 per query depending on mode"),
              tags$li(strong("Custom RAG:"), " Also uses your API key but costs <$0.01 per query")
            )
          ),
          
          p(em("This message will disappear after your first question."), 
            style = "color: #6c757d; font-size: 0.9em; margin-top: 10px; margin-bottom: 0;")
        )
      ),
      
      div(
        style = "background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 8px; padding: 25px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
        uiOutput("api_response_output")
      )
    ),
    
    conditionalPanel('output.cond_exploreComp == "1"',
                     
                                      div(
                                        # Header section
                                        div(
                                          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                                          h4(align = "center", icon("dna"), "Explore computationally predicted transcription factor sets from leading algorithms")
                                        ),
                                        
                                        # Info boxes for each tool
                                        div(
                                          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-bottom: 25px;",
                                          
                                          # Mogrify box
                                          div(
                                            class = "well",
                                            style = "background-color: #f8f9fa; border-left: 4px solid #667eea; padding: 20px; margin: 0; height: 100%;",
                                            h5(icon("microscope"), " ", a("Mogrify", href="https://www.nature.com/articles/ng.3487", target="_blank", 
                                                                          style = "color: #667eea; text-decoration: none;"), 
                                               style = "margin-top: 0; color: #495057;"),
                                            p(strong("230 predictions"), style = "margin-bottom: 5px; color: #667eea;"),
                                            p("Network-based approach using gene expression and protein interactions", 
                                              style = "font-size: 0.9em; color: #6c757d; margin-bottom: 0;")
                                          ),
                                          
                                          # JSD/Alessio box
                                          div(
                                            class = "well",
                                            style = "background-color: #f8f9fa; border-left: 4px solid #764ba2; padding: 20px; margin: 0; height: 100%;",
                                            h5(icon("chart-bar"), " ", a("JSD (Alessio et al.)", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/", 
                                                                         target="_blank", style = "color: #764ba2; text-decoration: none;"), 
                                               style = "margin-top: 0; color: #495057;"),
                                            p(strong("225 predictions"), style = "margin-bottom: 5px; color: #764ba2;"),
                                            p("Core TF classification based on expression patterns", 
                                              style = "font-size: 0.9em; color: #6c757d; margin-bottom: 0;")
                                          ),
                                          
                                          # IRENE box
                                          div(
                                            class = "well",
                                            style = "background-color: #f8f9fa; border-left: 4px solid #06b6d4; padding: 20px; margin: 0; height: 100%;",
                                            h5(icon("project-diagram"), " ", a("IRENE", href="https://www.nature.com/articles/s41467-021-21801-4", 
                                                                               target="_blank", style = "color: #06b6d4; text-decoration: none;"), 
                                               style = "margin-top: 0; color: #495057;"),
                                            p(strong("317 predictions"), style = "margin-bottom: 5px; color: #06b6d4;"),
                                            p("Integrative approach with epigenomic and PPI data", 
                                              style = "font-size: 0.9em; color: #6c757d; margin-bottom: 0;")
                                          ),
                                          
                                          # CellNet box
                                          div(
                                            class = "well",
                                            style = "background-color: #f8f9fa; border-left: 4px solid #10b981; padding: 20px; margin: 0; height: 100%;",
                                            h5(icon("network-wired"), " ", a("CellNet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/", 
                                                                             target="_blank", style = "color: #10b981; text-decoration: none;"), 
                                               style = "margin-top: 0; color: #495057;"),
                                            p(strong("25 predictions"), style = "margin-bottom: 5px; color: #10b981;"),
                                            p("Network biology framework for cell type similarity", 
                                              style = "font-size: 0.9em; color: #6c757d; margin-bottom: 0;")
                                          ),
                                          
                                          # Taiji box
                                          div(
                                            class = "well",
                                            style = "background-color: #f8f9fa; border-left: 4px solid #f59e0b; padding: 20px; margin: 0; height: 100%;",
                                            h5(icon("sitemap"), " ", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166", 
                                                                       target="_blank", style = "color: #f59e0b; text-decoration: none;"), 
                                               style = "margin-top: 0; color: #495057;"),
                                            p(strong("25,000+ predictions"), style = "margin-bottom: 5px; color: #f59e0b;"),
                                            p("GRN-based with up/down regulation indicators (↑↓)", 
                                              style = "font-size: 0.9em; color: #6c757d; margin-bottom: 0;")
                                          )
                                        ),
                                        
                                        # Instructions box
                                        div(
                                          style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 8px; padding: 20px; margin-bottom: 20px;",
                                          h5(icon("info-circle"), " How to Use", style = "color: #0066cc; margin-top: 0;"),
                                          tags$ol(
                                            style = "margin-bottom: 0; padding-left: 20px;",
                                            tags$li("Select one or more computational tools using the checkboxes above"),
                                            tags$li("Choose your starting cell type from the dropdown"),
                                            tags$li("Select your target cell type"),
                                            tags$li("Browse the predicted TF sets in the table below"),
                                            tags$li(strong("Click any TF set"), " to load it into the Signature Mining module for further analysis")
                                          )
                                        ),
                                        
                                        # Additional info box
                                        div(
                                          style = "background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                                          p(icon("lightbulb"), strong("Tip:"), " Predictions from multiple tools for the same conversion often differ. 
        Consider testing TF sets from different algorithms or combining their insights for better results.",
                                            style = "margin-bottom: 0; color: #856404;")
                                        )
                                      ),
                                      
                     DTOutput("dt_inferred_protocols")
    )
    
  )
  ) 
)
