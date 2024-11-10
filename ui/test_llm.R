tabPanel("Home",
         div(id="homeBox",
             
             titlePanel("Ollama Integration with Shiny"),
             
             sidebarLayout(
               sidebarPanel(
                 textInput("user_prompt", "Enter your prompt:", ""),
                 actionButton("submit_btn_prompt", "Submit")
               ),
               
               mainPanel(
                 h3("Response from Ollama:"),
                 verbatimTextOutput("ollama_response")
               )
             )
             )
)