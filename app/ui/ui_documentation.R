tabPanel("Documentation",
         sidebarLayout(
           sidebarPanel(
             column(12,
                    div(class="center-flex",
                        actionButton("actnBtnExplore", "Explore", class = "actBtnsDoc"),
                        actionButton("actnBtnSignature", "Analyze TF Sets", class = "actBtnsDoc"),
                        actionButton("actnBtnDiscovery", "Discover New TFs", class = "actBtnsDoc")
                    )
             ),
           ),
           mainPanel(
             conditionalPanel(
               condition = "output.showWelcome",
               div(id = "doc-welcome", style = "padding: 20px; text-align: center;",
                   h2("DiReG Documentation", style = "color: #2c3e50; margin-bottom: 20px;"),
                   
                   div(style = "max-width: 600px; margin: 0 auto; text-align: left;",
                       p("Welcome to the DiReG documentation! Here you'll find detailed explanations of all features and methodologies used throughout the application.", 
                         style = "font-size: 16px; line-height: 1.6; margin-bottom: 20px;"),
                       
                       h4("What you'll find here:", style = "color: #2c3e50; margin-bottom: 15px;"),
                       tags$ul(
                         tags$li(strong("Explore:"), " Understand literature search capabilities and computational prediction tools"),
                         tags$li(strong("Analyze TF Sets:"), " Learn about TF activity analysis, pathway enrichment, expression patterns, and more"),
                         tags$li(strong("Discover New TFs:"), " Get details on ATAC-seq analysis and motif discovery methods"),
                         style = "font-size: 14px; line-height: 1.8; margin-bottom: 20px;"
                       )
                   )
               )
             ),
             
             uiOutput("sectionContent"),
             uiOutput("tabsetOutput")
           )
         )
)