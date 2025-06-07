tabPanel("Home",
         div(id="homeBox",
             
             style = "display: flex; flex-direction: column; align-items: center; padding: 20px; box-sizing: border-box; min-height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden; text-align: center;",
             
             h1("Welcome to DiReG", style = "font-weight: bold; font-size: 2.5vw; margin: 0.5vw; color: #2c3e50;"),
             h3("The Direct Reprogramming Guide", align = "center", style = "font-weight: normal; font-size: 1.5vw; margin: 0.5vw; color: #7f8c8d;"),
             
             # Biological context section
             div(style = "max-width: 1000px; margin: 20px auto; display: flex; align-items: center; gap: 30px; flex-wrap: wrap;",
                 
                 # Text content
                 div(style = "flex: 1; min-width: 400px; padding: 20px; background-color: #f8f9fa; border-radius: 10px; border-left: 4px solid #3498db;",
                     h4("What is Direct Reprogramming?", style = "color: #2c3e50; margin-bottom: 15px;"),
                     p("Direct reprogramming allows scientists to convert one cell type directly into another (e.g., skin cells into neurons) by introducing specific transcription factors. This breakthrough technology has enormous potential for regenerative medicine, disease modeling, and drug discovery.", 
                       style = "font-size: 16px; line-height: 1.6; color: #34495e; margin-bottom: 10px;"),
                     p("Identifying the right combination of transcription factors is a complex and time-consuming challenge. 
          DiReG is an app developed to simplify the process of identifying and validating transcription factor combinations.", 
                       style = "font-size: 16px; line-height: 1.6; color: #34495e;")
                 ),
                 
                 # Image
                 div(style = "flex: 0 0 300px; text-align: center;",
                     img(src='avocado2pumpkin.png', style="max-width: 100%; height: auto; border-radius: 10px; box-shadow: rgb(38, 57, 77) 0px 10px 20px -5px;")
                 )
             ),
             
             # What DiReG does
             div(style = "max-width: 1200px; margin: 20px auto;",
                 h4("How DiReG Helps You", style = "color: #2c3e50; margin-bottom: 20px;"),
                 
                 # Container for all boxes
                 div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; align-items: stretch;",
                     
                     # Three main use cases
                     div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; flex: 3; min-width: 600px;",
                         
                         # Explore existing knowledge
                         div(style = "flex: 1; min-width: 200px; max-width: 250px; padding: 20px; background-color: #e8f5e8; border-radius: 10px; border: 1px solid #27ae60;",
                             div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                 icon("search", style = "color: #27ae60; margin-right: 10px; font-size: 20px;"),
                                 h5("Explore", style = "color: #27ae60; margin: 0;")
                             ),
                             p("Search through 360+ reprogramming studies using AI-powered literature analysis. Find existing protocols and compare different approaches.", 
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50;"),
                             p("Compare TF predictions for your cell transition of interest from multiple specialized computational methods.", 
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50;")
                         ),
                         
                         # Analyze your factors
                         div(style = "flex: 1; min-width: 200px; max-width: 250px; padding: 20px; background-color: #fff2e8; border-radius: 10px; border: 1px solid #e67e22;",
                             div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                 icon("chart-line", style = "color: #e67e22; margin-right: 10px; font-size: 20px;"),
                                 h5("Analyze TF Sets", style = "color: #e67e22; margin: 0;")
                             ),
                             p("Input your transcription factors and analyze their expression patterns, interactions, pathway enrichment, and reprogramming potential across different cell types.", 
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50;")
                         ),
                         
                         # Discover new factors
                         div(style = "flex: 1; min-width: 200px; max-width: 250px; padding: 20px; background-color: #e8f4fd; border-radius: 10px; border: 1px solid #3498db;",
                             div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                 icon("microscope", style = "color: #3498db; margin-right: 10px; font-size: 20px;"),
                                 h5("Discover New TFs", style = "color: #3498db; margin: 0;")
                             ),
                             p("Upload your ATAC-seq data to identify novel transcription factor candidates using computational motif discovery methods.", 
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50;")
                         )
                     ),
                     
                     # Vertical separator
                     div(style = "width: 3px; background: linear-gradient(to bottom, transparent, #bdc3c7, transparent); margin: 0 10px; align-self: stretch; min-height: 200px;"),
                     
                     # Right side boxes
                     div(style = "display: flex; flex-direction: row; gap: 20px; flex: 1; min-width: 400px;",
                         
                         # Quick Start box
                         div(style = "padding: 20px; background-color: #f0f9ff; border-radius: 10px; border: 1px solid #60a5fa; border-left: 4px solid #60a5fa;",
                             div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                 icon("play", style = "color: #60a5fa; margin-right: 10px; font-size: 20px;"),
                                 h5("Quick Start", style = "color: #60a5fa; margin: 0;")
                             ),
                             p("New to direct reprogramming? Start with ", 
                               strong("Explore"), " to browse existing protocols, then use ", 
                               strong("Signature Mining"), " to analyze your factors of interest.",
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50; margin-bottom: 15px;"),
                             actionButton("goto_explore", "Start Exploring", 
                                          style = "background-color: #60a5fa; color: white; border: none; padding: 8px 16px; border-radius: 5px; font-weight: bold; width: 100%;",
                                          onclick = "document.getElementById('menu').querySelector('a[data-value=\"Explore\"]').click();")
                         ),
                         
                         # Resources box
                         div(style = "padding: 20px; background-color: #faf5ff; border-radius: 10px; border: 1px solid #a855f7; border-left: 4px solid #a855f7;",
                             div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                 icon("book", style = "color: #a855f7; margin-right: 10px; font-size: 20px;"),
                                 h5("Resources", style = "color: #a855f7; margin: 0;")
                             ),
                             p("Need detailed explanations or found a bug? Access comprehensive documentation or report issues to help improve DiReG.",
                               style = "font-size: 14px; line-height: 1.5; color: #2c3e50; margin-bottom: 15px;"),
                             div(style = "display: flex; gap: 10px; flex-direction: column;",
                                 actionButton("goto_docs", "Documentation", 
                                              style = "background-color: #a855f7; color: white; border: none; padding: 6px 12px; border-radius: 5px; font-weight: bold; width: 100%;",
                                              onclick = "document.getElementById('menu').querySelector('a[data-value=\"Documentation\"]').click();"),
                                 tags$a(href = "https://github.com/MichaelLauber/DiReG/issues", target = "_blank", style = "width: 100%;",
                                        tags$button("Report Issues", 
                                                    style = "background-color: #64748b; color: white; border: none; padding: 6px 12px; border-radius: 5px; font-weight: bold; width: 100%;"))
                             )
                         )
                     )
                 )
             )
         )
)