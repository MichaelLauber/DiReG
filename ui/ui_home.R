tabPanel("Home",
         div(id="homeBox",
             
             style = "display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 10px; box-sizing: border-box; height: calc(100vh - 150px); overflow: auto; text-align: center;",
             
             h1("Welcome to DiReG"),
             h4("An App developed for Scientists working on Direct Reprogramming", align = "center", style = "font-weight: bold;; font-size: 2vw; margin: 0.5vw;" ),
             
             img(src='DALLE.png', align = "center", class="img", style="max-width: 23%; height: auto; margin: auto; display: block;"),
             
             p(HTML("<b>Explore:</b> Mine the scientific literature and ask specific questions to a state-of-the-art LLM with RAG"), 
               align = "center", 
               style = "font-size: 18px;"),
             
             p(HTML("<b>Signature Mining:</b> Use predictive models and explorative tools to identify sets of transcription factors and compare key metrics"), 
               align = "center", 
               style = "font-size: 18px;"),
             
             p(HTML("<b>Discovery:</b> Leverage epigenome data for de novo predictions"), 
               align = "center", 
               style = "font-size: 18px;")
         )
)