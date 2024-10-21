tabPanel("Home",
         div(id="homeBox",
             
             h1("Welcome to DiReG"),
             h3("DiReG was developed for wetlab and computational biologist working on directed reprogramming approaches", align = "center", style = "font-weight: bold;" ),
             
             #h3("Any (inferred) set of TF can be investigated in various ways"),
             img(src='DALLE.png', align = "center", class="img" ),
             # h4("Explore: Literature curated and predicted directed differentation protocols for various cell types are provided", align = "center" ),
             # h4("Signature Mining: Sets of predicted TFs can be investigated in various ways and 
             #    evalutated based on different metrics and compare them to other tools", align="center"),
             # h4("Discovery: Make a de novo prediction based on epigenome data", align="center")
             # p(HTML("<b>Explore:</b> Mine the scientific literature and ask specific questions to an state of the art LLM with RAG"), align = "center"),
             # p(HTML("<b>Signature Mining:</b> Use predictive models and explorative tools to identify sets of transcription factors and compare key metrics"), align = "center"),
             # p(HTML("<b>Discovery:</b> Leverage epigenome data for de novo predictions"), align = "center")
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