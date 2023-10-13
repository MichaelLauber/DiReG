tabPanel("Home",
         div(id="homeBox",
             
             h1("Welcome to DiReG"),
             h3("DiReG was devoloped for wetlab and computational biologist working on directed reprogramming approaches", align = "center" ),
             
             #h3("Any (inferred) set of TF can be investigated in various ways"),
             img(src='DALLE.png', align = "center", class="img" ),
             # h4("Explore: Literature curated and predicted directed differentation protocols for various cell types are provided", align = "center" ),
             # h4("Signature Mining: Sets of predicted TFs can be investigated in various ways and 
             #    evalutated based on different metrics and compare them to other tools", align="center"),
             # h4("Discovery: Make a de novo prediction based on epigenome data", align="center")
             p(HTML("<b>Explore:</b> Literature curated and predicted directed differentiation protocols for various cell types are provided"), align = "center"),
             p(HTML("<b>Signature Mining:</b> Sets of predicted TFs can be investigated in various ways and evaluated based on different metrics and compare them to other tools"), align = "center"),
             p(HTML("<b>Discovery:</b> Make a de novo prediction based on epigenome data"), align = "center")
         )
)