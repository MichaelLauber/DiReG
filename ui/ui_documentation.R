tabPanel("Documentation",
         sidebarLayout(
           sidebarPanel(
             column(12,
                    div(class="center-flex",
                        actionButton("actnBtnSignature", "Signature Mining", class = "actBtnsDoc"),
                        actionButton("actnBtnExplore", "Explore", class = "actBtnsDoc"),
                        actionButton("actnBtnDiscovery", "Discovery", class = "actBtnsDoc")
                    )
             ),
           ),
           mainPanel(
             uiOutput("sectionContent"),
             uiOutput("tabsetOutput")
           )
         )
)