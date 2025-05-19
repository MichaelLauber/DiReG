tabPanel("About",
         h3("Application Information"),
         p("This Shiny application was developed by Michael Lauber under the supervision of Prof. Markus List at the", 
           a(href = "https://www.mls.ls.tum.de/en/daisybio/home/", target = "_blank", "Chair of Data Science in Systems Biology"), "@the Technical University of Munich."),
        
         h3("Source Code"),
         p("The source code for this application is available on",
           a(href = "https://github.com/MichaelLauber/DiReG_APP", target = "_blank", "GitHub"),
           ". You can access, fork, or contribute to the project via our repository."),
         
         h3("Support"),
         p("If you encounter any issues while using this application:"),
         tags$ul(style = "display: inline-block; text-align: left;",
                 tags$li("Please open an issue on our",
                         a(href = "https://github.com/MichaelLauber/DiReG_APP/issues", target = "_blank", "GitHub repository")),
                 tags$li("Or contact us directly via ",
                         a(href = "mailto:michaellauber05@gmail.com", "email"))
         ),
         
         p("Thank you for using our application!")
)