# This script is used to run the application defined in app.R in the background
options(shiny.autoreload = TRUE)
shiny::runApp(port = 5515)

#  rstudioapi::translateLocalUrl("http://127.0.0.1:5515", T)