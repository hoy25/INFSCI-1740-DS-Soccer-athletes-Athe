library(shiny)

# Assuming ui.R and server.R are in the same directory as app.R
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)