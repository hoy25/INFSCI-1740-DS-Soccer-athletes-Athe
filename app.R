# app.R
library(shiny)

source("2global.R")
source("2ui.R")
source("2server.R")


shinyApp(ui = ui, server = server)
