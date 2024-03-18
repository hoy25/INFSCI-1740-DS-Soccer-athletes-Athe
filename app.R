# app.R
library(shiny)

source("2global.R")
source("2ui.R")
source("2server.R")

# 运行应用程序
shinyApp(ui = ui, server = server)