#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(jsonlite)
library(ggplot2)
library(plotly)
library(lubridate)

# Define UI
ui <- fluidPage(
  titlePanel("Soccer Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Choose JSON File", accept = ".json"),
      selectInput("teamFilter", "Select Team", choices = c("All" = "", "Loading teams..." = NULL)),
      dateRangeInput("dateFilter", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("applyFilters", "Apply Filters")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Score Distribution", plotOutput("scoreDistribution")),
                  tabPanel("Performance Comparison", plotOutput("performanceComparison")),
                  tabPanel("Game Duration Over Time", plotOutput("gameDurationOverTime")),
                  tabPanel("Interactive Plot", plotlyOutput("interactivePlot")),
                  tabPanel("Scoring Probability Heatmap", plotOutput("scoringProbability"))
      )
    )
  )
)