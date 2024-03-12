#library(shiny)
#library(ggplot2)
#library(jsonlite)
#library(dplyr)
#library(plotly)
#library(leaflet)

function(input, output, session) {
  
  # Data upload and preprocess
  processed_data <- reactive({
    req(input$dataFile)
    data <- read_json(input$dataFile$datapath, simplifyVector = TRUE)
    return(data)
  })
  
  # Visualization 1: Distribution of Scores with Facet Wrap by Team
  output$scoreDistribution <- renderPlot({
    data <- processed_data()
    ggplot(data, aes(x = score)) +
      geom_histogram(fill = "blue", bins = 30) +
      facet_wrap(~team) +
      labs(title = "Score Distribution by Team", x = "Score", y = "Count") +
      theme_minimal()
  })
  
  # Visualization 2: Performance Metrics Comparison
  output$performanceComparison <- renderPlot({
    data <- processed_data()
    ggplot(data, aes(x = metric1, y = metric2, color = team)) +
      geom_point() +
      labs(title = "Comparison of Performance Metrics", x = "Metric 1", y = "Metric 2") +
      theme_light()
  })
  
  # Visualization 3: Game Duration Over Time (Time Series)
  output$gameDurationOverTime <- renderPlot({
    data <- processed_data()
    ggplot(data, aes(x = game_date, y = game_duration)) +
      geom_line() +
      labs(title = "Game Duration Over Time", x = "Date", y = "Duration (minutes)") +
      theme_bw()
  })
  
  # Visualization 4: Interactive Plot with Plotly (e.g., Scores Over Time)
  output$interactivePlot <- renderPlotly({
    data <- processed_data()
    plot_ly(data, x = ~game_date, y = ~score, type = 'scatter', mode = 'lines+markers',
            color = ~team, marker = list(size = 10)) %>%
      layout(title = "Scores Over Time by Team")
  })
  
  # Visualization 5: Interactive Time Series Analysis
  output$timeSeriesPlot <- renderPlot({
    variable <- input$timeSeriesVariable
    data <- my_json() %>% select(time = matchId, !!variable) %>% na.omit()
    ggplot(data, aes(x = time, y = !!sym(variable))) +
      geom_line() +
      geom_smooth(method = "loess", color = "red") +
      labs(title = paste("Time Series of", variable))
  })
  
  # Visualization 6: Heatmap for Correlation Analysis
  output$correlationHeatmap <- renderPlot({
    variables <- input$correlationVariables
    data <- my_json() %>% select(all_of(variables)) %>% na.omit()
    correlation_matrix <- cor(data)
    ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      labs(title = "Correlation Heatmap")
  })
  
}