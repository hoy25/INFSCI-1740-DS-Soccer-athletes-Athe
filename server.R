library(shiny)
library(jsonlite)
library(ggplot2)
library(dplyr)

options(shiny.maxRequestSize = 10*1024^2)

server <- function(input, output, session) {
  processed_data <- reactive({
    req(input$dataFile)
    data <- jsonlite::fromJSON(input$dataFile$datapath, simplifyDataFrame = TRUE)
    
    # Processing data: extract x, y, and outcome (if available)
    shots_data <- data.frame(
      x = sapply(data, function(record) record$location$x),
      y = sapply(data, function(record) record$location$y),
      outcome = sapply(data, function(record) record$shot$isGoal)
    )
    
    # Remove rows with NAs in the outcome
    valid_shots_data <- shots_data[!is.na(shots_data$outcome), ]
    
    return(valid_shots_data)
  })
  
  output$scoringProbability <- renderPlot({
    data <- processed_data()
    
    # Assuming the binning was successful and no NAs are introduced in x_bins and y_bins
    # If you haven't already, add checks here to confirm that x_bins and y_bins do not contain NAs
    
    scoring_data <- data %>%
      # Create bins for x and y coordinates
      mutate(
        x_bin = cut(x, breaks=seq(0, 100, by=10), include.lowest = TRUE),
        y_bin = cut(y, breaks=seq(0, 100, by=10), include.lowest = TRUE)
      ) %>%
      # Remove rows with NA in x_bin or y_bin
      na.omit() %>%
      group_by(x_bin, y_bin) %>%
      summarise(scoring_prob = mean(outcome, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(scoring_prob)) # Remove bins with NA scoring probability
    
    # Check if scoring_data is empty or has NA values
    if(nrow(scoring_data) == 0 || any(is.na(scoring_data$scoring_prob))) {
      print("No data to plot or NA values present in scoring_prob.")
      return(NULL)
    }
    
    # Plot the heatmap
    ggplot(scoring_data, aes(x = x_bin, y = y_bin, fill = scoring_prob)) +
      geom_tile() +
      scale_fill_gradient(low = "blue", high = "red", name = "Scoring Probability") +
      labs(title = "Scoring Probability Heatmap", x = "Field Width", y = "Field Length") +
      theme_minimal() +
      coord_fixed(ratio = 1)
  })
}