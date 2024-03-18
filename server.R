function(input, output, session) {
  
  ### -- ### UPLOAD JSON DATA PANEL ### -- ###
  
  # process user upload, or return NULL if nothing is uploaded
  my_json <- reactive({
    infile <- input$user_file
    
    if( is.null(infile) ){
      return( NULL )
    }
    
    # read in the user specified JSON file and flatten nested data frames
    # we may need to change this later, we'll see
    jsonlite::fromJSON( infile$datapath, flatten = TRUE)
  })
  
  my_json_un <- reactive({
    infile <- input$user_file
    
    if( is.null(infile) ){
      return( NULL )
    }
    
    jsonlite::fromJSON( infile$datapath)
  })
  
  
  ### create a reactive that stores the number of games
  number_of_games <- reactive({
    my_json() %>% length()
  })
  
  # reactive for list of match ids
  matchIds <- reactive({
    sapply(my_json(),
           function(one_game) one_game %>% select(matchId) %>% pluck(1, 1)
    )
  })
  
  # get team names from all games
  team_names <- reactive({
    sapply(my_json(), 
           function(one_game) one_game %>% select(team.name) %>% unique
    )
  })

  # reactive for the currently chosen game index
  # this is chosen in the variable details tab, or is 1 by default
  selected_game_index <- reactive({
    ifelse(str_equal(input$one_game_selector, ''),
           1,
           match(input$one_game_selector, matchIds()) # this is the R match function. Confusing naming...
    )
    
  })
  
  # reactive for currently chosen game
  selected_game <- reactive({
    get_game(selected_game_index(), my_json())
  })
  
  # reactive for currently chosen game dictionary
  selected_game_dict <- reactive({
    get_game_dict(selected_game_index(), my_json())
  })
  
  # reactive for the currently chosen game index in the BAR CHARTS panel
  # this is chosen in Bar Charts -> Counts - 1 Game, or is 1 by default
  selected_game_index_bar_charts <- reactive({
    ifelse(str_equal(input$bar_charts_one_game_selector, ''),
           1,
           match(input$bar_charts_one_game_selector, matchIds()) # this is the R match function. Confusing naming...
    )
    
  })
  
  # reactive for the currently chosing multiple games in the BAR CHARTS panel
  # this is chosen in Bar Charts -> Counts - Multiple Games, or is 1 by default
  selected_games_index_bar_charts <- reactive({
    selected_games <- input$bar_charts_multiple_game_selector
    if (length(selected_games) == 0) {
      return(1)
    } else {
      match(selected_games, matchIds())
    }
    
  })
  
  # reactive for currently chosen game in the BAR CHARTS panel
  selected_game_bar_charts <- reactive({
    get_game(selected_game_index_bar_charts(), my_json())
  })
  
  # reactive for currently chosen game dictionary in the BAR CHARTS panel
  selected_game_dict_bar_charts <- reactive({
    get_game_dict(selected_game_index_bar_charts(), my_json())
  })
  
  # reactive for currently chosen games in the BAR CHARTS panel
  selected_games_bar_charts <- reactive({
    my_json() %>% bind_rows() %>%
      filter(matchId %in% input$bar_charts_multiple_game_selector)
  })
  
  # reactive for the currently chosen game index in the HISTOGRAMS panel
  # this is chosen in Histograms -> Histogram - 1 Game, or is 1 by default
  selected_game_index_histograms <- reactive({
    ifelse(str_equal(input$histograms_one_game_selector, ''),
           1,
           match(input$histograms_one_game_selector, matchIds()) # this is the R match function. Confusing naming...
    )
    
  })
  
  # reactive for the currently chosen games index in the HISTOGRAMS multiple games panel
  selected_games_histogram_charts <- reactive({
    my_json() %>% bind_rows() %>%
      filter(matchId %in% input$histograms_multiple_game_selector)
  })
  
  # reactive for currently chosen game in the HISTOGRAMS panel
  selected_game_histograms <- reactive({
    get_game(selected_game_index_histograms(), my_json())
  })
  
  # reactive for currently chosen game dictionary in the HISTOGRAMS panel
  selected_game_dict_histograms <- reactive({
    get_game_dict(selected_game_index_histograms(), my_json())
  })
  
  # reactive for the currently chosen game index in the LIST VARS panel
  # this is chosen in the List Vars sidebar, or is 1 by default
  selected_game_index_list_vars <- reactive({
    ifelse(str_equal(input$list_vars_one_game_selector, ''),
           1,
           match(input$list_vars_one_game_selector, matchIds()) # this is the R match function. Confusing naming...
    )
    
  })
  
  # reactive for currently chosen game in the LIST VARS panel
  selected_game_list_vars <- reactive({
    get_game(selected_game_index_list_vars(), my_json())
  })
  
  # reactive for currently chosen game dictionary in the LIST VARS panel
  selected_game_dict_list_vars <- reactive({
    get_game_dict(selected_game_index_list_vars(), my_json())
  })
  
  
  number_of_variables <- reactive({
    get_game(selected_game_index(), my_json())%>% ncol()
  })
  ### number of variable groups
  number_of_variables_group <- reactive({
    nrow(selected_game_dict() %>% count(variable_group))
  })
  
  number_of_variables_per_group <- reactive({
    my_json() %>% map( function(xdf){ xdf %>% 
        keep(is.data.frame) %>% 
        map_dbl( ncol )} )
  })
  
  
  
  ### define functions that act on OR use the dynamically 
  ### defined and uploaded data
  
  ### --- Games in upload tab ---
  
  # number of games
  output$num_games <- renderText({
    if( is.null(my_json()) ){
      return('No file has been uploaded')
    } else {
      return(number_of_games())
    }
  })
  
  # number of unique teams
  output$num_unique_teams <- renderText({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      
      # remove duplicates
      unique_teams <- team_names() %>% unlist %>% unique
      return(length(unique_teams))
    }
  })
  
  # number of events per game
  output$num_events_per_game <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      # get num events and match id from all games
      num_events <- sapply(my_json(), 
                           function(game) c(
                             nrow(game), 
                             game %>% select(matchId) %>% pluck(1, 1)
                           ))
      # reshape and add col names
      num_events <- t(num_events)
      colnames(num_events) <- c("num_events", "matchId")
      num_events <- num_events %>% as_tibble
      
      # plot
      return(
        num_events %>% ggplot(aes(x=factor(matchId), y=num_events)) +
          geom_col() +
          coord_flip() +
          labs(title = "Number of events per match") +
          xlab("Match ID") +
          ylab("Number of Events")
      )
    }
  })
  
  # teams for each game
  output$team_names_per_game <- renderTable({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      # get team names from all games
      team_names <- sapply(my_json(), 
                           function(game) game %>% select(team.name) %>% unique)
      
      # transform team_names into a clean tibble
      team_names_tibble <- suppressMessages( team_names %>% as_tibble(.name_repair = "unique") %>% t ) # quiet "new names" message
      rownames(team_names_tibble) <- 1:nrow(team_names_tibble)
      colnames(team_names_tibble) <- c("Team 1", "Team 2")
      team_names_tibble <- team_names_tibble %>% as_tibble
      # add match IDs
      team_names_tibble <- team_names_tibble %>% mutate(
        `Match ID` = matchIds(), .before = `Team 1`
      )
      
      return(team_names_tibble)
    }
  })
  
  output$number_of_variables <- renderPrint({
    if( is.null(input$user_file) ){
      return('No file has been uploaded')
    } else {
      return( number_of_variables() )
    }
  })
  
  output$number_of_variables_dataType <- renderPlot({
    selected_game_dict() %>% ggplot(mapping = aes(x = data_type)) + geom_bar() +theme_bw()
  })
  
  output$number_of_variables_group <- renderPrint({
    if( is.null(input$user_file) ){
      return('No file has been uploaded')
    } else {
      return( number_of_variables_group() )
    }
  })
  
  output$number_of_variables_per_group <- renderPlot({
    
    
    selected_game_dict() %>% ggplot(mapping = aes(x =variable_group)) + geom_bar() +theme_bw()
  })
  
  output$number_of_variables_per_group_dataType <- renderPlot({
    data <- selected_game_dict() %>% 
      group_by(variable_group) %>% 
      summarise(num_variables = n_distinct(flattened_variable_name),
                num_unique_datatypes = n_distinct(data_type),
                .groups = 'drop')
    
    ggplot(data,aes(x =variable_group,y = num_unique_datatypes)) + geom_bar(stat= "identity") +theme_bw()
  })
  
  ### -- ### Variable details PANEL ### -- ###
  
  # update sidebar dropdowns to have accurate choices
  
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'variable_group_selector', 
                        choices = selected_game_dict() %>% select(variable_group) %>% unique)
    }
  }) %>% bindEvent(input$one_game_selector)
  
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'one_game_selector', 
                        choices = matchIds())
    }
  }) %>% bindEvent(input$user_file)
  
  ### --- Unique Values tab --- 
  
  output$unique_values_per_variable_chart <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      var_group <- input$variable_group_selector
      variables_in_group <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$variable_group == var_group]
      
      data_in_var_group <- selected_game() %>% select(all_of(variables_in_group))
      
      num_unique_per_var <- sapply(data_in_var_group, function(col) length(unique(col)))
      
      if (input$variable_details_show_proportions) {
        num_unique_per_var <- num_unique_per_var / nrow(data_in_var_group)
      }
      
      num_unique_per_var <- data.frame(variable = names(num_unique_per_var), 
                                       num_unique=num_unique_per_var, 
                                       data_type=selected_game_dict()$data_type[selected_game_dict()$variable_group == var_group])
      
      g <- num_unique_per_var %>% ggplot() +
        geom_col(aes(x = variable, y = num_unique))
      
      if (input$top_bar_chart_orientation == "Horizontal") {
        g <- g + coord_flip()
      }
      if (input$unique_values_facet_by_data_type == "Yes") {
        g <- g + facet_wrap(~data_type, scales="free") # maybe should be free_x only?
      }
      
      return(g)
    }
  })
  
  # update dropdown to have choices from variable group
  
  observe({
    if (!is.null(my_json())) {
      var_group <- input$variable_group_selector
      variables_in_group <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$variable_group == var_group]
      updateSelectInput(session, 'unique_values_1_var_in_group', 
                        choices = variables_in_group)
      
      
    }
  }) %>% bindEvent(input$variable_group_selector, input$one_game_selector)
  
  output$unique_values_for_1_variable_chart <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$unique_values_1_var_in_group
      num_unique <- sapply(my_json(), function(game) game %>% select(all_of(one_var)) %>% n_distinct)
      
      if (input$variable_details_show_proportions) {
        total_entries <- sapply(my_json(), function(game) nrow(game))
        num_unique <- num_unique / total_entries
      }
      
      combined_data = data.frame(num_unique=num_unique, matchId=matchIds())
      
      g <- combined_data %>% ggplot() +
        geom_col(aes(x = factor(matchId), y = num_unique, fill = (matchId == input$one_game_selector))) +
        scale_fill_manual(values = c("grey", "green"))
      
      g$labels$fill <- "Selected Game"
      
      if (input$bottom_bar_chart_orientation == "Horizontal") {
        g <- g + coord_flip()
      }
      
      return(g)
    }
    
  })
  
  ### --- Missing Values tab --- 
  
  output$missing_values_per_variable_chart <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      var_group <- input$variable_group_selector
      variables_in_group <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$variable_group == var_group]
      
      data_in_var_group <- selected_game() %>% select(all_of(variables_in_group))
      
      num_missing_per_var <- sapply(data_in_var_group, function(col) sum(is.na(col)))
      
      if (input$variable_details_show_proportions) {
        num_missing_per_var <- num_missing_per_var / nrow(data_in_var_group)
      }
      
      num_missing_per_var <- data.frame(variable = names(num_missing_per_var), 
                                        num_missing=num_missing_per_var, 
                                        data_type=selected_game_dict()$data_type[selected_game_dict()$variable_group == var_group])
      
      g <- num_missing_per_var %>% ggplot() +
        geom_col(aes(x = variable, y = num_missing))
      
      if (input$top_bar_chart_orientation == "Horizontal") {
        g <- g + coord_flip()
      }
      if (input$missing_values_facet_by_data_type == "Yes") {
        g <- g + facet_wrap(~data_type, scales="free") # maybe should be free_x only?
      }
      
      return(g)
    }
  })
  
  # update dropdown to have choices from variable group
  
  observe({
    if (!is.null(my_json())) {
      var_group <- input$variable_group_selector
      variables_in_group <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$variable_group == var_group]
      updateSelectInput(session, 'missing_values_1_var_in_group', 
                        choices = variables_in_group)
      
      
    }
  }) %>% bindEvent(input$variable_group_selector, input$one_game_selector)
  
  output$missing_values_for_1_variable_chart <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$missing_values_1_var_in_group
      num_missing <- sapply(my_json(), function(game) game %>% select(all_of(one_var)) %>% is.na %>% sum)
      
      if (input$variable_details_show_proportions) {
        total_entries <- sapply(my_json(), function(game) nrow(game))
        num_missing <- num_missing / total_entries
      }
      
      combined_data = data.frame(num_missing=num_missing, matchId=matchIds())
      
      g <- combined_data %>% ggplot() +
        geom_col(aes(x = factor(matchId), y = num_missing, fill = (matchId == input$one_game_selector))) +
        scale_fill_manual(values = c("grey", "green"))
      
      g$labels$fill <- "Selected Game"
      
      if (input$bottom_bar_chart_orientation == "Horizontal") {
        g <- g + coord_flip()
      }
      
      return(g)
    }
    
  })
  
  ### --- Data dictionary --- 
  
  output$data_dict<- renderTable({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      list <- selected_game()
      
      dict <-selected_game_dict() %>%
        filter(data_type == input$data_type_selector) %>%
        select(-data_type,-variable_name)
      
      dict <- dict %>%
        mutate(
          num_of_uniq = sapply(flattened_variable_name, function(col_name){ length(unique(list[[col_name]]))}),
          num_of_missing = sapply(flattened_variable_name, function(col_name){  sum(is.na(list[[col_name]]))}),
          prop_uniq =  sapply(flattened_variable_name, function(col_name){ length(unique(list[[col_name]]))/length(list[[col_name]])}),
          prop_missing = sapply(flattened_variable_name, function(col_name){  sum(is.na(list[[col_name]]))/ length(list[[col_name]])})
        )
      
      
      if(input$data_type_selector == "character")
      {
        dict <- dict %>%
          mutate(
            label = sapply(flattened_variable_name, function(col_name) {
              names(sort(table(unlist(list[[col_name]])), decreasing = TRUE)[1])
            }),
            count = sapply(flattened_variable_name, function(col_name) {
              max(table(unlist(list[[col_name]])))
            })
          )
      }
      else if(input$data_type_selector %in% c("integer","numeric"))
      {
        dict <- dict %>% 
          mutate(max = sapply(flattened_variable_name, function(col_name) {max(list[[col_name]], na.rm = TRUE)}),
                 min = sapply(flattened_variable_name, function(col_name) {min(list[[col_name]], na.rm = TRUE)}),
                 median = sapply(flattened_variable_name, function(col_name) {median(list[[col_name]], na.rm = TRUE)}),
                 mean =sapply(flattened_variable_name, function(col_name) {mean(list[[col_name]], na.rm = TRUE)}),
                 sd = sapply(flattened_variable_name, function(col_name) {sd(list[[col_name]], na.rm = TRUE)})
          )
      }
      else if(input$data_type_selector == "logical")
      {
        dict <- dict %>% 
          mutate(count = sapply(flattened_variable_name, function(col_name) {sum(list[[col_name]], na.rm = TRUE)}),
                 proportion = sapply(flattened_variable_name, function(col_name) {mean(list[[col_name]], na.rm = TRUE)})
          )
      }
      else if(input$data_type_selector == "list")
      {
        dict <- dict %>% 
          mutate(max = sapply(flattened_variable_name, function(col_name) {max(length(list[[col_name]]), na.rm = TRUE)}),
                 min = sapply(flattened_variable_name, function(col_name) {min(length(list[[col_name]]),na.rm = TRUE)}),
                 mean =sapply(flattened_variable_name, function(col_name) {mean(length(list[[col_name]]),na.rm = TRUE)})
          )
      }
      
      return(dict)
    }
    
  })  
  
  ### -- ### Bar Charts PANEL ### -- ###
  
  # update sidebar dropdowns to have accurate choices
  
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'bar_charts_variable_group_selector', 
                        choices = selected_game_dict_bar_charts() %>% select(variable_group) %>% unique)
    }
  }) %>% bindEvent(input$one_game_selector)
  
  observe({
    if (!is.null(my_json())) {
      var_group <- input$bar_charts_variable_group_selector
      variables_in_group <- (selected_game_dict_bar_charts()$flattened_variable_name)[selected_game_dict_bar_charts()$variable_group == var_group]
      variable_data_types <- (selected_game_dict_bar_charts()$data_type)[selected_game_dict_bar_charts()$flattened_variable_name %in% variables_in_group]
      
      data_in_var_group <- selected_game_bar_charts() %>% select(all_of(variables_in_group))
      num_unique_per_var <- sapply(data_in_var_group, function(col) length(unique(col)))
      max_unique <- as.numeric(input$bar_charts_max_unique)
      
      variables_in_group_max_n_unique <- variables_in_group[num_unique_per_var <= max_unique | !(variable_data_types %in% c('integer', 'numeric'))]
      # TODO: what to do about viewing list data types
      
      updateSelectInput(session, 'bar_charts_1_var_in_group', 
                        choices = variables_in_group_max_n_unique)
    }
  }) %>% bindEvent(input$bar_charts_variable_group_selector, input$bar_charts_one_game_selector)
  
  
  ### --- Counts - 1 Game ---
  
  # update game selector
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'bar_charts_one_game_selector', 
                        choices = matchIds())
    }
  }) %>% bindEvent(input$user_file)
  
  output$bar_chart_counts_1_game <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$bar_charts_1_var_in_group
      
      g <- selected_game_bar_charts() %>% ggplot()
      
      if (input$bar_charts_show_proportions) {
        g <- g + geom_bar(aes(x = factor(.data[[one_var]]), y = after_stat(prop), group = 1))
      } else {
        g <- g + geom_bar(aes(x = factor(.data[[one_var]])))
      }
      
      g <- g + labs(x = one_var)
      
      if (input$bar_charts_orientation == "Horizontal") {
        g <- g + coord_flip()
      }
      return(g)
    }
  })
  
  ### --- Counts - Multiple Games ---
  
  observe({
    if (!is.null(my_json())) {
      
      choices <- matchIds()
      selected_choices <- if (length(choices) >= 3) choices[1:3] else choices[1]
      
      
      updateSelectizeInput(session, 'bar_charts_multiple_game_selector',
                           choices = matchIds(),
                           options = list(selected = selected_choices)
      )
    }
  }) %>% bindEvent(input$user_file)
  
  #for the barchart selector
  observe({
    if (!is.null(my_json())) {
      
      if(input$bar_charts_show_proportions)
      {
        choice <- c("stacked")
      }
      else
      {
        choice <- c("stacked","dodged", "facet")
      }
      
      updateSelectizeInput(session, 'bar_charts_selector',
                           choices = choice)
    }
  }) %>% bindEvent(input$user_file,input$bar_charts_show_proportions)
  
  
  output$bar_chart_counts_multiple_game <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      
      one_var <- input$bar_charts_1_var_in_group
      
      
      g <- selected_games_bar_charts() %>% ggplot()
      
      
      
      if (input$bar_charts_selector == "stacked")
      {
        if (input$bar_charts_show_proportions)
        {
          g <- g + geom_bar(aes(x = factor(.data[[one_var]]), fill = as.factor(matchId)), position = "fill")
        }
        else
        {
          g <- g + geom_bar(aes(x = factor(.data[[one_var]]), fill = as.factor(matchId)), position = "stack")
        }
        
      }
      else if (input$bar_charts_selector == "dodged")
      {
        if (input$bar_charts_show_proportions)
        {
          proportion_data <- selected_games_bar_charts()%>%
            count(.data[[one_var]], matchId) %>%
            group_by(.data[[one_var]]) %>%
            mutate(prop = n / sum(n))
          
          g <- ggplot(proportion_data, aes(x = factor(.data[[one_var]]), y = prop, fill = as.factor(matchId))) +
            geom_bar(stat = "identity", position = "dodge")
          
        }
        else
        {
          g <- g + geom_bar(aes(x = factor(.data[[one_var]]), fill = as.factor(matchId)), position = "stack")
        }
        
      }
      else
      {
        if (input$bar_charts_show_proportions)
        {
          g <- g + geom_bar(aes(x = factor(.data[[one_var]]), fill = as.factor(matchId)), position = "fill") +
            facet_wrap(~matchId)
        }
        else
        {
          g<- g + geom_bar(aes(x= factor(.data[[one_var]]))) +
            facet_wrap(~matchId)
        }
      }
      
      
      g <- g + labs(x = one_var)
      
      if (input$bar_charts_orientation == "Horizontal")
      {
        g <- g + coord_flip()
      }
      
      return(g)
    }
  })
  
  ### --- Counts - across categories ---
  
  observe({
    if (!is.null(my_json())) {
      var_group <- input$bar_charts_variable_group_selector
      
      # one_var <- input$bar_charts_1_var_in_group
      # variables_in_group <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$variable_group == var_group]
      the_games<- my_json() %>% bind_rows()
      # one_var_unique<-unique(the_games[,one_var])
      # one_var_unique <- my_json() %>% bind_rows() %>% select(all_of(one_var ))%>%
      # distinct() %>% pull()
      
      selected_var <- input$bar_charts_1_var_in_group
      data <- the_games[[selected_var]]
      variables_in_group <- unique(data)
      
      updateSelectInput(session, 'bar_charts_category_selector',
                        choices = variables_in_group)
      
      
    }
  }) %>% bindEvent(input$bar_charts_variable_group_selector, input$bar_charts_1_var_in_group)
  
  output$bar_chart_categories <- renderPlot({
    if (is.null(my_json()))
    {
      return ('No file has been uploaded')
    }
    else
    {
      one_var <- input$bar_charts_1_var_in_group
      unique <- input$bar_charts_category_selector
      
      the_games <- my_json() %>% bind_rows()
      #counts_data <- data.frame(table(the_games$unique, the_games$matchId))
      # counts_data <- the_games %>%
      #   group_by( matchId) %>%
      #   summarise(Count = n_distinct(relatedEventId), .groups = "drop")
      #
      # colnames(counts_data) <- c("matchId", "Count")
      
      ## TODO tidy select programming with dplyr
      # category_data <- the_games %>%
      #   select(matches(one_var))
      #
      # counts <- table(category_data)
      # counts_df <- as.data.frame(counts)
      result <- my_json() %>% bind_rows() %>%
        filter(.data[[one_var]] == unique)
      counts <- table(result$matchId)
      
      # Convert counts to a data frame
      count_df <- data.frame(matchId = as.numeric(names(counts)), Count = as.numeric(counts))
      
      # Create ggplot
      
      if (input$bar_charts_show_proportions) {
        
        count_df <- count_df %>%
          group_by(matchId) %>%
          mutate(Proportion = Count / sum(Count))
        g<-ggplot(count_df, aes(x = factor(matchId), y = Proportion))+
          geom_bar(stat = "identity", fill = "skyblue", width = 0.5)
      } else {
        g<-ggplot(count_df, aes(x = factor(matchId), y = Count))+
          geom_bar(stat = "identity", fill = "skyblue", width = 0.5)
      }
      g <- g+labs(x = "matchId")
      
      if (input$bar_charts_orientation == "Horizontal")
      {
        g <- g + coord_flip()
      }
      return(g)
    }
  })
  
  ### -- ### Histograms PANEL ### -- ###
  
  # update sidebar dropdowns to have accurate choices
  
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'histograms_variable_group_selector', 
                        choices = selected_game_dict_histograms() %>% select(variable_group) %>% unique)
    }
  }) %>% bindEvent(input$one_game_selector)
  
  observe({
    if (!is.null(my_json())) {
      var_group <- input$histograms_variable_group_selector
      variables_in_group <- (selected_game_dict_histograms()$flattened_variable_name)[selected_game_dict_histograms()$variable_group == var_group]
      variable_data_types <- (selected_game_dict_histograms()$data_type)[selected_game_dict_histograms()$flattened_variable_name %in% variables_in_group]
      
      variables_in_group_numeric <- variables_in_group[variable_data_types %in% c('integer', 'numeric')]
      
      updateSelectInput(session, 'histograms_1_var_in_group', 
                        choices = variables_in_group_numeric)
    }
  }) %>% bindEvent(input$histograms_variable_group_selector, input$histograms_one_game_selector)
  
  
  ### --- Histograms - 1 Game ---
  
  # update game selector
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'histograms_one_game_selector', 
                        choices = matchIds())
    }
  }) %>% bindEvent(input$user_file)
  
  output$histogram_1_game <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$histograms_1_var_in_group
      
      g <- selected_game_histograms() %>% ggplot()
      
      if (input$histograms_include_kde) {
        g <- g + geom_histogram(aes(x = .data[[one_var]],
                                    y = after_stat(density)),
                                bins = input$histograms_num_bins)
        g <- g + geom_density(aes(x = .data[[one_var]]))
      }
      else {
        g <- g + geom_histogram(aes(x = .data[[one_var]]),
                                bins = input$histograms_num_bins)
      }
      
      if (input$histograms_include_rugs) {
        g <- g + geom_rug(aes(x = .data[[one_var]]))
      }
      
      g <- g + labs(x = one_var)
      
      
      return(g)
    }
  })
  
  ### --- Histograms - Multiple Games ---
  
  observe({
    if (!is.null(my_json())) {
      
      choices <- matchIds()
      selected_choices <- if (length(choices) >= 3) choices[1:3] else choices[1]
      
      updateSelectizeInput(session, 'histograms_multiple_game_selector',
                           choices = matchIds(),
                           options = list(selected = selected_choices)
      )
    }
  }) %>% bindEvent(input$user_file)
  
  
  output$histogram_multiple_games <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      
      one_var <- input$histograms_1_var_in_group
      
      
      
      if(input$histogram_type_selector == "color")
      {
        
        g<-selected_games_histogram_charts() %>%ggplot(., aes(x = .data[[one_var]], color = matchId)) +
          geom_freqpoly()
      }
      else
      {
        g <- selected_games_histogram_charts() %>%
          ggplot(aes(x = .data[[one_var]])) +
          geom_freqpoly() +
          facet_wrap(~ matchId, scales = "free")
      }
      
      
      if (input$histograms_multiple_kde) {
        g <- g + stat_density(geom = "line", aes(group = matchId), position = "identity")
      }
      
      
      
      g <- g + labs(x = one_var)
      
      
      
      
      return(g)
      
    }
  })
  
  ### Histograms - Boxplots
  
  histogram_var_per_game <- reactive({
    one_var <- input$histograms_1_var_in_group
    var_data <- lapply(my_json(), function(game) game %>% select(all_of(c(one_var, 'matchId'))))
    var_data <- bind_rows(var_data)
    var_data$matchId <- as.factor(var_data$matchId)
    
    return(var_data)
  })
  
  output$histograms_boxplots <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$histograms_1_var_in_group
      g <- histogram_var_per_game() %>% ggplot() +
        geom_boxplot(aes(x=matchId, y=.data[[one_var]]))
      
      return(g)
    }
  })
  
  output$histograms_boxplot_mean_ci <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      one_var <- input$histograms_1_var_in_group
      summary_stats <- histogram_var_per_game() %>% group_by(matchId) %>% summarise(
        mean = mean(.data[[one_var]], na.rm=TRUE),
        sd = sd(.data[[one_var]], na.rm=TRUE),
        se = sd / sqrt(n()),
        CI_lower = mean - (1.96 * se),
        CI_upper = mean + (1.96 * se)
      )
      
      g <- summary_stats %>% ggplot(aes(x=matchId)) + 
        geom_point(aes(y=mean), color='blue', size=3) +
        geom_linerange(aes(ymin=CI_lower, ymax=CI_upper))
      
      return(g)
    }
  })
  
  ### Histograms - Violin Plots
  
  output$histograms_violinplots <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      
      quantiles = NULL
      if (input$histograms_violin_quantiles) {
        quantiles = c(0.25, 0.5, 0.75)
      }
      
      one_var <- input$histograms_1_var_in_group
      g <- histogram_var_per_game() %>% ggplot() +
        geom_violin(aes(x=matchId, y=.data[[one_var]]), 
                    draw_quantiles = quantiles)
      
      return(g)
    }
  })
  
  ### --- ### LIST TYPES PANEL ### --- ###
  
  # update dropdowns to have match ID choices and list variable choices
  
  observe({
    if (!is.null(my_json())) {
      updateSelectInput(session, 'list_vars_one_game_selector', 
                        choices = matchIds())
    }
  }) %>% bindEvent(input$user_file)
  
  observe({
    if (!is.null(my_json())) {
      list_variables <- (selected_game_dict()$flattened_variable_name)[selected_game_dict()$data_type == 'list']
      updateSelectInput(session, 'list_vars_variable_selector', 
                        choices = list_variables)
      
      
    }
  }) %>% bindEvent(input$list_vars_one_game_selector)
  
  val_to_row <- reactive({
    purrr::map_dfr(1:nrow(selected_game_list_vars()),
                   extract_list_values_per_row,
                   list_column_name = input$list_vars_variable_selector,
                   single_game = selected_game_list_vars())
  })
  
  # calculated outside render block to make faceting by team really easy
  list_vars_1_game_plot <- reactive({
    g <- val_to_row() %>% 
      left_join(selected_game_list_vars() %>% rowid_to_column() %>% 
                  select(rowid, 'type.primary', 'matchPeriod', 'team.name'), 
                by = 'rowid') %>% 
      mutate(var_in_freq_order = forcats::fct_infreq(val_to_row()[[input$list_vars_variable_selector]])) %>% 
      ggplot(mapping = aes(y = var_in_freq_order))
    
    if (input$list_vars_fill_by == 'type.primary') {
      g <- g + geom_bar(aes(fill = type.primary))
    }
    else if (input$list_vars_fill_by == 'matchPeriod') {
      g <- g + geom_bar(aes(fill = matchPeriod))
    }
    else {
      # no fill
      g <- g + geom_bar()
    }
    g <- g + labs(y = input$list_vars_variable_selector)
    
    return(g)
  })
  
  
  # output$list_vars_1_game <- renderPlot({
  #   if (is.null(my_json())) {
  #     return ('No file has been uploaded')
  #   } else {
  #     return(list_vars_1_game_plot())
  #   }
  # })
  
  output$list_vars_1_game_dynamic <- renderUI({
    renderPlot({
      if (is.null(my_json())) {
        return ('No file has been uploaded')
      } else {
        return(list_vars_1_game_plot())
      }
    }, height = input$list_vars_graph_height)
  })
  
  # output$list_vars_1_game_by_team <- renderPlot({
  #   if (is.null(my_json())) {
  #     return ('No file has been uploaded')
  #   } else {
  #     return(list_vars_1_game_plot() + facet_wrap(~team.name))
  #   }
  # })
  # 
  output$list_vars_1_game_by_team_dynamic <- renderUI({
    renderPlot({
      if (is.null(my_json())) {
        return ('No file has been uploaded')
      } else {
        return(list_vars_1_game_plot() + facet_wrap(~team.name))
      }
    }, height = input$list_vars_by_team_graph_height)
  })
  
  ######  ######  ######  ######  ######  ######  ###### 
  #get pass origin x, y
  
  start_x <- reactive({
    map_dbl(my_json(), ~pluck(.x, "location", "x", .default = NA_real_))
  })
  
  start_y <- reactive({
    map_dbl(my_json(), ~pluck(.x, "location", "y", .default = NA_real_))
  })
  
  end_x <- reactive({
    map_dbl(my_json(), ~pluck(.x, "pass", "endLocation", "x", .default = NA_real_))
  })
  
  end_y <- reactive({
    map_dbl(my_json(), ~pluck(.x, "pass", "endLocation", "y", .default = NA_real_))
  })

  
  output$pass_positions <- renderPlot({
    req(selected_game()) # Ensure the selected game data is loaded
    
    # Extract the start and end coordinates for plotting
    data_for_plot <- tibble(
      start_x = start_x(),
      start_y = start_y(),
      end_x = end_x(),
      end_y = end_y()
    )
    
    print(head(data_for_plot))

    ggplot(data_for_plot, aes(x = start_x, y = start_y)) +
      geom_point(color = "blue", size = 3) +
      geom_segment(aes(xend = end_x, yend = end_y), arrow = arrow(length = unit(0.02, "npc")), color = "green") +
      scale_x_continuous(name = "Field Width") +
      scale_y_continuous(name = "Field Length") +
      ggtitle("Pass Start and End Positions")
    
  }
  )
  
  
}
