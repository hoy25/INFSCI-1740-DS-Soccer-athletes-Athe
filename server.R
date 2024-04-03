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
  

  
  ### ---psss mod ---
  
  # reactive for list of match ids
  palyerid <- reactive({
    my_json() %>% bind_rows() %>% pull(player.id) %>% unique()
  })
  # reactive for list of match ids
  matchPeriod <- reactive({
    my_json() %>% bind_rows() %>% pull(matchPeriod) %>% unique()
  })
  observe({
    if (!is.null(my_json())) {
      updateSelectizeInput(session, 'list_vars_one_id',
                           choices = palyerid(),
                           selected = palyerid()[1:10])
    }
  }) %>% bindEvent(input$user_file)
  observe({
    if (!is.null(my_json())) {
      updateSelectizeInput(session, 'list_match_period',
                           choices = matchPeriod(),
                           selected = matchPeriod()[1])
    }
  }) %>% bindEvent(input$user_file)
  ### pass plot
  dat<-eventReactive(input$confirmButton,{
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
    my_json() %>% bind_rows()%>% 
      filter(player.id%in%input$list_vars_one_id) %>%
      mutate(time=minute(hms(matchTimestamp)))
    }})
  output$passplot <- renderPlot({
    d1=dat()
      ggplot(d1,aes(x=location.x,y=location.y,
                    col=as.factor(player.id)))+
        geom_point()+
        geom_point(data=d1,aes(x=pass.endLocation.x,y=pass.endLocation.y,
                               col=as.factor(pass.recipient.id)))+
        facet_wrap(~paste0(player.name,"(",player.id,")"))+
        geom_segment(data=d1,aes(x = location.x, y = location.y,
                                 xend = pass.endLocation.x,
                                 yend = pass.endLocation.y,
                                 linetype =pass.accurate),show.legend = F,
                     col= hsv(v = seq(0, 1, 1/(nrow(d1)-1))))
  })

  # reactive for list of match ids
  d1 <- reactive({
    my_json() %>% bind_rows() %>% filter(matchPeriod%in%input$list_match_period) %>% 
      mutate(x1=cut(location.x,seq(0,100,by=10)),
             y1=cut(location.y,seq(0,100,by=10)),
             x2=cut(pass.endLocation.x,seq(0,100,by=10)),
             y2=cut(pass.endLocation.y,seq(0,100,by=10)))
  })
  output$hmap1 <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      d1() %>% count(x1,y1) %>% drop_na() %>% 
        ggplot(aes(x1,y1,fill=n))+geom_tile(col=1)+scale_fill_gradient(
          low="white",high = "red")
    }
  })
  output$hmap2 <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } else {
      d1() %>% count(x2,y2) %>% drop_na() %>% 
        ggplot(aes(x2,y2,fill=n))+geom_tile(col=1)+scale_fill_gradient(
          low="white",high = "red")
    }
  })
  
  ###Scoring Probability Heatmap
  
  shots_data <- reactive({
    req(my_json()) 
    shots <- my_json() %>% 
      bind_rows() %>% 
      select(location.x, location.y, shot.isGoal) %>% 
      na.omit() 
    
    shots <- shots %>% 
      mutate(x_bin = cut(location.x, breaks = seq(0, 100, by = 6)),
             y_bin = cut(location.y, breaks = seq(0, 100, by = 6))) %>% 
      group_by(x_bin, y_bin) %>% 
      summarise(goals = sum(shot.isGoal, na.rm = TRUE),
                shots = n(),
                scoring_prob = goals / shots) %>% 
      ungroup()
    
    shots
  })
  
  output$shotHeatmap <- renderPlot({
    req(shots_data())
    
    ggplot(shots_data(), aes(x = x_bin, y = y_bin, fill = scoring_prob)) +
      geom_tile() + 
      scale_fill_gradient(low = "blue", high = "red") +
      labs(fill = "Scoring Probability") +
      theme_minimal() +
      ggtitle("Scoring Probability Heatmap")
  })
      
  ### Generate Shot Table
  shot_df <- reactive({
    games_list <- my_json()
    games_df <- games_list[[1]] # init the dataframe with the first game
    
    if(number_of_games() > 1){
      for(i in 2:number_of_games()){
        games_df <- rbind(games_df, games_list[[i]])
      }
    }
    # now games_df is a dataframe with all of the games that were in the json
    df_shots <- games_df %>% select(matchPeriod, minute, second, location.x, 
                                    location.y, shot.bodyPart, shot.isGoal, 
                                    shot.onTarget, shot.goalZone,shot.xg, 
                                    player.name, team.name) %>% na.omit()
    
    return(df_shots)
  })
  
  generate_shot_table <- reactive({
    df_shots <- shot_df()
    
    #attempts=TRUE, onTarget=TRUE, 
    #goals=TRUE, xg=TRUE, xg_diff=TRUE,
    #sort="desc", grp="player.name",
    #sort_by="actual_goals"
    
    ## Set up the variables
    if("Shot Attempts" %in% input$shotTableCols){
      attempts = TRUE
    }
    else{
      attempts = FALSE
    }
    
    if("Shots on Target" %in% input$shotTableCols){
      onTarget = TRUE
    }
    else{
      onTarget = FALSE
    }
    
    if("Actual Goals Scored" %in% input$shotTableCols){
      goals = TRUE
    }
    else{
      goals = FALSE
    }
    
    if("Xg" %in% input$shotTableCols){
      xg = TRUE
    }
    else{
      xg = FALSE
    }
    
    if("Xg Difference" %in% input$shotTableCols){
      xg_diff = TRUE
    }
    else{
      xg_diff = FALSE
    }
    
    if(input$shotTableGroup == "Player"){
      grp = "player.name"
    }
    else{
      grp = "team.name"
    }
    
    shot_table <- NULL
    
    shots_attempted <- df_shots %>% group_by(df_shots[[grp]]) %>% 
      count(df_shots[[grp]]) %>%
      summarise(shotAttempts = n)
    
    shot_table = shots_attempted
    
    names(shot_table)[1] <- input$shotTableGroup
    
    shotsOnTarget <- df_shots %>% filter(shot.onTarget=TRUE) %>% 
      group_by(df_shots[[grp]])%>% 
      count(df_shots[[grp]])%>%
      summarise(shotsOnTarget = n)
    
    names(shotsOnTarget)[1] <- input$shotTableGroup
    
    if(grp=="player.name"){
      shot_table <- merge(shot_table, shotsOnTarget, 
                          by="Player",
                          all.x=TRUE)
    }
    else{
      shot_table <- merge(shot_table, shotsOnTarget, 
                          by="Team",
                          all.x=TRUE)
    }
    
    if(grp == "player.name"){
      actual_goals <- df_shots %>% filter(shot.isGoal==TRUE) %>% 
        group_by(player.name) %>% 
        count(player.name) %>% 
        summarise(actual_goals = n)
    }
    else{
      actual_goals <- df_shots %>% filter(shot.isGoal==TRUE) %>% 
        group_by(team.name) %>% 
        count(team.name) %>% 
        summarise(actual_goals = n)
    }
    
    names(actual_goals)[1] <- input$shotTableGroup
    
    if(grp=="player.name"){
      shot_table <- merge(shot_table, actual_goals, 
                          by="Player",
                          all.x=TRUE)
    }
    else{
      shot_table <- merge(shot_table, actual_goals, 
                          by="Team",
                          all.x=TRUE)
    }
    
    # maybe don't do the arrange part
    if(grp=="player.name"){
      xg_df <- df_shots %>% select(player.name, shot.xg) %>%
        group_by(player.name) %>% 
        summarise(xg = sum(shot.xg)) %>% arrange(-xg)
    }
    else{
      xg_df <- df_shots %>% select(team.name, shot.xg) %>%
        group_by(team.name) %>% 
        summarise(xg = sum(shot.xg)) %>% arrange(-xg)
    }
    
    names(xg_df)[1] <- input$shotTableGroup
    
    if(grp=="player.name"){
      shot_table <- merge(shot_table, xg_df,
                          by="Player",
                          all.x=TRUE)
    }
    else{
      shot_table <- merge(shot_table, xg_df,
                          by="Team",
                          all.x=TRUE)
    }
    
    shot_table[is.na(shot_table)] <- 0
    
    shot_table <- shot_table %>% mutate(difference = (actual_goals -xg))
    
    # attempts, onTarget, goals, xg, xg_diff
    if(!attempts){
      shot_table <- shot_table %>% select(-shotAttempts)
    }
    
    if(!onTarget){
      shot_table <- shot_table %>% select(-shotsOnTarget)
    }
    
    if(!goals){
      shot_table <- shot_table %>% select(-actual_goals)
    }
    
    if(!xg){
      shot_table <- shot_table %>% select(-xg)
    }
    
    if(!xg_diff){
      shot_table <- shot_table %>% select(-difference)
    }
    
    return(shot_table)
  })
  
  output$shotTable <- renderDataTable({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } 
    else {
      return(generate_shot_table())
    }
  })
  
  create_shot_plot <- reactive({
    df_shots <- shot_df()
    
    guide_str <- c("Match Period" = "matchPeriod",
                   "Body Part" = "shot.bodyPart",
                   "Shot is Goal" = "shot.isGoal",
                   "Shot on Target" = "shot.onTarget",
                   "Team Name" = "team.name",
                   "Player Name" = "player.name",
                   "Goal Zone" = "shot.goalZone")
    
    # scale the x and y locations
    dfs <- df_shots %>% mutate(loc.x = convert_x(location.x))
    dfs <- dfs %>% mutate(loc.y = convert_y(location.y))
    
    plt <- dfs %>% ggplot(aes(x=loc.x, y=loc.y))
    
    if(input$shotVizColor != "None"){
      clr_on = TRUE
      clr = guide_str[input$shotVizColor]
    }
    else{
      clr_on = FALSE
    }
    
    if(input$shotVizShape != "None"){
      shape_on = TRUE
      shpe = guide_str[input$shotVizShape]
    }
    else{
      shape_on = FALSE
    }
    
    # do the color and the shape for the points
    if(clr_on && shape_on){
      plt <- plt + geom_point(aes(color = dfs[[clr]], 
                                  shape = dfs[[shpe]])) +
        guides(color=guide_legend(input$shotVizColor),
               shape=guide_legend(input$shotVizShape))
    }
    else if(clr_on){
      plt <- plt + geom_point(aes(color=dfs[[clr]])) +
        guides(color=guide_legend(input$shotVizColor))
    }
    else if (shape_on){
      plt <- plt + geom_point(aes(shape=dfs[[shpe]])) +
        guides(shape=guide_legend(input$shotVizShape))
    }
    else{
      plt <- plt + geom_point()
    }
    
    # set the boundaries on the plot so it better represents a soccer field
    plt <- plt + xlim(0, field_length_x) + ylim(0, field_width_y)
    
    # account for any necessary facetting
    
    if(input$facet_1 != "None" && input$facet_2 != "None"){
      # facet_grid
      if(input$facet_1 == input$facet_2){
        plt <- plt + facet_wrap(as.formula(paste("~", guide_str[input$facet_1])), ncol=1)
      }
      else{
        plt <- plt + facet_grid(as.formula(paste(guide_str[input$facet_1], "~", guide_str[input$facet_2])))
      }
    }
    else if(input$facet_1 != "None"){
      # facet_wrap with facet_1
      plt <- plt + facet_wrap(as.formula(paste("~", guide_str[input$facet_1])), ncol=1)
    }
    else if(input$facet_2 != "None"){
      # facet_wrap with facet_2
      plt <- plt + facet_wrap(as.formula(paste("~", guide_str[input$facet_2])), ncol=1)
    }
    else{
      # no facet_wrap
    }
    
    # account for the goal circles
    if(input$shotVizGoalCircle == "Yes"){
      plt <- plt + geom_point(data=dfs %>% filter(shot.isGoal),
                              pch=21, 
                              size=4, 
                              colour="purple")
    }
    
    return(plt)
    
  })
  
  output$shotVisualization <- renderPlot({
    if (is.null(my_json())) {
      return ('No file has been uploaded')
    } 
    else {
      return(create_shot_plot())
    }
  })
  
}
