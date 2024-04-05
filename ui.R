navbarPage('Wyscout Event Level Data',
           tabPanel('Welcome!',
                    includeMarkdown("README.md")
           ),
           tabPanel('Upload JSON',
                    sidebarLayout(
                      sidebarPanel(
                        fileInput('user_file',
                                  'Upload Wyscout exported JSON file:')
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                                    tabPanel('Games in upload',
                                             h3("The number of games in the JSON file is:"),
                                             textOutput('num_games'),
                                             h3("The number of unique teams in the JSON file is:"),
                                             textOutput('num_unique_teams'),
                                             h3("The number of events in each game is:"),
                                             plotOutput('num_events_per_game'),
                                             h3("The date and team names for each game is:"),
                                             tableOutput('team_names_per_game')
                                    )
                        )
                      )
                    )),
                    tabPanel("Pass Analysis",
                        tabsetPanel(type = 'pills',
                                    tabPanel("Football Passing Network Map",
                                 fluidRow(
                                   column(2,
                                   actionButton("confirmButton", "Confirm"),  
                                   selectInput('list_vars_one_id',
                                               "Select ID want to see:", "",
                                               multiple = T)),# choices updated in server,
                                   column(10,
                                   plotOutput("passplot")))),
                                    tabPanel("Probability Heatmap Function",
                                    selectInput('list_match_period',
                                                "Select match_period:", "",multiple = T), # choices updated in server
                                    fluidRow(
                                    column(6,p("start"),plotOutput("hmap1")),
                                    column(6,p("end"),plotOutput("hmap2"))
                                    )
                        )
                    )
                  ),
                    tabPanel("Scoring Probability Heatmap",
                      sidebarLayout(
                        sidebarPanel(
                            h4("Scoring Probability Heatmap"),
                            helpText("Visualize the probability of scoring from different field positions.")
                        ),
                          mainPanel(
                            plotOutput("shotHeatmap")
                          )
                        )
                  ),
                  tabPanel("Shot Information Table",
                    # attempts, onTarget, goals, xg, xg_diff
                    # sort, grp, sort_by
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("shotTableCols",
                                           "Information Categories",
                                           choices=c("Shot Attempts",
                                                     "Shots on Target",
                                                     "Actual Goals Scored",
                                                     "Xg",
                                                     "Xg Difference"),
                                           selected=c("Shot Attempts",
                                                      "Shots on Target",
                                                      "Actual Goals Scored",
                                                      "Xg",
                                                      "Xg Difference")),
                        radioButtons("shotTableGroup",
                                     "Present Information by Player or by Team",
                                     choices=c("Player", "Team"))
                      ),
                      mainPanel(
                        dataTableOutput("shotTable")
                      )
                    )
                    
                  ),
                  tabPanel("Shot Location Visualization",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("facet_1",
                                    "Select the first Category to group by",
                                    choices=c("None",
                                              "Match Period",
                                              "Body Part",
                                              "Shot is Goal",
                                              "Shot on Target",
                                              "Team Name",
                                              "Player Name",
                                              "Goal Zone"),
                                    selected="None"
                        ),
                        selectInput("facet_2",
                                    "Select the second Category to group by",
                                    choices=c("None",
                                              "Match Period",
                                              "Body Part",
                                              "Shot is Goal",
                                              "Shot on Target",
                                              "Team Name",
                                              "Player Name",
                                              "Goal Zone"),
                                    selected="None"
                        ),
                        selectInput("shotVizColor",
                                    "Select the variable that controls the color",
                                    choices=c("None",
                                              "Match Period",
                                              "Body Part",
                                              "Shot is Goal",
                                              "Shot on Target",
                                              "Team Name",
                                              "Player Name",
                                              "Goal Zone"),
                                    selected="None"
                        ),
                        selectInput("shotVizShape",
                                    "Select the variable that controls the shape",
                                    choices=c("None",
                                              "Match Period",
                                              "Body Part",
                                              "Shot is Goal",
                                              "Shot on Target",
                                              "Team Name",
                                              "Player Name",
                                              "Goal Zone"),
                                    selected="None"
                        ),
                        radioButtons("shotVizGoalCircle",
                                     "Should goals be circled?",
                                     choices=c("Yes", "No"))
                      ),
                      mainPanel(
                        plotOutput("shotVisualization")
                      )
                    ) 
           
           )
           tabPanel("Type of Duel Analysis",
                 fluidPage(
                 selectInput("duelType", "Select Duel Type:", choices = NULL),
                 plotOutput("duelPlot")
               )
             )

)
