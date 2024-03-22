navbarPage('Wyscout Event Level Data',
           tabPanel('Welcome!',
                    includeMarkdown("welcome.md")
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
                                    ),
                                    tabPanel('Variable data types',
                                             h3("Number of variables"),
                                             verbatimTextOutput('number_of_variables'),
                                             h3("Number of Variables per data type"),
                                             plotOutput('number_of_variables_dataType'),
                                             h3("Number of Variables groups"),
                                             verbatimTextOutput('number_of_variables_group'),
                                             h3("Number of Variables per each group"),
                                             plotOutput('number_of_variables_per_group'),
                                             h3("Number of Variables per each group by dataType"),
                                             plotOutput('number_of_variables_per_group_dataType')
                                    )
                        )
                      )
                    )),
           tabPanel("pass",
                        tabsetPanel(type = 'pills',
                                    tabPanel("pass plot 1",
                                      fluidRow(
                                        column(2,
                                          actionButton("confirmButton", "Confirm"),  
                                              selectInput('list_vars_one_id',
                                               "Select ID want to see:", "",
                                               multiple = T)),# choices updated in server,
                                        column(10,
                                          plotOutput("passplot")))),
                                    tabPanel("hmap plot 2",
                                        selectInput('list_match_period',
                                                "Select match_period:", "",multiple = T), # choices updated in server
                                        fluidRow(
                                          column(6,p("start"),plotOutput("hmap1")),
                                          column(6,p("end"),plotOutput("hmap2"))
                                        )
                                    )
                        )
           ),
           tabPanel("Shots",
                    tabsetPanel(type = 'pills',
                                tabPanel("Goal Information",
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
                                             
                                           ),
                                           mainPanel(
                                             
                                           )
                                         )  
                                )
                        )
           )








)
