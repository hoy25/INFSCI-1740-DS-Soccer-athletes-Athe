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
           tabPanel('Variable details',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('variable_group_selector',
                                    "Variable Group:", ""), # choices updated in server
                        radioButtons('top_bar_chart_orientation',
                                     "Top Bar Chart Orientation:",
                                     c("Vertical", "Horizontal")),
                        selectInput('one_game_selector',
                                    "Select 1 game by Match ID:", ""), # choices updated in server
                        radioButtons('bottom_bar_chart_orientation',
                                     "Bottom Bar Chart Orientation:",
                                     c("Vertical", "Horizontal")),
                        checkboxInput('variable_details_show_proportions',
                                      "Show Proportions?")
                        
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                          tabPanel('Unique values',
                                   h4("Study the number of unique values for all variables within a single game"),
                                   radioButtons('unique_values_facet_by_data_type',
                                                "Facet by data type:",
                                                c("Yes", "No"), inline = TRUE),
                                   h5("Number of unique values for each variable in the variable group for 1 game:"),
                                   plotOutput('unique_values_per_variable_chart'),
                                   h4("Study the number of unique values for 1 variable across all games"),
                                   selectInput('unique_values_1_var_in_group',
                                               "Select 1 variable in variable group:", ""), # choices updated in server
                                   h5("Number of unique values for the selected variable across all games:"),
                                   plotOutput('unique_values_for_1_variable_chart')
                                   ),
                          tabPanel('Missing values',
                                   h4("Study the number of missing values for all variables within a single game"),
                                   radioButtons('missing_values_facet_by_data_type',
                                                "Facet by data type:",
                                                c("Yes", "No"), inline = TRUE),
                                   h5("Number of missing values for each variable in the variable group for 1 game:"),
                                   plotOutput('missing_values_per_variable_chart'),
                                   h4("Study the number of missing values for 1 variable across all games"),
                                   selectInput('missing_values_1_var_in_group',
                                               "Select 1 variable in variable group:", ""), # choices updated in server
                                   h5("Number of missing values for the selected variable across all games:"),
                                   plotOutput('missing_values_for_1_variable_chart')
                                   ),
                          tabPanel('Data dictionary',
                                  h5("Select data type:"),
                                             selectInput('data_type_selector',
                                                         "Select 1 data type:", 
                                                         choices = c("character","integer","numeric","list","logical")),
                                             tableOutput('data_dict'))
                        )
                      )
                    )
                    ),
           tabPanel('Bar charts',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('bar_charts_variable_group_selector',
                                    "Variable Group:", ""), # choices updated in server
                        selectInput('bar_charts_max_unique',
                                    "Include numeric columns with at most this many unique values:",
                                    choices = 2:10),
                        selectInput('bar_charts_1_var_in_group',
                                    "Select variable:", ""), # choices updated in server
                        radioButtons('bar_charts_orientation',
                                     "Bar Chart Orientation:",
                                     c("Vertical", "Horizontal")),
                        checkboxInput('bar_charts_show_proportions',
                                      "Show Proportions?")
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                          tabPanel("Counts - 1 Game",
                                   selectInput('bar_charts_one_game_selector',
                                               "Select 1 game by Match ID:", ""), # choices updated in server
                                   plotOutput('bar_chart_counts_1_game')
                                   ),
                          tabPanel("Counts - Multiple Games",
                                             selectizeInput('bar_charts_multiple_game_selector',
                                                         "Select multiple game by Match ID:","", multiple =TRUE), # choices updated in server
                                             selectInput('bar_charts_selector',
                                                            "Show seperate games as:",""),
                                             plotOutput('bar_chart_counts_multiple_game')
                                             ),
                                    tabPanel("Category across games",
                                             selectInput('bar_charts_category_selector',
                                                         "Select category:", ""),
                                             plotOutput('bar_chart_categories')
                                             )
                          
                        )
                      )
                    )),
           tabPanel("Histograms",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('histograms_variable_group_selector',
                                    "Variable Group:", ""), # choices updated in server
                        selectInput('histograms_1_var_in_group',
                                    "Select variable:", ""), # choices updated in server
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                          tabPanel("Histogram - 1 Game",
                                   selectInput('histograms_one_game_selector',
                                               "Select 1 game by Match ID:", ""), # choices updated in server
                                   sliderInput('histograms_num_bins', "Number of bins", 5, 100, 30, step = 1),
                                   checkboxInput('histograms_include_kde', "Include KDE?"),
                                   checkboxInput('histograms_include_rugs', "Include Rugs?"),
                                   plotOutput('histogram_1_game')
                                   ),
                          tabPanel("Histogram - Multiple Games",
                                             selectizeInput('histograms_multiple_game_selector',
                                                            "Select multiple game by Match ID:","", multiple =TRUE),
                                             selectInput('histogram_type_selector',
                                                         "Choose color or facets:",choices = c("color","facet"),),
                                             plotOutput('histogram_multiple_games'),
                                             checkboxInput('histograms_multiple_kde', "KDE")
                                             ),
                          tabPanel("Boxplots",
                                   plotOutput('histograms_boxplots'),
                                   h5("The mean of the variable with 95% confidence interval across all games is:"),
                                   plotOutput('histograms_boxplot_mean_ci')
                                   ),
                          tabPanel("Violin Plots",
                                   checkboxInput('histograms_violin_quantiles', "Include quantiles?"),
                                   plotOutput('histograms_violinplots')
                                   )
                        )
                      )
                    )),
           tabPanel("List-Type Values",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('list_vars_one_game_selector',
                                    "Select 1 game by Match ID:", ""), # choices updated in server
                        selectInput('list_vars_variable_selector',
                                    "Select list variable:", ""), # choices updated in server
                        selectInput('list_vars_fill_by',
                                    "Color bars by:", c("No Color", "type.primary", "matchPeriod"))
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                          tabPanel("Counts - 1 Game",
                                   sliderInput('list_vars_graph_height', "Graph height", 200, 1000, 500, step = 100),
                                   uiOutput('list_vars_1_game_dynamic')),
                          tabPanel("Facet by Team",
                                   sliderInput('list_vars_by_team_graph_height', "Graph height", 200, 1000, 500, step = 100),
                                   uiOutput('list_vars_1_game_by_team_dynamic'))
                        )
                      )
                      
                    ) ),
           
           tabPanel("Scoring Probability Heatmap", plotOutput("scoringProbability"))
)
