
navbarPage("Pitt Soccer Analytics",
           tabPanel('Welcome!',
                    includeMarkdown("welcome.md")
           ),
           tabPanel("Upload JSON",
                  # This code can probably be copied from the provided
                  # app
           ),
           tabPanel("Passes",
                  # Visualizations and Information about passes
           ),
           tabPanel("Shots",
                  # Visualizations and Information about shots
           )
           
)