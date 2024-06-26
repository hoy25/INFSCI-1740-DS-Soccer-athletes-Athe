# Pitt Soccer Analytics App

Welcome! 

This app is designed to provide insight about collegiate soccer through the use of visual plots and tables that summarize key statistics.

## How to use the App

### Uploading Data
First the app needs data to analyze. To upload data:
1. Navigate to the tab labeled **Upload JSON** in the top row.
2. Click on the box labeled **Browse...** on the lefthand side of the page
3. Select the Wayscout Event level data JSON file that contains the game(s) that you would like to analyze.
4. There should be a blue bar under the **Browse...** button that says "Upload complete", along with information in the "Games in upload" section on the right; If so, that indicates that the data has been successfully uploaded into the app.

With the data uploaded now you can use the other tabs in the top row of the app to view the data in different ways.

## Tabs

### Pass Analysis
#### Probability Heatmap Function
This function is designed to analyze and visualize where passes start and end in the form of a heatmap that represents different areas of the soccer field. 

The algorithm can show the pass occurances of each half (e.g. `1H` means first half) games separately, and this plot also can show occurances combination by selecting on the tab.

#### Football Passing Network Map
This page provides a visualization of different soccer players' passing patterns, with the following functionalities:
1. **Pass Plots for Individual Players**: Each subplot corresponds to an individual player's passing data during a match. The players are likely identified by the IDs listed in the “Select ID want to see” section.
2. **Pass Outcomes**: The solid lines represent successful passes, while the dotted lines indicate unsuccessful ones. This feature allows for an analysis of both the quantity and quality of a player's passes.
3. **Player Selection**: Users can select which players' data they wish to view by checking boxes next to the given IDs.
4. **Color-Coded Players**：Each player is assigned a unique color, which is used to plot their passes on the grid. This makes it easier to distinguish between different players on the same plot
5. **Field Grid**: The x and y axes, labeled “location.x” and “location.y,” represent the football pitch, providing a spatial reference for where passes are made.
6. **Confirm Button**: After selecting the desired player IDs, the user likely needs to press the “Confirm” button to update the plots according to their selection.

With these features, the Shiny app enables users to analyze passing strategies and performance of individual players, potentially providing insights into team dynamics and player decision-making on the pitch.


### Scoring Probability Heatmap
This function is designed to analyze and visualize scoring probabilities based on shot locations, particularly focusing on shots made during a game. The visualization is in the form of a heatmap that represents different areas of the soccer field.

The algorithm combines the scoring probabilities of all games in a single dataset to ensure the greatest confidence in the probability of scoring at different positions of the field.

1. **Heatmap Generation**: Utilizes ggplot2 to create a heatmap. Each tile's color intensity represents the scoring probability for that area, with the color gradient ranging from low (blue) to high (red) scoring probabilities.
2. **Aesthetics and Labels**: The heatmap includes a color scale to indicate scoring probabilities, minimalistic theme styling, and a title.
3. **Reactivity**: The heatmap updates reactively to changes in the input data, ensuring that the visualization always reflects the current dataset.

The output of this function is a heatmap visualization that provides insight into which areas of the playing field have higher or lower probabilities of scoring. This information can be crucial for teams and coaches to develop strategies, train players, and analyze opponents' defensive weaknesses.

###  Shot Information Table
This page generates a table that summarizes the information regarding shots from the provided data. It can display:
1. **shotAttempts**: The number of shots attempted
2. **shotsOnTarget**: The number of shots that were actually on goal
3. **actualGoals**: The number of goals scored
4. **xg**: The expected number of goals scored
5. **difference**: The difference between the expected goals and the actual goals (xg - actual)

The user can decide whether these columns are calculated for each player or each team in the provided data, although a player/team must register at least one shot attempt to be included. 

The checkboxes on the left hand side of the screen give the ussr this control. Any box checked under **Information Categories** will have the corresponding column presented, and the selected option, either “Player” or “Team”, under **Present Information by Player or by Team** will determine the grouping. 

Users can also sort the resulting table by any of the columns by simply clicking on the column name at the top. 

This table is useful because it presents all the information in one location, allowing the user to quickly determine what players/team are shooting the most, scoring the most, who is expected to score the most, who may be over/under performing in terms of scoring, etc. 

### Shot Location Visualization
This page generates plots that provide another method for the user to visualize the location of shots. The user has control over the groupings used, as well as the coloring and shapes of the data points. They also have the option to have all of the shots that resulted in goals be circled, for increased clarity.

The user changes the plots using the dropboxes on the lefthand side of the screen.
1. **Select the first Category to group by**: Indicates what variable should be grouped by first. If both grouping options are selected, this variable will appear
on the y-axis (vertically)
2. **Select the second Category to group by**: Indicates what variable should be grouped by second. If both grouping options are selected, this variable will appear
on the x-axis (horizontally)
3. **Select the variable that controls the color**: Indicates what variable controls the color of the data points. For example, if "Team" is selected, then all shots taken by Team A will be in one color, while all shots taken by Team B will be in a different color.
4. **Select the variable that controls the shape**: Indicates what variable controls the shape of the data points. For example, if "Team" is selected, then all shots taken by Team A will be one shape, while all shots taken by Team B will be a different shape.

The Legend for color and shape will appear to the right of the plot(s), once the options in question are selected.

*Note*: If neither grouping option is selected there will only be one plot. Also, it is possible to select "None" for the first group option and pick a real variable
for the second, in that case the variable selected second, will be treated as though it was selected first.

**Should goals be circled?**: If "Yes" is selected then all shots that resulted in goals will be circled. If "No" is selected then all shots will remain uncircled.

The variable options are as follows for each of the dropboxes:
1. **None**: This will leave the given dropbox option blank.
2. **Match Period**: Represents what part of the game the shot took place during. Includes the halves and extra time.
3. **Body Part**: What part of the body the shot was taken with.
4. **Shot is Goal**: Indicates whether or not the shot resulted in a goal.
5. **Shot on Target**: Indicates whether or not the shot was on target.
6. **Team Name**: The name of the team that took the shot.
7. **Player Name**: The name of the player that took the shot.
8. **Goal Zone**: Represents where the shot went on goal.

The following image shows a breakdown for **Goal Zone**:

![Goal Zones](Images/goal-zones.png)

This page provides users with options regarding how to view the data along with ways to break down the data spatially to identify trends and other patterns.

### Duel By Position Graph

**Purpose:** This visualization breaks down the different ground duels (offensive, defensive, dribble) by position. 

**Bar Graph Generation:** Utilizes ggplot2 to create the bar graph using the position of the player and the counts. Filtered by duels to and then picked out ground duels to analyze. 

**Aesthetics and Labels:** Used color scheme “Set2” and angled labels to ensure readability, along with changing readability.

**Reactivity:** You have the ability to switch between dribble, defensive, and offensive duels to get a more clear picture.


Generally, there will naturally be some positions that have more duels than others, defenders, for example. However, this plot will be useful to notice if some positions has more duels than they should have. This can help with planning and analysis of games. 

### Duel Heatmap

**Purpose**: Creates areas based on the amount of duels that occurred in different parts of the field. 

**Heatmap Generation**: Utilizes ggplot and then uses cut() to break the graph up and generates general areas instead of more specific points. 

**Aesthetic and Visuals**: Uses a scale gradient color scheme with black being the most amount of duels and white being the lease amount


The general idea behind the Duel Heatmap is to serve as a base to understand ground duels. Why are there duels happening in these areas? Why is there one edge of the field having a lot of ground duels? 


### Player Position

**Purpose**: Averages the x and y coordinates of the different player positions to see which part of the field they spend most of their time on. Allows coaches to see a general pattern and cross-reference it with the duels map to learn patterns. 

**Plot Generation**: Utilizes ggplot2 and the mean() function with the x and y coordinate and grouped by player position

**Aesthetics and visuals**: Adjusted the text to ensure readability. Did not add size to geom_point() to increase precision.

### Soccer Field Data Visualization:

**Purpose**: The point of this plot is to add a unique spin onto the visualizations and their representation. The overall plot, called Shot Attempts and Goals, illustrates the x and y coordinate of the data upon the position of the shot attempts Pitt had against the games they played. This allows for a better understanding of the positioning of where the shots were taken amongst the field view, rather than an x and y axis and a bunch of points scattered.

**Plot Generation**: Inside of Rstudio, I utilized the tidyverse library to integrate ggplot to create an aesthetically pleasing plot of the data. However, to create the numerous shapes and lines of the soccer field, I composed a function in order to do - which I called in the creation of the plot.

**Aesthetics and visuals**: Instead of a boring black and white plot, I decided to boost this appeal by incorporating colors and emphasizing text to make it bolder and bigger - to place significance of the important aspects to make it simpler for the audience to see.

