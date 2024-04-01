# Pitt Soccer Analytics App

# We should probably get rid of the Variable data types part of the upload JSON page

Welcome! 

This app is designed to provide insight about collegiate soccer through the use of visual plots and tables that summarize key statistics.

## How to use the App

### Uploading Data
First the app needs data to analyze. To upload data:
1. Navigate to the tab labelled **Upload JSON** in the top row.
2. Click on the box labelled **Browse...** on the lefthand side of the page
3. Select the Wayscout Event level data JSON file that contains the game(s) that you would like to analyze.
4. There should be a blue bar under the **Browse...** button that says "Upload complete", along with information in the "Games in upload" section on the right; If so, that indicates that the data has been successfully uploaded into the app.

With the data uploaded now you can use the other tabs in the top row of the app to view the data in different ways.

## Tabs

### Probability Heatmap Function
This function is designed to analyze and visualize scoring probabilities based on shot locations, particularly focusing on shots made during a game. The visualization is in the form of a heatmap that represents different areas of the soccer field.

The algorithm combines the scoring probabilities of all games in a single dataset to ensure the greatest confidence in the probability of scoring at different position of the field.

1. **Heatmap Generation**: Utilizes ggplot2 to create a heatmap. Each tile's color intensity represents the scoring probability for that area, with the color gradient ranging from low (blue) to high (red) scoring probabilities.
2. **Aesthetics and Labels**: The heatmap includes a color scale to indicate scoring probabilities, minimalistic theme styling, and a title.
3. **Reactivity**: The heatmap updates reactively to changes in the input data, ensuring that the visualization always reflects the current dataset.

The output of this function is a heatmap visualization that provides insight into which areas of the playing field have higher or lower probabilities of scoring. This information can be crucial for teams and coaches to develop strategies, train players, and analyze opponents' defensive weaknesses.

### Shots (Probably going to remove this and the pass one)
####  Goal Information (maybe rename to Shot Information Table)
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
