# Interactive App of Global University Rankings
# Final Project

This is the repo for our Final Project for Stats.220 on Global University Rankings. For the most up-to-date instructions, see [the course website](https://stat220-s25.github.io/portfolio/portfolio-4.html)

### link: [universities](https://ianchang2.shinyapps.io/map_app/)

### Files:

- `README.md` – This file, which explains the app and how to use the repo.
- `map_app` – Folder for main Shiny app.
  - `app.R` – Main Shiny app file that contains both UI and server logic.
  - `universities.csv` - Cleaned dataset used in the visualization.



## Link, Write-Up, and Repo Navigation:

To view the final report, follow this [link](https://ianchang2.shinyapps.io/map_app/) to get to the shiny app. This repo can be navigated by viewing the commit history within the main branch. The commit history includes notes on the types of changes made with each commit and displays the history of code along with any potential mishaps encountered along the way. 

The `README.md` file is this file which explains the app and how to use the repo. The file `country exploration with nav menu.R` is the main shiny app file which contains the UI and server. The app can be run by clicking the link above or opening `app.R` in Rstudio and clicking Run App.

## Write-Up:

For our final project, we created a shiny app visualizing exploratory dataset of global university rankings using three reputable international university ranking organizations. The goal of our app is to allow for the user to explore the data in various different ways and define their own interests in terms of the data information they would like to receive. In order to create this project, we obtained data from the university ranking organizations themselves and open-source data sets that scraped the data from the organizations’ websites. While we recognize that a second hand source is not entirely reliable, the information for these datasets is directly taken by those who sourced it from the ranking organizations. We also used a data set that provided coordinates for every country in the world and the states of the United States to help us accurately map things.

We had multiple goals for each section in the creation of our shiny app. For data wrangling, this is where we did the most work and encountered the most issues. Our data from the ARWU was separated within a folder to csv’s of each individual year so we created a function to iterate through the folder to read and organize the data into a complete csv altogether. From there, we joined, cleaned, and made the two other data sets consistent to where we were able to combine them into an entire data set that was used for the data analysis in the app. We had to manually go through many of the entry names and work with things such as the parsing down and through information.

Our goal with our visualizations was to provide ways of answering our initial five questions with exploratory elements for the user. We created our graphs with different purposes for each one and added an additional visual representation through a map. Our top 10 by range graph allows for a ranking organization to be chosen and year range to customize the time series you would like to view data for. The top universities by country tab allows you to pick through ranking organization, year, country, and the number of universities to pull to see the varying difference in the rankings universities get between and within countries. The map provides a visual look at the locations of the top universities across the globe to understand if some areas of the world are more likely than others to have a highly ranked university. The final tab of highest ranked takes the amount of highly ranked universities for each country across all three ranking organizations and visualizes it in the form of a stacked bar chart.

In terms of communication of the idea behind each graph, we created explanatory titles for each graph to explain the information contained inside. We also created an initial pop-up that provides a broad overview of what the app is and is meant for. In the about page, the user can learn more about where our data came from and the purpose behind our app as well as information such as us making it for a class. We hope that these comments provide the user with greater clarity on what things are while still giving them the freedom to explore without being fed direct information.

## Data:

The data come from the Kaggle users neosh11, Padhma Muniraj, and Jatin, as well as the ranking websites QS TopUniversities and World University Rankings 2025 | Times Higher Education (THE). 

The Academic Ranking of World Universities is created by ShanghaiRanking Consultancy, which is a fully independent organization on higher education and unaffiliated to any universities or government agencies. 
The Times Higher Education is an organization that has created rankings since 2004 to assess university performance and to provide a resource for readers to understand the different missions and successes of higher education institutions. 
QS Universities Rankings are created by QS TopUniversity, a company that creates rankings for multiple other organizations as well.

