# Load necessary libraries for UI, data handling, and visualization
library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(dplyr)
library(gitcreds)
library(plotly)
library(leaflet)

# Load and clean data
universities <- readr::read_csv("universities.csv") %>%
  mutate(
    source = stringr::str_trim(source),
    country = stringr::str_trim(country),
    university = stringr::str_trim(university)
  )

# Get unique ranking organizations for dropdown options
rankingOrg_choices <- unique(universities$source)


# Define the User Interface ----------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Global University Rankings"),
  
  # Sidebar with navigation menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "Data"),
      menuItem("Top 10 by Range", tabName = "Top10"),
      menuItem("Top Universities by Country", tabName = "TopCountry"),
      menuItem("Map", tabName = "Map"),
      menuItem("Highest Rank", tabName = "HighestRank"),
      menuItem("About", tabName = "About")
    )
  ),
  
  # Main content for each tab
  dashboardBody(
    tabItems(
      
      # Raw dataset view
      tabItem(tabName = "Data",
              fluidRow(
                box(DT::DTOutput("data")),
                box(title = "Select",
                    selectInput("rankingOrg", "Select Ranking Organization:",
                                choices = sort(unique(universities$source))),
                    sliderInput("yearSingle", "Select Year:",
                                min = 2003, max = 2025, value = 2003, step = 2, sep = "")
                )
              )
      ),
      
      # Top 10 universities by selected year range
      tabItem(tabName = "Top10",
              fluidRow(
                box(title = "Select", width = 12,
                    selectInput("rankingOrg", "Select Ranking Organization:",
                                choices = sort(unique(universities$source))),
                    sliderInput("yearRange", "Select Year Range:",
                                min = 2003, max = 2025,
                                value = c(2003, 2003),
                                step = 2, sep = "")
                ),
                box(plotOutput("top10_plot", height = "500px"), width = 12)
              )
      ),
      
      # Top universities in a specific country for a given year
      tabItem(tabName = "TopCountry",
              fluidRow(
                box(plotOutput("top_country_plot")),
                box(title = "Select",
                    selectInput("rankingOrg", "Select Ranking Organization:",
                                choices = sort(unique(universities$source))),
                    sliderInput("yearSingle_country", "Select Year:",
                                min = 2003, max = 2025, value = 2003, step = 2, sep = ""),
                    selectInput("country", "Choose a country:",
                                choices = sort(unique(universities$country))),
                    sliderInput("number", "Choose Top _ Universities:",
                                min = 1, max = 100, value = 10, step = 1, ticks = FALSE, sep = "")
                )
              )
      ),
      
      # Leaflet map showing university rankings geographically
      tabItem(tabName = "Map",
              fluidRow(
                box(leafletOutput("map")),
                box(title = "Select",
                    selectInput("rankingOrg", "Select Ranking Organization:",
                                choices = sort(unique(universities$source))),
                    sliderInput("yearSingle_map", "Select Year:",
                                min = 2003, max = 2025, value = 2003, step = 2, sep = ""),
                    sliderInput("number", "Choose Top _ Universities:",
                                min = 1, max = 100, value = 10, step = 1, ticks = FALSE, sep = "")
                )
              )
      ),
      
      # Plotly stacked area chart showing share of top N universities by country
      tabItem(tabName = "HighestRank",
              fluidRow(
                box(plotlyOutput("highest_rank", height = "600px"), width = 12)
              )
      ),
      
      # About tab describing the project and data sources
      tabItem(tabName = "About",
              fluidRow(
                box(
                  title = "About the Global University Ranking App",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  p("This interactive app, Global University Rankings, was created for the final project in STAT.220: Data Science at Carleton College, taught by Professor Amanda Luby. This app allows users to explore global university rankings over time and in various different ways. Each tab seeks to offer an answer to an exploratory question with mutatable elements that allow the user to come up with new questions that can be answered as well."),
                  p("Our data comes from Kaggle users neosh11, Padhma Muniraj, and Jatin, as well as the ranking websites QS TopUniversities and World University Rankings 2025 | Times Higher Education (THE). While it would have been preferrable to get all the data directly from the ranking organizations, many of the websites removed data past a certain point and the data sets found online scraped by users accounted for any of the years we could not obtain directly. The Academic Ranking of World Universities is created by ShanghaiRanking Consultancy, which is a fully independent organization on higher education and unaffiliated to any universities or government agencies. The Times Higher Education is an organzation that has created rankings since 2004 to assess university performance and to provide a resource for readers to understand the different missions and successes of higher education institutions. QS Universities Rankings are created by QS TopUniversity, a company that creates ranking for multiple other organizations as well."),
                  p("Use the tabs to view rankings by country, organization, year range, and map visualizations. These tabs offer different options for interactivity that you can use with."),
                  p("This app was created by Ian, Evelin, and Mary-Kathryn as part of a team project for STAT.220. We hope you enjoy exploring the data!")
                )
              )
      )
    )
  )
)

# Define server logic -------------------------------------------------
server <- function(input, output, session) {
  
  # Show welcome message on app load
  observe({
    showModal(modalDialog(
      p("This app displays different statistics about global university rankings from three reputable international ranking organizations."),
      title = "Global University Ranking Exploration",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Dynamically update year sliders based on selected ranking organization
  observeEvent(input$rankingOrg, {
    years_for_source <- universities %>%
      filter(source == input$rankingOrg) %>%
      pull(year)
    
    min_year <- min(years_for_source, na.rm = TRUE)
    max_year <- max(years_for_source, na.rm = TRUE)
    
    updateSliderInput(session, "yearRange",
                      min = min_year, max = max_year, value = c(min_year, max_year))
    
    updateSliderInput(session, "yearSingle",
                      min = min_year, max = max_year, value = min_year)
    
    updateSliderInput(session, "yearSingle_country",
                      min = min_year, max = max_year, value = min_year)
    
    updateSliderInput(session, "yearSingle_map",
                      min = min_year, max = max_year, value = min_year)
  })
  
  
  # Update country dropdown based on selected org and year
  observeEvent(c(input$rankingOrg, input$yearSingle), {
    available_countries <- universities %>%
      filter(source == input$rankingOrg, year == input$yearSingle) %>%
      pull(country) %>% unique() %>% sort()
    if (length(available_countries) > 0) {
      updateSelectInput(session, "country", choices = available_countries, selected = available_countries[1])
    }
  })
  
  
  # Dynamically update 'Top N' slider based on available data
  observeEvent(c(input$rankingOrg, input$yearSingle, input$country), {
    country_unis <- universities %>%
      filter(source == input$rankingOrg,
             year == input$yearSingle,
             country == input$country)
    max_unis <- nrow(country_unis)
    updateSliderInput(session, "number",
                      max = ifelse(max_unis > 0, max_unis, 1),
                      value = min(input$number, max_unis))
  })
  
  # Reactive datasets for selected filters
  rankingOrg_data_single <- reactive({
    universities %>% filter(source == input$rankingOrg, year == input$yearSingle)
  })
  
  rankingOrg_data_range <- reactive({
    universities %>% filter(source == input$rankingOrg, year >= input$yearRange[1], year <= input$yearRange[2])
  })
  
  # Render data table
  output$data <- DT::renderDataTable({
    rankingOrg_data_single() %>% select(university, country, rank, year)
  })
  
  
  # Render Top 10 plot
  output$top10_plot <- renderPlot({
    latest_year <- input$yearRange[2]
    plot_data <- rankingOrg_data_range() %>% group_by(year) %>% slice_min(rank, n = 10) %>% ungroup()
    latest_ranks <- plot_data %>% filter(year == latest_year) %>% select(university, rank) %>% distinct()
    plot_data <- plot_data %>%
      left_join(latest_ranks, by = "university", suffix = c("", "_latest")) %>%
      mutate(rank_latest = ifelse(is.na(rank_latest), 999, rank_latest)) %>%
      mutate(university = forcats::fct_reorder(university, rank_latest))
    ggplot(plot_data, aes(x = year, y = rank, color = university)) +
      geom_line(size = 1) + geom_point() +
      labs(title = paste0(input$rankingOrg, " Top 10 Universities (", input$yearRange[1], "–", input$yearRange[2], ")"),
           x = "Year", y = "Rank", color = "University") +
      theme_minimal() + scale_y_reverse(breaks = 1:10)
  })
  
  
  # Render top universities in selected country
  output$top_country_plot <- renderPlot({
    df <- universities %>%
      filter(source == input$rankingOrg,
             country == input$country,
             year == input$yearSingle) %>%
      arrange(rank) %>%
      slice_head(n = input$number)
    
    ggplot(df, aes(x = reorder(university, rank), y = rank)) +
      geom_segment(aes(x = university, xend = university, y = max(rank) + 2, yend = rank), color = "gray") +
      geom_point(size = 4, color = "steelblue") +
      coord_flip() +
      scale_y_reverse() +
      labs(title = paste("Top", input$number, "Universities in", input$country),
           x = "University", y = "Rank") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 9)
      )
  })
  
  # Render interactive map
  output$map <- renderLeaflet({
    selector_uni_data <- universities %>%
      filter(source == input$rankingOrg, year == input$yearSingle) %>%
      arrange(rank) %>%
      slice_head(n = input$number)
    
    map_data <- selector_uni_data %>%
      group_by(latitude, longitude) %>%
      summarize(
        popup_text = paste0(
          "<strong>", university, "</strong> (Rank: ", rank, ")",
          collapse = "<br>"
        ),
        .groups = "drop"
      )
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = "red",
        fillOpacity = 0.7,
        popup = ~popup_text
      ) %>%
      setView(
        lng = mean(map_data$longitude, na.rm = TRUE),
        lat = mean(map_data$latitude, na.rm = TRUE),
        zoom = 1.5
      )
  })
  
  
  # Render stacked bar plot of top countries in top 100
  output$highest_rank <- renderPlotly({
    top_countries <- universities %>%
      filter(rank <= 100) %>%
      count(country, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      pull(country)
  
    filtered_data <- universities %>%
      filter(rank <= 100, country %in% top_countries) %>%
      group_by(year, country) %>%
      summarize(n_top = n(), .groups = "drop")
  
    p <- ggplot(filtered_data, aes(x = year, y = n_top, fill = country,
                                   text = country)) +
      geom_col(position = "fill") +
      labs(title = "Top 100 Universities by Country (Top 10 Countries)",
           x = "Year", y = "Share of Top 100 Universities", fill = "Country") +
      theme_minimal() +
      scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
      scale_fill_viridis_d(option = "D") +
      theme(legend.position = "right")
  
    ggplotly(p, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)