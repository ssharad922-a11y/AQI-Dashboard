# ==============================================================================
# 1. SETUP & DATA
# ==============================================================================
library(shiny)
library(shinydashboard)
library(readxl)
library(lubridate)
library(plotly)
library(dplyr)
library(leaflet)

data <- read_excel("day_wise_aqi_data.xlsx")
data$year <- year(data$date)
data$month_name <- month(data$date, label = TRUE)

# Custom coordinates for Indian Cities (since Excel usually lacks them)
# This allows the Map to work instantly.
city_coords <- data.frame(
  area = c("Delhi", "Mumbai", "Kolkata", "Chennai", "Hyderabad", "Bengaluru", "Jabalpur", "Chittoor"),
  lat = c(28.61, 19.07, 22.57, 13.08, 17.38, 12.97, 23.16, 13.21),
  lng = c(77.20, 72.87, 88.36, 80.27, 78.48, 77.59, 79.93, 79.10)
)

# ==============================================================================
# 2. UI - DARK THEME & GRID LAYOUT
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "AQI INDEX ANALYSIS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("year_select", "Year", choices = c("All", unique(data$year)), selected = 2025),
      selectInput("state_select", "State", choices = c("All", unique(data$state))),
      selectInput("area_select", "Area", choices = "All")
    )
  ),
  dashboardBody(
    # CUSTOM DARK CSS
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #0b0c10 !important; }
      .box { background-color: #1f2833; border-top: none; color: white; border-radius: 10px; }
      .box-header .box-title { color: #66fcf1; font-weight: bold; }
      .small-box { border-radius: 10px; }
    "))),
    
    # GRID SYSTEM (Following the image layout)
    fluidRow(
      # COLUMN 1: Gauge & Small Cards
      column(width = 4,
             box(width = NULL, title = textOutput("selected_city_label"), 
                 plotlyOutput("aqi_gauge", height = "250px")),
             fluidRow(
               valueBoxOutput("max_aqi_card", width = 4),
               valueBoxOutput("pollutant_card", width = 4),
               valueBoxOutput("category_card", width = 4)
             ),
             box(width = NULL, title = "AQI by City (Top 10)", 
                 plotlyOutput("city_bar", height = "350px"))
      ),
      
      # COLUMN 2: Map & Donut Chart
      column(width = 4,
             box(width = NULL, title = "AQI by State Map", 
                 leafletOutput("aqi_map", height = "400px")),
             box(width = NULL, title = "AQI Level Distribution", 
                 plotlyOutput("aqi_donut", height = "300px"))
      ),
      
      # COLUMN 3: Legend, Trend, & Pollutant Frequency
      column(width = 4,
             box(width = NULL, title = "AQL Level Reference", 
                 img(src = "https://i.imgur.com/your_legend_placeholder.png", width = "100%"), # Placeholder for color scale
                 p("Green: Good | Yellow: Mod | Red: Poor", style="font-size: 10px;")),
             box(width = NULL, title = "AQI Trend Over Time", 
                 plotlyOutput("aqi_trend", height = "250px")),
             box(width = NULL, title = "Pollutant Occurrence Frequency", 
                 plotlyOutput("pollutant_bar", height = "300px"))
      )
    )
  ),
  skin = "black"
)

# ==============================================================================
# 3. SERVER - REFRESHING ALL VISUALS
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive Filter
  filtered_df <- reactive({
    df <- data
    if (input$year_select != "All")  { df <- df[df$year == input$year_select, ] }
    if (input$state_select != "All") { df <- df[df$state == input$state_select, ] }
    if (input$area_select != "All")  { df <- df[df$area == input$area_select, ] }
    return(df)
  })
  
  # Dynamic Area Selection
  observeEvent(input$state_select, {
    choices <- if(input$state_select == "All") unique(data$area) else unique(data$area[data$state == input$state_select])
    updateSelectInput(session, "area_select", choices = c("All", sort(choices)))
  })
  
  output$selected_city_label <- renderText({ input$area_select })
  
  # 1. THE GAUGE
  output$aqi_gauge <- renderPlotly({
    val <- round(mean(filtered_df()$aqi_value, na.rm = TRUE), 0)
    plot_ly(type = "indicator", mode = "gauge+number", value = val,
            gauge = list(axis = list(range = list(NULL, 500)),
                         bar = list(color = "white"),
                         steps = list(list(range = c(0, 200), color = "red"), 
                                      list(range = c(200, 500), color = "white")))) %>%
      layout(paper_bgcolor = "#1f2833", font = list(color = "white"), margin = list(t=30, b=10))
  })
  
  # 2. THE MAP (Leaflet)
  output$aqi_map <- renderLeaflet({
    map_data <- filtered_df() %>% group_by(area) %>% summarise(val = mean(aqi_value)) %>%
      inner_join(city_coords, by = "area")
    
    leaflet(map_data) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(~lng, ~lat, radius = ~val/10, color = "orange", stroke = FALSE, fillOpacity = 0.8)
  })
  
  # 3. THE DONUT CHART
  output$aqi_donut <- renderPlotly({
    df <- filtered_df() %>% count(air_quality_status)
    plot_ly(df, labels = ~air_quality_status, values = ~n, type = 'pie', hole = 0.6) %>%
      layout(showlegend = FALSE, paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "white"))
  })
  
  # 4. TREND CHART
  output$aqi_trend <- renderPlotly({
    trend <- filtered_df() %>% group_by(month_name) %>% summarise(v = mean(aqi_value))
    plot_ly(trend, x = ~month_name, y = ~v, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"))
  })
  
  # 5. POLLUTANT FREQUENCY (Horizontal Bar)
  output$pollutant_bar <- renderPlotly({
    pol <- filtered_df() %>% count(prominent_pollutants) %>% arrange(n)
    plot_ly(pol, x = ~n, y = ~prominent_pollutants, type = 'bar', orientation = 'h', marker = list(color = "#66fcf1")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"))
  })
  
  # 6. TOP CITY BAR
  output$city_bar <- renderPlotly({
    city_top <- filtered_df() %>% group_by(area) %>% summarise(v = mean(aqi_value)) %>% slice_max(v, n = 10)
    plot_ly(city_top, x = ~v, y = ~area, type = 'bar', orientation = 'h', marker = list(color = "red")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"))
  })
  
  # CARDS
  output$max_aqi_card <- renderValueBox({ valueBox(max(filtered_df()$aqi_value), "Max AQI", color = "red") })
  output$pollutant_card <- renderValueBox({ valueBox("PM2.5", "Prime Pollutant", color = "orange") })
  output$category_card <- renderValueBox({ valueBox("Poor", "Category", color = "yellow") })
}

shinyApp(ui, server)

