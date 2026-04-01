# ==============================================================================
# 1. SETUP & DATA (With Automated Geo-Cleaning)
# ==============================================================================
library(shiny)
library(shinydashboard)
library(readxl)
library(lubridate)
library(plotly)
library(dplyr)
library(leaflet)
library(stringr) # For nice name formatting

# Load AQI Data
data <- read_excel("day_wise_aqi_data.xlsx")
data$year <- year(data$date)
data$month_name <- month(data$date, label = TRUE)

# Prepare AQI Area names for matching (Lowercase for robustness)
data <- data %>% mutate(match_area = tolower(area))

# Load External Coordinates (Using city_ascii Column B to avoid dots/diacritics)
# We rename 'city_ascii' to 'match_area' to join with our AQI data
city_coords <- read_xlsx("Indian cities.xlsx") %>%
  select(city_ascii, lat, lng) %>%
  rename(match_area = city_ascii) %>%
  mutate(match_area = tolower(match_area)) %>%
  distinct(match_area, .keep_all = TRUE) # Remove duplicates if any

# ==============================================================================
# 2. UI - POWER BI STYLE DARK GRID
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "AQI INDEX ANALYSIS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("year_select", "Select Year:", 
                  choices = c("All", sort(unique(data$year), decreasing = TRUE)), 
                  selected = 2025),
      selectInput("state_select", "Select State:", 
                  choices = c("All", sort(unique(data$state)))),
      selectInput("area_select", "Select Area:", 
                  choices = "All")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #0b0c10 !important; }
      .box { background-color: #1f2833; border-top: none; color: white; border-radius: 10px; }
      .box-header .box-title { color: #66fcf1; font-weight: bold; }
      .small-box { border-radius: 10px; }
    "))),
    
    fluidRow(
      # COLUMN 1: Gauge & Cards & City Bar
      column(width = 4,
             box(width = NULL, title = textOutput("selected_city_label"), 
                 plotlyOutput("aqi_gauge", height = "250px")),
             fluidRow(
               valueBoxOutput("max_aqi_card", width = 4),
               valueBoxOutput("pollutant_card", width = 4),
               valueBoxOutput("category_card", width = 4)
             ),
             box(width = NULL, title = "Top Polluted Cities (Ranked)", 
                 plotlyOutput("city_bar", height = "350px"))
      ),
      
      # COLUMN 2: Map & Donut
      column(width = 4,
             box(width = NULL, title = "Map of Selected Areas", 
                 leafletOutput("aqi_map", height = "400px")),
             box(width = NULL, title = "AQI Level Distribution", 
                 plotlyOutput("aqi_donut", height = "300px"))
      ),
      
      # COLUMN 3: Reference, Trend, & Pollutants
      column(width = 4,
             box(width = NULL, title = "AQI Level Reference", 
                 p("0-50 Good | 51-100 Satisfactory", style="color: #00E400; font-weight: bold;"),
                 p("101-200 Moderate | 201-300 Poor", style="color: #FFFF00; font-weight: bold;"),
                 p("301-400 Very Poor | 401+ Severe", style="color: #ff4c4c; font-weight: bold;")),
             box(width = NULL, title = "AQI Monthly Trend", 
                 plotlyOutput("aqi_trend", height = "250px")),
             box(width = NULL, title = "Prime Pollutant Frequency", 
                 plotlyOutput("pollutant_bar", height = "300px"))
      )
    )
  ),
  skin = "black"
)

# ==============================================================================
# 3. SERVER - DYNAMIC INTEGRATED LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive Base Filter
  filtered_df <- reactive({
    df <- data
    if (input$year_select != "All")  { df <- df[df$year == input$year_select, ] }
    if (input$state_select != "All") { df <- df[df$state == input$state_select, ] }
    if (input$area_select != "All")  { df <- df[df$area == input$area_select, ] }
    return(df)
  })
  
  # SHARED DATA LOGIC: Map and Bar Chart stay in sync
  display_data <- reactive({
    df <- filtered_df()
    
    # Logic: Show 1 City if selected, else Top 5 (National) or Top 3 (State)
    if(input$area_select != "All") {
      df_top <- df %>% 
        group_by(area, match_area) %>% 
        summarise(v = mean(aqi_value, na.rm = TRUE)) %>% 
        filter(area == input$area_select)
    } else {
      n_to_show <- if(input$state_select == "All") 5 else 3
      df_top <- df %>% 
        group_by(area, match_area) %>% 
        summarise(v = mean(aqi_value, na.rm = TRUE)) %>% 
        slice_max(v, n = n_to_show, with_ties = FALSE)
    }
    
    # Automated Join with cleaned coordinates
    df_top %>% left_join(city_coords, by = "match_area")
  })
  
  # Helper: Color Logic for Map
  getColor <- function(aqi) {
    sapply(aqi, function(x) {
      if(x <= 100) "green" else if(x <= 200) "orange" else "red"
    })
  }
  
  # Update Area Slicer when State changes
  observeEvent(input$state_select, {
    choices <- if(input$state_select == "All") unique(data$area) else unique(data$area[data$state == input$state_select])
    updateSelectInput(session, "area_select", choices = c("All", sort(choices)))
  })
  
  output$selected_city_label <- renderText({ input$area_select })
  
  # VISUAL 1: Gauge
  output$aqi_gauge <- renderPlotly({
    val <- round(mean(filtered_df()$aqi_value, na.rm = TRUE), 0)
    plot_ly(type = "indicator", mode = "gauge+number", value = val,
            gauge = list(axis = list(range = list(NULL, 500)),
                         bar = list(color = "white"),
                         steps = list(list(range = c(0, 100), color = "green"),
                                      list(range = c(101, 200), color = "yellow"),
                                      list(range = c(201, 500), color = "red")))) %>%
      layout(paper_bgcolor = "transparent", font = list(color = "white"), margin = list(t=40))
  })
  
  # VISUAL 2: Map (Dynamic Zoom & Colors)
  output$aqi_map <- renderLeaflet({
    map_df <- display_data()
    if(nrow(map_df) == 0 || all(is.na(map_df$lat))) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% setView(78.96, 20.59, 4))
    }
    leaflet(map_df) %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(~lng, ~lat, radius = 12, color = ~getColor(v),
                       label = ~paste0(area, ": ", round(v, 0)), stroke = TRUE, fillOpacity = 0.8)
  })
  
  # VISUAL 3: Slim Bar Chart
  output$city_bar <- renderPlotly({
    city_top <- display_data() %>% arrange(v)
    plot_ly(city_top, x = ~v, y = ~reorder(area, v), type = 'bar', orientation = 'h', 
            marker = list(color = "#ff4c4c")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"),
             bargap = 0.5, margin = list(l=120, r=20, t=20, b=40), xaxis = list(title = "Avg AQI"), yaxis = list(title = ""))
  })
  
  # VISUAL 4: Donut Chart
  output$aqi_donut <- renderPlotly({
    df <- filtered_df() %>% count(air_quality_status)
    plot_ly(df, labels = ~air_quality_status, values = ~n, type = 'pie', hole = 0.6) %>%
      layout(showlegend = FALSE, paper_bgcolor = "transparent", font = list(color = "white"))
  })
  
  # VISUAL 5: Monthly Trend
  output$aqi_trend <- renderPlotly({
    trend <- filtered_df() %>% group_by(month_name) %>% summarise(v = mean(aqi_value))
    plot_ly(trend, x = ~month_name, y = ~v, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy') %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"))
  })
  
  # VISUAL 6: Pollutant Bars
  output$pollutant_bar <- renderPlotly({
    pol <- filtered_df() %>% count(prominent_pollutants) %>% arrange(n) %>% tail(8)
    plot_ly(pol, x = ~n, y = ~reorder(prominent_pollutants, n), type = 'bar', orientation = 'h', 
            marker = list(color = "#66fcf1")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "white"), bargap = 0.4)
  })
  
  # VALUE BOXES
  output$max_aqi_card <- renderValueBox({ valueBox(max(filtered_df()$aqi_value, na.rm=T), "Max AQI", color = "red") })
  output$pollutant_card <- renderValueBox({ 
    top_p <- names(sort(table(filtered_df()$prominent_pollutants), decreasing = TRUE))[1]
    valueBox(top_p, "Prime", color = "orange") 
  })
  output$category_card <- renderValueBox({ 
    top_stat <- names(sort(table(filtered_df()$air_quality_status), decreasing = TRUE))[1]
    valueBox(top_stat, "Status", color = "yellow") 
  })
}

# ==============================================================================
# 4. RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)

