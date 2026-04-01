# AQI-Dashboard
This project is a high-performance, interactive data visualization dashboard built using R and the Shiny framework. It  analyses over 425,000+ rows of Indian Air Quality data spanning 10 years.
The goal of this project was to transition a complex static dataset into a "Live" analytical tool. It provides a deep dive into the air quality trends across 32 Indian States and 295+ Cities, allowing users to filter by Year, State, and specific Area to see real-time recalculated metrics.
Reactive Filtering Engine: Handles a dataset of 425,279 rows with near-instant recalculations using R's reactive logic.

KPI Scorecards: Real-time calculation of Average AQI, Overall Status (Good, Poor, Severe), and Monitoring Station counts.

Interactive Gauge: A custom-coded Plotly Gauge that updates its needle and color-coded zones based on Indian AQI standards.

Geospatial Analysis: An interactive Leaflet Map with a "Dark Matter" theme, plotting city-wise air quality intensity.

Trend Analysis: A monthly historical trend line to visualize seasonal fluctuations in pollution over the last decade.

Categorical Insights: Donut charts for AQI Level Distribution and horizontal bar charts for Pollutant Frequency.
