# Load necessary libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(geojsonio)
# Read the dataset (update the file path accordingly)
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)  # Update path to your dataset

# Assuming you have a GeoJSON or Shape file for the hospital service areas
# Load the GeoJSON file for the service area map (update the file path accordingly)
service_area_geojson <- "hospital_service_areas.geojson"

# Define UI (User Interface)
ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Hospital Dashboard"),
  
  # Sidebar layout
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Characteristics", tabName = "descriptive_char", icon = icon("info-circle")),
      menuItem("Hospital Patient Count", tabName = "patient_count", icon = icon("hospital")),
      menuItem("Surgical Frequency", tabName = "surgical_frequency", icon = icon("procedure")),
      menuItem("Service Area Map", tabName = "service_area_map", icon = icon("map"))
    )
  ),
  
  # Body of the dashboard
  dashboardBody(
    tags$style(HTML("
      .total-patient-text {
        font-size: 30px;  /* Set font size to 30px */
        font-weight: bold; /* Make the text bold */
        color: #2c3e50;  /* Set a dark color for better readability */
      }
    ")),
    
    tabItems(
      # Descriptive Characteristics Tab
      tabItem(tabName = "descriptive_char",
              fluidRow(
                box(width = 12,
                    title = "Descriptive Characteristics of Hospital Service Area",
                    selectInput("area", "Choose a Hospital Service Area:", 
                                choices = unique(ny_hospdata$Hospital.Service.Area),
                                selected = unique(ny_hospdata$Hospital.Service.Area)[1]),
                    tabsetPanel(
                      tabPanel("Age Group", 
                               tableOutput("age_group_table"), 
                               plotOutput("age_group_chart")),
                      tabPanel("Severity", 
                               tableOutput("severity_table"), 
                               plotOutput("severity_chart")),
                      tabPanel("Surgical Status", 
                               tableOutput("surgical_status_table"), 
                               plotOutput("surgical_status_chart"))
                    )
                )
              )),
      
      # Hospital Patient Count Tab
      tabItem(tabName = "patient_count",
              fluidRow(
                box(width = 6,
                    selectInput("area_patient", "Choose a Hospital Service Area:", 
                                choices = unique(ny_hospdata$Hospital.Service.Area),
                                selected = unique(ny_hospdata$Hospital.Service.Area)[1])
                ),
                box(width = 12,
                    div(textOutput("total_patient_count"), class = "total-patient-text"),
                    tableOutput("patient_count_table")
                )
              )),
      
      # Surgical Frequency Tab
      tabItem(tabName = "surgical_frequency",
              fluidRow(
                box(width = 12,
                    title = "Surgical Frequency",
                    tableOutput("surgical_frequency_table")
                )
              )),
      
      # Service Area Map Tab
      tabItem(tabName = "service_area_map",
              fluidRow(
                box(width = 12,
                    title = "Interactive Service Area Map",
                    leafletOutput("service_area_map")
                )
              ))
    )
  )
)

# Define server function
server <- function(input, output, session) {
  
  # Ensure input$area is initialized correctly in Descriptive Characteristics tab
  observe({
    if (is.null(input$area) || input$area == "") {
      updateSelectInput(session, "area", selected = unique(ny_hospdata$Hospital.Service.Area)[1])
    }
  })
  
  # Render the map for the selected Hospital Service Area
  output$service_area_map <- renderLeaflet({
    # Read the geojson file to create the map
    service_area_data <- geojsonio::geojson_read(service_area_geojson, what = "sp")
    
    # Map the service areas and color the selected area
    leaflet(service_area_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~ifelse(service_area_data$Hospital.Service.Area == input$area,
                                      "#FF5733", "#CCCCCC"),  # Highlight the selected service area in red
                  color = "#000000", weight = 2, opacity = 1, fillOpacity = 0.7,
                  popup = ~Hospital.Service.Area) %>%
      setView(lng = -74.0060, lat = 40.7128, zoom = 10)  # Center on New York City, you may need to adjust coordinates
  })
  
  # Render the Age Group table for the selected service area
  output$age_group_table <- renderTable({
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Create descriptive statistics for Age Group
    age_group_stats <- data.frame(
      Category = c("18–29", "30–49", "50–69", "70+"),
      N = c(sum(selected_area_data$Age.Group == "18 to 29"),
            sum(selected_area_data$Age.Group == "30 to 49"),
            sum(selected_area_data$Age.Group == "50 to 69"),
            sum(selected_area_data$Age.Group == "70 or Older")),
      Percentage = c(
        round(sum(selected_area_data$Age.Group == "18 to 29") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "30 to 49") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "50 to 69") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "70 or Older") / nrow(selected_area_data) * 100, 1)
      )
    )
    
    # Return the Age Group table
    age_group_stats
  })
  
  # Render the bar chart for Age Group
  output$age_group_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    age_group_data <- selected_area_data %>%
      group_by(Age.Group) %>%
      summarise(Count = n())
    
    ggplot(age_group_data, aes(x = Age.Group, y = Count, fill = Age.Group)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
      scale_fill_brewer(palette = "Set3") +  # Use a better color palette
      geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 5) +  # Add data labels
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
        panel.grid = element_blank(),  # Remove grid lines
        axis.title = element_blank(),  # Remove axis titles
        plot.title = element_blank(),  # Remove plot title
        axis.text.y = element_blank()  # Remove vertical y-axis numbers
      )
  })
  
  # The rest of the code remains the same for the severity and surgical status sections
}

# Run the app
shinyApp(ui = ui, server = server)



#rsconnect::deployApp()

