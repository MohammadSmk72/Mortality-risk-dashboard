# Load necessary libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)

# Read the dataset (make sure to update the path to your data)
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)

# Define UI (User Interface)
ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Hospital Dashboard"),
  
  # Sidebar layout
  dashboardSidebar(
    sidebarMenu(
      # Reorder the menu items
      menuItem("Descriptive Characteristics", tabName = "descriptive_char", icon = icon("info-circle")),
      menuItem("Hospital Patient Count", tabName = "patient_count", icon = icon("hospital")),
      menuItem("Surgical Frequency", tabName = "surgical_frequency", icon = icon("procedure"))
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
      # First Tab: Descriptive Characteristics
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
      
      # Second Tab: Hospital Patient Count
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
      
      # Third Tab: Surgical Frequency Table
      tabItem(tabName = "surgical_frequency",
              fluidRow(
                box(width = 12,
                    title = "Surgical Frequency",
                    tableOutput("surgical_frequency_table")
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
  
  # Render the bar chart for Age Group with enhanced design
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
  
  # Render the Severity table for the selected service area
  output$severity_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Create descriptive statistics for Severity
    severity_stats <- data.frame(
      Category = c("Extreme", "Major", "Minor", "Moderate"),
      N = c(sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Major"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate")),
      Percentage = c(
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Major") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate") / nrow(selected_area_data) * 100, 1)
      )
    )
    
    # Return the Severity table
    severity_stats
  })
  
  # Render the bar chart for Severity with enhanced design
  output$severity_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    severity_data <- selected_area_data %>%
      group_by(APR.Severity.of.Illness.Description) %>%
      summarise(Count = n())
    
    ggplot(severity_data, aes(x = APR.Severity.of.Illness.Description, y = Count, fill = APR.Severity.of.Illness.Description)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +  # Use a better color palette
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
  
  # Render the Surgical Status table for the selected service area
  output$surgical_status_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Create descriptive statistics for Surgical Status
    surgical_status_stats <- data.frame(
      Category = c("Non-Surgical", "Surgical"),
      N = c(sum(selected_area_data$Surgical.Status == "Non-Surgical"),
            sum(selected_area_data$Surgical.Status == "Surgical")),
      Percentage = c(
        round(sum(selected_area_data$Surgical.Status == "Non-Surgical") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Surgical.Status == "Surgical") / nrow(selected_area_data) * 100, 1)
      )
    )
    
    # Return the Surgical Status table
    surgical_status_stats
  })
  
  # Render the bar chart for Surgical Status with enhanced design
  output$surgical_status_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    surgical_data <- selected_area_data %>%
      group_by(Surgical.Status) %>%
      summarise(Count = n())
    
    ggplot(surgical_data, aes(x = Surgical.Status, y = Count, fill = Surgical.Status)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
      scale_fill_brewer(palette = "Pastel1") +  # Use a better color palette
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
  
  # Render the patient count for hospitals within the selected area
  output$patient_count_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    
    # Count the number of patients for each hospital (count rows per hospital)
    patient_count <- selected_area_data %>%
      group_by(Facility.Name) %>%
      summarise(PatientCount = n()) %>%
      arrange(desc(PatientCount))  # Sort by patient count in descending order
    
    # Return the data frame to display in the table
    patient_count
  })
  
  # Render the total patient count for the selected service area
  output$total_patient_count <- renderText({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    
    # Count the total number of patients in the selected service area
    total_patient_count <- nrow(selected_area_data)
    
    # Return the total patient count as text
    paste("Total number of patients in", input$area_patient, "is:", total_patient_count)
  })
  
  # Render the surgical frequency table
  output$surgical_frequency_table <- renderTable({
    
    # Group by surgical procedure and count the frequency
    surgical_frequency <- ny_hospdata %>%
      group_by(CCSR.Procedure.Description) %>%
      summarise(Frequency = n()) %>%
      arrange(desc(Frequency))  # Sort by frequency in descending order
    
    # Return the surgical frequency table
    surgical_frequency
  })
}

# Run the app
shinyApp(ui = ui, server = server)
