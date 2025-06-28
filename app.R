#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load necessary libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(ggforce)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data
options(shiny.maxRequestSize = 100 * 1024^2) 

# Load your dataset
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)

# Create a Status column based on Patient.Disposition (Expired = Dead, else Alive)
ny_hospdata <- ny_hospdata %>%
  mutate(Status = ifelse(Patient.Disposition == "Expired", "Dead", "Alive"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define UI (User Interface)
ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Hospital Dashboard"),
  
  # Sidebar with Vertical Filters and Navigation
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information Frequency", tabName = "information_frequency", icon = icon("info-circle")),
      menuItem("Machine Learning", tabName = "machine_learning", icon = icon("robot")),
      
      # Filters apply to both tabs
      selectInput("area_service_patient", "Hospital Service Area:", 
                  choices = unique(ny_hospdata$Hospital.Service.Area),
                  selected = unique(ny_hospdata$Hospital.Service.Area),
                  multiple = TRUE),
      
      selectInput("age_group_patient", "Age Group:", 
                  choices = unique(ny_hospdata$Age.Group),
                  selected = unique(ny_hospdata$Age.Group),
                  multiple = TRUE),
      
      selectInput("severity_patient", "Severity:", 
                  choices = unique(ny_hospdata$APR.Severity.of.Illness.Description),
                  selected = unique(ny_hospdata$APR.Severity.of.Illness.Description),
                  multiple = TRUE),
      
      selectInput("race_patient", "Race:", 
                  choices = unique(ny_hospdata$Race),
                  selected = unique(ny_hospdata$Race),
                  multiple = TRUE),
      
      checkboxGroupInput("surgical_patient", "Surgical:",
                         choices = unique(ny_hospdata$APR.Medical.Surgical.Description),
                         selected = unique(ny_hospdata$APR.Medical.Surgical.Description)),
      
      checkboxGroupInput("status_patient", "Patient Outcome:",
                         choices = c("Alive", "Dead"),
                         selected = c("Alive", "Dead"))
    )
  ),
  
  # Body of the dashboard
  dashboardBody(
    tags$style(HTML("
      body {
        background-color: #D4D9DD;
      }
      .table, .table th, .table td {
        text-align: center;
        vertical-align: middle;
      }
      .table th {
        text-align: center;
      }
      .total-patient-text {
        font-size: 30px;
        font-weight: bold;
        color: #2c3e50;
      }
    ")),
    
    tabItems(
      # First Tab: Information Frequency
      tabItem(tabName = "information_frequency",
              fluidRow(
                box(width = 12,
                    title = "Hospital Patient Statistics for Diseases and Disorders of the Circulatory System in New York City",
                    uiOutput("area_summary_text")
                )
              )
      ),
      
      # Machine Learning Tab
      tabItem(tabName = "machine_learning",
              fluidRow(
                box(width = 4, title = "Machine Learning Inputs",
                    selectInput("ml_target", "Select Target Variable:",
                                choices = c("Status"),
                                selected = "Status"),
                    
                    actionButton("train_model", "Train Logistic Regression Model")
                ),
                box(width = 8, title = "Model Output",
                    verbatimTextOutput("ml_output")
                )
              )
      )
    )
  )
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define server function
server <- function(input, output, session) {
  
  # Helper function to format large numbers
  format_large_numbers <- function(x) {
    if (x >= 1e9) {
      return(paste0(round(x / 1e9, 2), "B"))
    } else if (x >= 1e6) {
      return(paste0(round(x / 1e6, 2), "M"))
    } else if (x >= 1e3) {
      return(paste0(round(x / 1e3, 2), "K"))
    } else {
      return(as.character(x))
    }
  }
  
  # Create a reactive filtered dataset
  filtered_data <- reactive({
    ny_hospdata %>%
      filter(
        Hospital.Service.Area %in% input$area_service_patient,
        Age.Group %in% input$age_group_patient,
        APR.Severity.of.Illness.Description %in% input$severity_patient,
        APR.Medical.Surgical.Description %in% input$surgical_patient,
        Race %in% input$race_patient,
        Status %in% input$status_patient
      )
  })
  
  # Render the summary text for the selected filters
  output$area_summary_text <- renderUI({
    total_patient_count <- nrow(filtered_data())
    
    summary_text <- paste0(
      "<div class='total-patient-text'>Total Patients: ", 
      format_large_numbers(total_patient_count), 
      "</div>"
    )
    
    HTML(summary_text)
  })
  
  # Machine Learning Model Output
  observeEvent(input$train_model, {
    output$ml_output <- renderPrint({
      data <- filtered_data()
      
      if (nrow(data) < 10) {
        return("Not enough data for training. Please adjust your filters.")
      }
      
      # Prepare the data
      data <- data %>%
        mutate(Status = ifelse(Status == "Dead", 1, 0))  # Convert to binary outcome
      
      # Simple logistic regression example
      model <- glm(Status ~ Age.Group + Race + APR.Severity.of.Illness.Description + APR.Medical.Surgical.Description,
                   data = data, family = "binomial")
      
      summary(model)
    })
  })
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run the app
shinyApp(ui = ui, server = server)
