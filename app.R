# Load necessary libraries
library(shiny)
library(dplyr)

# Read the dataset (update the file path accordingly)
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)  # Update path to your dataset

# Define UI (User Interface)
ui <- fluidPage(
  titlePanel("Hospital Patient Count Dashboard"),
  
  # Sidebar layout for input controls
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select Hospital Service Area
      selectInput("area", "Choose a Hospital Service Area:", 
                  choices = unique(ny_hospdata$Hospital.Service.Area),  # Unique values from the dataset
                  selected = unique(ny_hospdata$Hospital.Service.Area)[1])  # Default to first area
    ),
    
    # Main panel for displaying output
    mainPanel(
      # Text output for showing patient count per hospital
      tableOutput("patient_count_table")
    )
  )
)

# Define server function
server <- function(input, output) {
  
  # Render the patient count for hospitals within the selected area
  output$patient_count_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Count the number of patients for each hospital (count rows per hospital)
    patient_count <- selected_area_data %>%
      group_by(Facility.Name) %>%
      summarise(PatientCount = n()) %>%
      arrange(desc(PatientCount))  # Sort by patient count in descending order
    
    # Return the data frame to display in the table
    patient_count
  })
}

# Run the app
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()

