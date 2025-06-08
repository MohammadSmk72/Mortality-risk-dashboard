# Load necessary libraries
library(shiny)
library(dplyr)

# Read the dataset (update the file path accordingly)
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)  # Update path to your dataset


# Define UI (User Interface)
ui <- navbarPage("Hospital Dashboard",  # Title of the app
                 
                 # First Tab: Hospital Patient Count
                 tabPanel("Hospital Patient Count",
                          sidebarLayout(
                            sidebarPanel(
                              # Dropdown menu to select Hospital Service Area
                              selectInput("area", "Choose a Hospital Service Area:", 
                                          choices = unique(ny_hospdata$Hospital.Service.Area),
                                          selected = unique(ny_hospdata$Hospital.Service.Area)[1])  # Default to first area
                            ),
                            
                            mainPanel(
                              # Text output for the total count of patients in the selected area
                              div(textOutput("total_patient_count"), class = "total-patient-text"),
                              
                              # Text output for showing patient count per hospital
                              tableOutput("patient_count_table")
                            )
                          )),
                 
                 # Second Tab: Surgical Frequency Table
                 tabPanel("Surgical Frequency",
                          mainPanel(
                            tableOutput("surgical_frequency_table")
                          ))
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
  
  # Render the total patient count for the selected service area
  output$total_patient_count <- renderText({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Count the total number of patients in the selected service area
    total_patient_count <- nrow(selected_area_data)
    
    # Return the total patient count as text
    paste("Total number of patients in", input$area, "is:", total_patient_count)
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

#rsconnect::deployApp()

