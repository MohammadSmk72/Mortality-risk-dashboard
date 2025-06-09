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
      .table, .table th, .table td {
        text-align: center;  /* Center content in the table */
        vertical-align: middle;  /* Vertically align text in the center */
      }
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
                    title = "Hospital Patient Statistics for Diseases and Disorders of the Circulatory System in New York City",
                    selectInput("area", "Choose a Hospital Service Area:", 
                                choices = unique(ny_hospdata$Hospital.Service.Area),
                                selected = unique(ny_hospdata$Hospital.Service.Area)[1]),
                    uiOutput("area_summary_text"),
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
                box(width = 12,
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
  
  format_large_numbers <- function(x) {
    if (x >= 1e9) {
      return(paste0(round(x / 1e9, 2), "B"))  # Convert to billions
    } else if (x >= 1e6) {
      return(paste0(round(x / 1e6, 2), "M"))  # Convert to millions
    } else if (x >= 1e3) {
      return(paste0(round(x / 1e3, 2), "K"))  # Convert to thousands
    } else {
      return(as.character(x))  # Keep the number as is for smaller values
    }
  }
  # Ensure input$area is initialized correctly in Descriptive Characteristics tab
  observe({
    if (is.null(input$area) || input$area == "") {
      updateSelectInput(session, "area", selected = unique(ny_hospdata$Hospital.Service.Area)[1])
    }
  })
  # Render the summary text for the selected service area
  output$area_summary_text <- renderUI({
    # Filter the data based on the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    # Get total patient count, total charges, and average length of stay
    total_patient_count <- nrow(selected_area_data)
    total_charges <- sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges)), na.rm = TRUE)
    # Format the total charges to be more readable (in millions or billions)
    total_charges_formatted <- format_large_numbers(total_charges)
    
    # Create the summary text
    summary_text <- paste(
      "Summary for", input$area, "Hospital Service Area:",
      "<br>Total Patients: ", total_patient_count,
      "<br>Total Charges: ", total_charges_formatted,
      "<br> - - - - - - - - - - - - - - - - - - "
    )
    
    # Return the summary text as HTML with line breaks
    HTML(summary_text)
  })
  
  # Render the Age Group table for the selected service area
  output$age_group_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
 
    
    total_charges_by_group <- data.frame(
      Category = c("18–29", "30–49", "50–69", "70+"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "18 to 29"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "30 to 49"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "50 to 69"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "70 or Older"])), na.rm = TRUE)
      )
    )
    
    # Calculate the total charges across all age groups
    total_charges_all <- sum(total_charges_by_group$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category based on the total charges
    total_charges_by_group$Percentage <- round(total_charges_by_group$Total_Charges / total_charges_all * 100, 1)
    
    # Gender distribution calculation
    Gender_Distribution <- c(
      paste(
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "18 to 29"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "18 to 29"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "30 to 49"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "30 to 49"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "50 to 69"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "50 to 69"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "70 or Older"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "70 or Older"] == "F", na.rm = TRUE)
      )
    )
    # Create descriptive statistics for Age Group
    age_group_stats <- data.frame(
      Category = c("18–29", "30–49", "50–69", "70+"),
      "Patient Count" = c(sum(selected_area_data$Age.Group == "18 to 29"),
            sum(selected_area_data$Age.Group == "30 to 49"),
            sum(selected_area_data$Age.Group == "50 to 69"),
            sum(selected_area_data$Age.Group == "70 or Older")),
      'Percentage' = c(
        round(sum(selected_area_data$Age.Group == "18 to 29") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "30 to 49") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "50 to 69") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "70 or Older") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "18 to 29"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "30 to 49"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "50 to 69"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "70 or Older"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "18 to 29"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "30 to 49"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "50 to 69"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "70 or Older"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group$Percentage # Use the percentage calculated from pie chart logic
      
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
    # Bar chart for total patient count
    p1 <- ggplot(age_group_data, aes(x = Age.Group, y = Count, fill = Age.Group)) +
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
    # Pie Chart: Percentage of Total Charges
    total_charges_by_group <- data.frame(
      Category = c("18–29", "30–49", "50–69", "70+"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "18 to 29"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "30 to 49"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "50 to 69"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "70 or Older"])), na.rm = TRUE)
      )
    )
    total_charges_all <- sum(total_charges_by_group$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category
    total_charges_by_group$Percentage <- round(total_charges_by_group$Total_Charges / total_charges_all * 100, 1)
    
    # Pie chart for total charges percentage
    p2 <- ggplot(total_charges_by_group, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Makes it a pie chart
      scale_fill_brewer(palette = "Set3") +  # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right") 
    # Arrange the bar chart and pie chart side by side
    gridExtra::grid.arrange(p1, p2, ncol = 2)
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
      Category = c("Medical", "Surgical"),
      N = c(sum(selected_area_data$APR.Medical.Surgical.Description == "Medical"),
            sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical")),
      Percentage = c(
        round(sum(selected_area_data$APR.Medical.Surgical.Description == "Medical") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical") / nrow(selected_area_data) * 100, 1)
      )
    )
    
    # Return the Surgical Status table
    surgical_status_stats
  })
  
  # Render the bar chart for Surgical Status with enhanced design
  output$surgical_status_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    surgical_data <- selected_area_data %>%
      group_by(APR.Medical.Surgical.Description) %>%
      summarise(Count = n())
    
    ggplot(surgical_data, aes(x = APR.Medical.Surgical.Description, y = Count, fill = APR.Medical.Surgical.Description)) +
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
