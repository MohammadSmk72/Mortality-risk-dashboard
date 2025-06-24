# Load necessary libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(ggforce)

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
      body {
        background-color: #D4D9DD;  /* Light grayish-blue background */
      }
      .table, .table th, .table td {
        text-align: center;  /* Center content in the table */
        vertical-align: middle;  /* Vertically align text in the center */
      }
      .table th {
        text-align: center;  /* Center the column names (headers) */
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
                               plotOutput("surgical_status_chart")),
                      tabPanel("Race", 
                               tableOutput("race_table"), 
                               plotOutput("race_chart")),
                      tabPanel("Ethnicity", 
                               tableOutput("ethnicity_table"), 
                               plotOutput("ethnicity_chart"))
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
                valueBoxOutput("total_patient_count", width = 3),
                valueBoxOutput("total_charge_count", width = 3),
                box(width = 12,
                    tabsetPanel(
                      tabPanel(
                        "Age Group", plotOutput("age_group_chart_bar"), plotOutput("age_group_chart_pie")
                         ),
                      tabPanel(
                        "Severity", plotOutput("age_group_chart")
                        ),
                    )
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

#______________________________________________________________________________   
  
  # Render the Age Group table for the selected service area
  output$age_group_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)

    total_charges_by_group <- data.frame(
      Category = c("0-17","18–29", "30–49", "50–69", "70+"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "0 to 17"])), na.rm = TRUE),
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
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "0 to 17"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Age.Group == "0 to 17"] == "F", na.rm = TRUE)
      ),
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
    # Dead and Alive distribution calculation
    Dead_Alive_Distribution <- c(
      paste(
        sum(selected_area_data$Age.Group == "0 to 17" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Age.Group == "0 to 17" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Age.Group == "18 to 29" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Age.Group == "18 to 29" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Age.Group == "30 to 49" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Age.Group == "30 to 49" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Age.Group == "50 to 69" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Age.Group == "50 to 69" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Age.Group == "70 or Older" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Age.Group == "70 or Older" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      )
    )
    
    # Create descriptive statistics for Age Group
    age_group_stats <- data.frame(
      Category = c("0-17","18–29", "30–49", "50–69", "70+"),
      "Patient Count" = c(
            sum(selected_area_data$Age.Group == "0 to 17"),
            sum(selected_area_data$Age.Group == "18 to 29"),
            sum(selected_area_data$Age.Group == "30 to 49"),
            sum(selected_area_data$Age.Group == "50 to 69"),
            sum(selected_area_data$Age.Group == "70 or Older")),
      'Percentage' = c(
        round(sum(selected_area_data$Age.Group == "0 to 17") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "18 to 29") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "30 to 49") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "50 to 69") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Age.Group == "70 or Older") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "0 to 17"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "18 to 29"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "30 to 49"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "50 to 69"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Age.Group == "70 or Older"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "0 to 17"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "18 to 29"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "30 to 49"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "50 to 69"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "70 or Older"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group$Percentage, # Use the percentage calculated from pie chart logic
      
      'Dead/Alive Patients' = Dead_Alive_Distribution 
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
      Category = c("0-17","18–29", "30–49", "50–69", "70+"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Age.Group == "0 to 17"])), na.rm = TRUE),
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
      scale_fill_brewer(palette = "Set3") +   # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right")  +  # Move legend to the right
      labs(title = "Percentage of Total Charges by Age Group") +  # Add the title
      theme(plot.title = element_text(hjust = .5, vjust = -7)) 
    
    
       gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
#______________________________________________________________________________   
  
  # Render the Severity table for the selected service area
  output$severity_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    total_charges_by_group_s <- data.frame(
      Category = c("Extreme", "Major", "Minor", "Moderate"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Major"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Minor"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"])), na.rm = TRUE)
      )
    )
    # Calculate the total charges across all age groups
    total_charges_all <- sum(total_charges_by_group_s$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category based on the total charges
    total_charges_by_group_s$Percentage <- round(total_charges_by_group_s$Total_Charges / total_charges_all * 100, 1)
    
    # Gender distribution calculation
    Gender_Distribution <- c(
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Major"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Major"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Minor"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Minor"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"] == "F", na.rm = TRUE)
      )
    )
    # Dead and Alive distribution calculation
    Dead_Alive_Distribution <- c(
      paste(
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Major" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Major" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      )
    )
    # Create descriptive statistics for Severity
    severity_stats <- data.frame(
      Category = c("Extreme", "Major", "Minor", "Moderate"),
      'Patient Count' = c(sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Major"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor"),
            sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate")),
      Percentage = c(
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Extreme") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Major") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Minor") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Severity.of.Illness.Description == "Moderate") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Severity.of.Illness.Description == "Major"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Severity.of.Illness.Description == "Minor"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Major"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Minor"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group_s$Percentage, # Use the percentage calculated from pie chart logic
      'Dead/Alive Patients' = Dead_Alive_Distribution 
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
    
    p1 <- ggplot(severity_data, aes(x = APR.Severity.of.Illness.Description, y = Count, fill = APR.Severity.of.Illness.Description)) +
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
    # Pie Chart: Percentage of Total Charges
    total_charges_by_group_s <- data.frame(
      Category = c("Extreme", "Major", "Minor", "Moderate"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Extreme"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Major"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Minor"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Severity.of.Illness.Description == "Moderate"])), na.rm = TRUE)
      )
    )
    total_charges_all <- sum(total_charges_by_group_s$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category
    total_charges_by_group_s$Percentage <- round(total_charges_by_group_s$Total_Charges / total_charges_all * 100, 1)
    
    # Pie chart for total charges percentage
    p2 <- ggplot(total_charges_by_group_s, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Makes it a pie chart
      scale_fill_brewer(palette = "Set3") +   # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right")  +  # Move legend to the right
      labs(title = "Percentage of Total Charges by Severity") +  # Add the title
      theme(plot.title = element_text(hjust = .5, vjust = -7)) 
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)

  })
  
#______________________________________________________________________________    
  
  # Render the Surgical Status table for the selected service area
  output$surgical_status_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    total_charges_by_group_m <- data.frame(
      Category = c("Medical", "Surgical"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Medical"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Surgical"])), na.rm = TRUE)
      )
    )
    # Calculate the total charges across all age groups
    total_charges_all <- sum(total_charges_by_group_m$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category based on the total charges
    total_charges_by_group_m$Percentage <- round(total_charges_by_group_m$Total_Charges / total_charges_all * 100, 1)
    
    # Gender distribution calculation
    Gender_Distribution <- c(
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Medical.Surgical.Description == "Medical"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Medical.Surgical.Description == "Medical"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$APR.Medical.Surgical.Description == "Surgical"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$APR.Medical.Surgical.Description == "Surgical"] == "F", na.rm = TRUE)
      )
    )
    # Dead and Alive distribution calculation
    Dead_Alive_Distribution <- c(
      paste(
        sum(selected_area_data$APR.Medical.Surgical.Description == "Medical" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Medical.Surgical.Description == "Medical" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      )
    )
    
    # Create descriptive statistics for Surgical Status
    surgical_status_stats <- data.frame(
      Category = c("Medical", "Surgical"),
      'Patient Count' = c(sum(selected_area_data$APR.Medical.Surgical.Description == "Medical"),
            sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical")),
      Percentage = c(
        round(sum(selected_area_data$APR.Medical.Surgical.Description == "Medical") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$APR.Medical.Surgical.Description == "Surgical") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Medical.Surgical.Description == "Medical"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$APR.Medical.Surgical.Description == "Surgical"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Medical"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Surgical"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group_m$Percentage, # Use the percentage calculated from pie chart logic
      'Dead/Alive Patients' = Dead_Alive_Distribution 
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
    
    p1 <- ggplot(surgical_data, aes(x = APR.Medical.Surgical.Description, y = Count, fill = APR.Medical.Surgical.Description)) +
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
    # Pie Chart: Percentage of Total Charges
    total_charges_by_group_m <- data.frame(
      Category = c("Medical", "Surgical"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Medical"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$APR.Medical.Surgical.Description == "Surgical"])), na.rm = TRUE)
      )
    )
    total_charges_all <- sum(total_charges_by_group_m$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category
    total_charges_by_group_m$Percentage <- round(total_charges_by_group_m$Total_Charges / total_charges_all * 100, 1)
    
    # Pie chart for total charges percentage
    p2 <- ggplot(total_charges_by_group_m, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Makes it a pie chart
      scale_fill_brewer(palette = "Set3") +   # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right")  +  # Move legend to the right
      labs(title = "Percentage of Total Charges by Severity") +  # Add the title
      theme(plot.title = element_text(hjust = .5, vjust = -7)) 
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    
  })
  
  #_____________________________________________________________________________
  
  # Render the Race table for the selected service area
  output$race_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    total_charges_by_group_r <- data.frame(
      Category = c("Black/African American", "Multi-racial", "Other Race", "White"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Black/African American"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Multi-racial"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Other Race"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "White"])), na.rm = TRUE)
      )
    )
    
    # Calculate the total charges across all age groups
    total_charges_all <- sum(total_charges_by_group_r$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category based on the total charges
    total_charges_by_group_r$Percentage <- round(total_charges_by_group_r$Total_Charges / total_charges_all * 100, 1)
    
    # Gender distribution calculation
    Gender_Distribution <- c(
      paste(
        sum(selected_area_data$Gender[selected_area_data$Race == "Black/African American"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Race == "Black/African American"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Race == "Multi-racial"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Race == "Multi-racial"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Race == "Other Race"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Race == "Other Race"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Race == "White"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Race == "White"] == "F", na.rm = TRUE)
      )
    )
    Dead_Alive_Distribution <- c(
      paste(
        sum(selected_area_data$Race == "Black/African American" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Race == "Black/African American" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Race == "Multi-racial" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Race == "Multi-racial" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Race == "Other Race" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Race == "Other Race" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Race == "White" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Race == "White" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      )
    )
    # Create descriptive statistics for Age Group
    race_group_stats <- data.frame(
      Category = c("Black/African American", "Multi-racial", "Other Race", "White"),
      "Patient Count" = c(sum(selected_area_data$Race == "Black/African American"),
                          sum(selected_area_data$Race == "Multi-racial"),
                          sum(selected_area_data$Race == "Other Race"),
                          sum(selected_area_data$Race == "White")),
      'Percentage' = c(
        round(sum(selected_area_data$Race == "Black/African American") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Race == "Multi-racial") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Race == "Other Race") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Race == "White") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$Race == "Black/African American"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Race == "Multi-racial"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Race == "Other Race"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Race == "White"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Black/African American"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Multi-racial"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Other Race"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "White"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group_r$Percentage, # Use the percentage calculated from pie chart logic
      'Dead/Alive Patients' = Dead_Alive_Distribution 
    )
    
    # Return the Age Group table
    race_group_stats
  })
  
  # Render the bar chart for Age Group with enhanced design
  output$race_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    race_group_data <- selected_area_data %>%
      group_by(Race) %>%
      summarise(Count = n())
    # Bar chart for total patient count
    p1 <- ggplot(race_group_data, aes(x = Race, y = Count, fill = Race)) +
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
    total_charges_by_group_r <- data.frame(
      Category = c("Black/African American", "Multi-racial", "Other Race", "White"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Black/African American"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Multi-racial"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "Other Race"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Race == "White"])), na.rm = TRUE)
      )
    )
    total_charges_all <- sum(total_charges_by_group_r$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category
    total_charges_by_group_r$Percentage <- round(total_charges_by_group_r$Total_Charges / total_charges_all * 100, 1)
    
    # Pie chart for total charges percentage
    p2 <- ggplot(total_charges_by_group_r, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Makes it a pie chart
      scale_fill_brewer(palette = "Set3") +   # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right")  +  # Move legend to the right
      labs(title = "Percentage of Total Charges by Race") +  # Add the title
      theme(plot.title = element_text(hjust = .5, vjust = -7)) 
    
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  #____________________________________________________________________________
  
  # Render the Ethnicity table for the selected service area
  output$ethnicity_table <- renderTable({
    
    # Filter the data for the selected area
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    total_charges_by_group_e <- data.frame(
      Category = c("Multi-ethnic", "Not Span/Hispanic", "Spanish/Hispanic", "Unknown"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Multi-ethnic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Not Span/Hispanic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Spanish/Hispanic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Unknown"])), na.rm = TRUE)
      )
    )
    
    # Calculate the total charges across all age groups
    total_charges_all <- sum(total_charges_by_group_e$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category based on the total charges
    total_charges_by_group_e$Percentage <- round(total_charges_by_group_e$Total_Charges / total_charges_all * 100, 1)
    
    # Gender distribution calculation
    Gender_Distribution <- c(
      paste(
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Multi-ethnic"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Multi-ethnic"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Not Span/Hispanic"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Not Span/Hispanic"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Spanish/Hispanic"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Spanish/Hispanic"] == "F", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Unknown"] == "M", na.rm = TRUE),
        "/",
        sum(selected_area_data$Gender[selected_area_data$Ethnicity == "Unknown"] == "F", na.rm = TRUE)
      )
    )
    Dead_Alive_Distribution <- c(
      paste(
        sum(selected_area_data$Ethnicity == "Multi-ethnic" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Ethnicity == "Multi-ethnic" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Ethnicity == "Not Span/Hispanic" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Ethnicity == "Not Span/Hispanic" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Ethnicity == "Spanish/Hispanic" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Ethnicity == "Spanish/Hispanic" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      ),
      paste(
        sum(selected_area_data$Ethnicity == "Unknown" & selected_area_data$Patient.Disposition == "Expired", na.rm = TRUE),
        "/",
        sum(selected_area_data$Ethnicity == "Unknown" & selected_area_data$Patient.Disposition != "Expired", na.rm = TRUE)
      )
    )
    # Create descriptive statistics for Age Group
    ethnicity_group_stats <- data.frame(
      Category = c("Multi-ethnic", "Not Span/Hispanic", "Spanish/Hispanic", "Unknown"),
      "Patient Count" = c(sum(selected_area_data$Ethnicity == "Multi-ethnic"),
                          sum(selected_area_data$Ethnicity == "Not Span/Hispanic"),
                          sum(selected_area_data$Ethnicity == "Spanish/Hispanic"),
                          sum(selected_area_data$Ethnicity == "Unknown")),
      'Percentage' = c(
        round(sum(selected_area_data$Ethnicity == "Multi-ethnic") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Ethnicity == "Not Span/Hispanic") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Ethnicity == "Spanish/Hispanic") / nrow(selected_area_data) * 100, 1),
        round(sum(selected_area_data$Ethnicity == "Unknown") / nrow(selected_area_data) * 100, 1)
      ),
      'Gender Distribution_M/F' = Gender_Distribution,
      'Average Length of Stay' = c(
        mean(selected_area_data$Length.of.Stay[selected_area_data$Ethnicity == "Multi-ethnic"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Ethnicity == "Not Span/Hispanic"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Ethnicity == "Spanish/Hispanic"], na.rm = TRUE),
        mean(selected_area_data$Length.of.Stay[selected_area_data$Ethnicity == "Unknown"], na.rm = TRUE)
      ),
      'Total Charges' = c(
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Multi-ethnic"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Not Span/Hispanic"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Spanish/Hispanic"])), na.rm = TRUE)),
        format_large_numbers(sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Unknown"])), na.rm = TRUE))
      ),
      'Percentage of Total Charges' = total_charges_by_group_e$Percentage, # Use the percentage calculated from pie chart logic
      'Dead/Alive Patients' = Dead_Alive_Distribution 
    )
    
    # Return the Age Group table
    ethnicity_group_stats
  })
  
  # Render the bar chart for Age Group with enhanced design
  output$ethnicity_chart <- renderPlot({
    selected_area_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
    
    ethnicity_group_data <- selected_area_data %>%
      group_by(Ethnicity) %>%
      summarise(Count = n())
    # Bar chart for total patient count
    p1 <- ggplot(ethnicity_group_data, aes(x = Ethnicity, y = Count, fill = Ethnicity)) +
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
    total_charges_by_group_e <- data.frame(
      Category = c("Multi-ethnic", "Not Span/Hispanic", "Spanish/Hispanic", "Unknown"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Multi-ethnic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Not Span/Hispanic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Spanish/Hispanic"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "", selected_area_data$Total.Charges[selected_area_data$Ethnicity == "Unknown"])), na.rm = TRUE)
      )
    )
    total_charges_all <- sum(total_charges_by_group_e$Total_Charges)  # Total charges for all groups
    
    # Calculate percentage for each category
    total_charges_by_group_e$Percentage <- round(total_charges_by_group_e$Total_Charges / total_charges_all * 100, 1)
    
    # Pie chart for total charges percentage
    p2 <- ggplot(total_charges_by_group_e, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Makes it a pie chart
      scale_fill_brewer(palette = "Set3") +   # Use a better color palette
      theme_void() +  # Remove gridlines and background
      theme(legend.position = "right")  +  # Move legend to the right
      labs(title = "Percentage of Total Charges by Ethnicity") +  # Add the title
      theme(plot.title = element_text(hjust = .5, vjust = -7)) 
    
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  
  #______________________________________________________________________________    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#_______________________________________________________________________________  
  
  # Render the patient count for hospitals within the selected area
  output$total_patient_count <- renderValueBox({
    req(input$area_patient)
    
    selected_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    total <- nrow(selected_data)
   
    # Create valueBox for total patient count
    valueBox(
      value = format(total, big.mark = ".", decimal.mark = ","),
      subtitle = "Patients",
      icon = icon("user"),
      color = "light-blue"
    )
  })
  output$total_charge_count <- renderValueBox({
    req(input$area_patient)
    
    selected_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    # Count the total number of patients in the selected service area
    total_patient_charge <- sum(as.numeric(gsub(",", "", selected_data$Total.Charges)), na.rm = TRUE)
    # Format the total charges to be more readable (in millions or billions)
    total_charges_formatted <- format_large_numbers(total_patient_charge)
    # Create valueBox for total patient count
    valueBox(
      value = format(total_charges_formatted, big.mark = ".", decimal.mark = ","),
      subtitle = "Total Charge",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })

#______________________________________________________________________________  
  output$age_group_chart_bar <- renderPlot({
    
    selected_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    
    # Prepare the data for Age Group
    age_group_data <-     selected_data %>%
      group_by(Age.Group) %>%
      summarise(Count = n())
    
    # Define gradient colors for bar chart
    colors <- c('#5e6eed')
    
    # Create the bar chart (age group count)
    p1 <- ggplot(age_group_data, aes(x = Count, y = Age.Group, fill = Count)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE, width = 0.4, 
               size = 0.1, linetype = "solid", alpha = 0.8) +  # Adjust the width and transparency of the bars
      scale_fill_gradientn(colors = colors) +  # Apply the gradient colors
      geom_text(aes(label = Count), hjust = .5, color = "black", size = 4,nudge_x =0,  # Add a horizontal margin (space to the right of the labels)
                nudge_y = .35) +  # Add labels at the end of the bars
      theme_minimal(base_size = 15) +
      ggtitle("Total Count of Patients by Age Group") +
      theme(
        axis.text.y = element_text(angle = 0, hjust = 1.2),
        plot.background = element_rect(fill = "#F1F2F6", color = "white",size = 5),  # Set background color for the plot area
        panel.grid = element_blank(),  # Remove grid lines
        axis.title = element_blank(),  # Remove axis titles
        plot.title = element_blank(),  # Remove plot title
        axis.text.x = element_blank()  # Remove x-axis labels
      )  
    
    # Second Bar Chart (same as p2)
    age_group_data_gender <- selected_data %>%
      group_by(Age.Group, Gender) %>%
      summarise(Count = n(), .groups = 'drop')  # Summarize by Age Group and Gender
    
    # Define the colors for the Gender categories
    colors_p <- c("#ff0854","#1a55ec")
    
    # Create the bar chart with the specified colors
    p2 <- ggplot(age_group_data_gender, aes(x = Age.Group, y = Count, fill = Gender)) +  # Fill by Gender
      geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7, 
               size = 0.1, linetype = "solid", alpha = 0.8) +  # Stacked bars for each gender (side-by-side)
      scale_fill_manual(values = colors_p) +  # Use the defined colors for the bars
      geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.5, color = "black", size = 4) +  # Add labels at the top of the bars
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 0, vjust = 5),  # Rotate x-axis labels for better visibility
        plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 5),  # Set background color for the plot area
        panel.grid = element_blank(),  # Remove grid lines
        axis.title = element_blank(),  # Remove axis titles
        plot.title = element_blank(),  # Remove plot title
        axis.text.y = element_blank()  # Remove y-axis labels
      )
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)# Add padding between plots
  })
  
#----------------------------------------------------------------------Pie Chart
  output$age_group_chart_pie <- renderPlot({
    selected_data <- filter(ny_hospdata, Hospital.Service.Area == input$area_patient)
    
    # Prepare the data for Age Group
    age_group_data <- selected_data %>%
      group_by(Age.Group) %>%
      summarise(Count = n())
    
    # Define gradient colors for bar chart
    colors <- c("green", "lightblue", "#5e6eed", "#ff0854","#1a55ec")
    
    # Prepare the data for Pie Chart (Total Charges by Age Group)
    total_charges_by_group <- data.frame(
      Category = c("0-17","18–29", "30–49", "50–69", "70+"),
      Total_Charges = c(
        sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$Age.Group == "0 to 17"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$Age.Group == "18 to 29"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$Age.Group == "30 to 49"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$Age.Group == "50 to 69"])), na.rm = TRUE),
        sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$Age.Group == "70 or Older"])), na.rm = TRUE)
      )
    )
    
    # Calculate the total charges
        total_charges_all <- sum(total_charges_by_group$Total_Charges)
    
    # Calculate the percentage for each category
        total_charges_by_group$Percentage <- round(total_charges_by_group$Total_Charges / total_charges_all * 100, 1)
    
        p1 <- ggplot(total_charges_by_group, aes(x = "", y = Percentage, fill = Category)) +  # Use Category for fill
          geom_bar(stat = "identity", width = 5, color = "black") +
          coord_polar(theta = "y") +  # Makes it a pie chart
          scale_fill_manual(values = colors) +  # Apply the custom colors to each category
          theme_void() +  # Remove gridlines and background
          theme(
            plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 10),  # Set background color for the plot area
            legend.position = "none",  # Hide the legend
            plot.title = element_text(hjust = 0.5, vjust = -1),  # Center the title
            plot.title.position = "plot",  # Ensure the title stays within the plot area
            legend.title = element_blank(),  # Remove legend title (if needed)
            legend.text = element_text(size = 14)  # Adjust the size of legend text (if needed)
          ) +
          labs(title = "Percentage of Total Charges by Age Group")
        
       p2 <- ggplot(total_charges_by_group, aes(x = "", y = Percentage, fill = Category)) +  # Use Category for fill
         geom_bar(stat = "identity", width = 5, color = "black") +
         coord_polar(theta = "y") +  # Makes it a pie chart
         scale_fill_manual(values = colors) +  # Apply the custom colors to each category
         theme_void() +  # Remove gridlines and background
         theme(
           plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 10),  # Set background color for the plot area
           legend.position = "none",  # Hide the legend
           plot.title = element_text(hjust = 0.5, vjust = -1),  # Center the title
           plot.title.position = "plot",  # Ensure the title stays within the plot area
           legend.title = element_blank(),  # Remove legend title (if needed)
           legend.text = element_text(size = 14)  # Adjust the size of legend text (if needed)
         ) +
         labs(title = "Percentage of Total Charges by Age Group")
       
       p3 <- ggplot(total_charges_by_group, aes(x = "", y = Percentage, fill = Category)) +  # Use Category for fill
         geom_bar(stat = "identity", width = 5, color = "black") +
         coord_polar(theta = "y") +  # Makes it a pie chart
         scale_fill_manual(values = colors) +  # Apply the custom colors to each category
         theme_void() +  # Remove gridlines and background
         theme(
           plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 10),  # Set background color for the plot area
           legend.position = "none",  # Hide the legend
           plot.title = element_text(hjust = 0.5, vjust = -1),  # Center the title
           plot.title.position = "plot",  # Ensure the title stays within the plot area
           legend.title = element_blank(),  # Remove legend title (if needed)
           legend.text = element_text(size = 14)  # Adjust the size of legend text (if needed)
         ) +
         labs(title = "Percentage of Total Charges by Age Group")
       
    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)# Add padding between plots
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#_____________________________________________________________________________
    output$severity_chart_hist <- renderPlot({
      
          selected_data <- filter(ny_hospdata, Hospital.Service.Area == input$area)
      
      severity_data <-     selected_data %>%
        group_by(APR.Severity.of.Illness.Description) %>%
        summarise(Count = n())
      # Define the color gradient you want (from your image)
      colors <- c("green", "light-blue", "#5e6eed", "#4b3f7d", "#6e4d85")
      
      p1 <- ggplot(severity_data, aes(x = Count, y = APR.Severity.of.Illness.Description, fill = Count)) +
        geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
        scale_fill_gradientn(colors = colors) +  # Apply the gradient colors
        geom_text(aes(label = Count), hjust = -0.05, color = "black", size = 5) +  # Place labels at the end of bars
        theme_minimal(base_size = 20) +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1, margin = margin(r = -25)),  # Adjust margin on the right
          panel.grid = element_blank(),  # Remove grid lines
          axis.title = element_blank(),  # Remove axis titles
          plot.title = element_blank(),  # Remove plot title
          axis.text.x = element_blank()  # Hide horizontal x-axis numbers
        ) +
        scale_x_continuous(expand = c(0, 5000))  # Increase space at the right (500 is adjustable)
      
      
      # Pie Chart: Percentage of Total Charges
      total_charges_by_group_s <- data.frame(
        Category = c("Extreme", "Major", "Minor", "Moderate"),
        Total_Charges = c(
          sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$APR.Severity.of.Illness.Description == "Extreme"])), na.rm = TRUE),
          sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$APR.Severity.of.Illness.Description == "Major"])), na.rm = TRUE),
          sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$APR.Severity.of.Illness.Description == "Minor"])), na.rm = TRUE),
          sum(as.numeric(gsub(",", "",     selected_data$Total.Charges[    selected_data$APR.Severity.of.Illness.Description == "Moderate"])), na.rm = TRUE)
        )
      )
      total_charges_all <- sum(total_charges_by_group_s$Total_Charges)  # Total charges for all groups
      
      # Calculate percentage for each category
      total_charges_by_group_s$Percentage <- round(total_charges_by_group_s$Total_Charges / total_charges_all * 100, 1)
      
      # Pie chart for total charges percentage
      p2 <- ggplot(total_charges_by_group_s, aes(x = "", y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar(theta = "y") +  # Makes it a pie chart
        scale_fill_brewer(palette = "Set3") +   # Use a better color palette
        theme_void() +  # Remove gridlines and background
        theme(legend.position = "right")  +  # Move legend to the right
        labs(title = "Percentage of Total Charges by Severity") +  # Add the title
        theme(plot.title = element_text(hjust = .5, vjust = -7)) 
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
      
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
