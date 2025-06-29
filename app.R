#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load necessary libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(ggforce)
library(tidytext)
library(treemap)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data
options(shiny.maxRequestSize = 100 * 1024^2) 

# Load your dataset
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Hospital Dashboard SPARS Data"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information Frequency", tabName = "information_frequency", icon = icon("chart-bar")),
      menuItem("Machine Learning", tabName = "machine_learning", icon = icon("robot")),
      menuItem("About Me", tabName = "about_me", icon = icon("user")),
      
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
  
  dashboardBody(
    tags$style(HTML("
      body {
        background-color: #D4D9DD;
      }
      .about-section {
        background-color: white;
        padding: 20px;
        border-radius: 10px;
      }
      .about-title {
        color: #2c3e50;
        font-size: 28px;
        font-weight: bold;
      }
      .about-text {
        color: #2c3e50;
        font-size: 16px;
      }
    ")),
    
    tabItems(
      
      # Information Frequency Tab ----
      tabItem(tabName = "information_frequency",
              fluidRow(
                valueBoxOutput("total_patient_count", width = 3),
                valueBoxOutput("total_patient_charge", width = 3),
                valueBoxOutput("total_patient_cost", width = 3),
                valueBoxOutput("avg_length_of_stay", width = 3)
              ),
              fluidRow(
                box(width = 7, title = "Top 3 Hospitals in Each Area",
                    plotOutput("top_hospitals_plot")),
                box(width = 5, title = "Mortality Risk Distribution",
                    plotOutput("mortality_risk_pie"))
              ),
              fluidRow(
                box(width = 5, title = "Top 7 Diagnoses Treemap",
                    plotOutput("diagnosis_treemap")),
                box(width = 7, title = "Patient Count by Age Group and Gender",
                    plotOutput("age_group_gender_plot"))
              )
      ),
      
      # Machine Learning Tab ----
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
      ),
      
      # ✅ About Me Tab ----
      tabItem(tabName = "about_me",
              fluidPage(
                div(class = "about-section",
                    fluidRow(
                      # ✅ Row 1: Image and About Me
                      column(3,
                             img(src = "profile.png", height = "250px", width = "250px", 
                                 style = "border-radius: 60%; border: 10px solid #2c3e50; margin-top: 50px;")
                      ),
                      column(9,
                             div(class = "about-title", "👨‍💻 About Me"),
                             br(),
                             div(class = "about-text",
                                 h4("Seyed Mohammad Khoshroo"),
                                 p("📍 Bologna, Italy"),
                                 p("💼 Data Scientist | Data Analyst | Healthcare Data Specialist"),
                                 p("I am a Data Analyst with over 4 years of experience in clinical trial data management, statistical modeling, and healthcare data analytics. I specialize in transforming clinical data into actionable insights for the pharmaceutical, healthcare, and research sectors."),
                                 p("Currently, I am pursuing my MSc in Statistical Science for Health and Population at the University of Bologna, focusing on mortality risk prediction using interpretable machine learning models."),
                                 p("This dashboard reflects my passion for blending statistical science with interactive visualizations to simplify complex hospital data into meaningful, intuitive insights.")
                            )
                      )
                    ),
                    
                    br(), hr(), br(),
                    
                    fluidRow(
                      # ✅ Row 2: Full-width text
                      column(12,
                             div(class = "about-title", "📊 About This Dashboard"),
                             div(class = "about-text",br(),
                                 p("- The 'Information Frequency' tab provides a comprehensive overview of patient distributions, hospital performances, severity breakdowns, and diagnosis patterns."),
                                 p("- The 'Machine Learning' tab enables users to apply logistic regression models to predict patient outcomes based on various clinical factors."),
                                 p("- The 'About Me' tab introduces the developer (me), my background, my motivation, and future goals."),
                                 br(),
                                 div(class = "about-title", "🚀 Future Development Plans"),br(),
                                 p("I am actively working on expanding this dashboard to integrate more advanced Machine Learning and AI-driven tools. This includes Decision Tree (DT) graphs, Neural Network visualizations, and interactive tools to explore feature relationships."),
                                 p("I am particularly interested in integrating survival analysis models to study time-to-event data, investigating post-COVID-19 effects on patient outcomes, and exploring dynamic frameworks to allow users to add their own data and view majestic visualizations of frequencies, relationships, and hidden patterns between variables."),
                                 p("My long-term goal is to transform this dashboard into an intelligent clinical analytics tool that supports decision-making through predictive modeling, pattern discovery, and automated reporting."),
                                 br(),
                                 div(class = "about-title", "🤝 Open for Collaboration & Job Opportunities"),br(),
                                 p("I am open to collaborate with companies, research institutes, and teams on projects in data science, healthcare analytics, machine learning, and statistical modeling. I am also actively searching for job opportunities in this area and eager to contribute to innovative projects with impactful outcomes."),
                                 br(),
                                 p("📧 Email: ", a("seyedmohammadkhoshroo@gmail.com", href="mailto:seyedmohammadkhoshroo@gmail.com")),
                                 p("🔗 LinkedIn: ", a("linkedin.com/in/seyed-mohammad-khoshroo-545127167", href="https://linkedin.com/in/seyed-mohammad-khoshroo-545127167", target="_blank")),
                                 p("🔗 GitHub: ", a("github.com/MohammadSmk72", href="https://github.com/MohammadSmk72", target="_blank"))
                                 
                             )
                      )
                    ),
                    hr(),
                    div(class = "about-text", "Made with ❤️ using R, Shiny, and a deep passion for data.")
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
      return(as.character(round(x, 1)))
    }
  }
  
  # Reactive filtered dataset
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
  
  # Value Box for Total Patients
  output$total_patient_count <- renderValueBox({
    total <- nrow(filtered_data())
    
    valueBox(
      value = format(total, big.mark = ".", decimal.mark = ","),
      subtitle = "Total Patients",
      icon = icon("user"),
      color = "light-blue"
    )
  })
  
  # Value Box for Total Patient Charges
  output$total_patient_charge <- renderValueBox({
    total_patient_charge <- sum(as.numeric(gsub(",", "", filtered_data()$Total.Charges)), na.rm = TRUE)
    total_charges_formatted <- format_large_numbers(total_patient_charge)
    
    valueBox(
      value = paste0("$", total_charges_formatted),
      subtitle = "Total Charges",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Value Box for Total Costs
  output$total_patient_cost <- renderValueBox({
    total_patient_cost <- sum(as.numeric(gsub(",", "", filtered_data()$Total.Costs)), na.rm = TRUE)
    total_cost_formatted <- format_large_numbers(total_patient_cost)
    
    valueBox(
      value = paste0("$", total_cost_formatted),
      subtitle = "Total Costs",
      icon = icon("money-bill"),
      color = "teal"
    )
  })
  
  # Value Box for Average Length of Stay
  output$avg_length_of_stay <- renderValueBox({
    avg_stay <- mean(as.numeric(filtered_data()$Length.of.Stay), na.rm = TRUE)
    
    valueBox(
      value = round(avg_stay, 1),
      subtitle = "Avg. Length of Stay (days)",
      icon = icon("bed"),
      color = "yellow"
    )
  })
  
  # Render summary text
  output$area_summary_text <- renderUI({
    total_patient_count <- nrow(filtered_data())
    
    summary_text <- paste0(
      "<div class='total-patient-text'>Total Patients: ", 
      format_large_numbers(total_patient_count), 
      "</div>"
    )
    
    HTML(summary_text)
  })
  
  
  output$top_hospitals_plot <- renderPlot({
    data <- filtered_data()
    
    top_hospitals <- data %>%
      group_by(Hospital.Service.Area, Facility.Name) %>%
      summarise(Patient_Count = n(), .groups = "drop") %>%
      arrange(Hospital.Service.Area, desc(Patient_Count)) %>%
      group_by(Hospital.Service.Area) %>%
      slice_max(order_by = Patient_Count, n = 3) %>%
      ungroup()
    
    top_hospitals <- top_hospitals %>%
      mutate(Area_Hospital = paste(Hospital.Service.Area, "-", Facility.Name))
    
    top_hospitals <- top_hospitals %>%
      mutate(Hospital_Short = trimws(sub(".*- ", "", Area_Hospital)))
    
    ggplot(top_hospitals, aes(x = reorder(Hospital_Short, Patient_Count), y = Patient_Count, fill = Hospital.Service.Area)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = Patient_Count),
                hjust = -0.1, color = "black", size = 4) +  
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "",
           x = NULL, y = "Patient Count") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 5),
        legend.title = element_blank(),
        legend.position = "bottom"
      ) +
      expand_limits(y = max(top_hospitals$Patient_Count) * 1.2)
  })
  
  output$mortality_risk_pie <- renderPlot({
    data <- filtered_data()
    
    # Group by Mortality Risk and count patients
    mortality_data <- data %>%
      group_by(APR.Risk.of.Mortality) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(
        fraction = Count / sum(Count),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1)),
        labelPosition = (ymax + ymin) / 2,
        label = paste0(APR.Risk.of.Mortality, "\n", round(fraction * 100, 1), "%")
      )
    
    # Set color palette (adjust as needed)
    colors <- c("#00B140", "#44A8D1", "#5e6eed", "#ff0854")  # For 4 levels: Minor, Moderate, Major, Extreme
    
    # Create the pie chart
    ggplot(mortality_data, aes(
      ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = APR.Risk.of.Mortality
    )) +
      geom_rect(color = "black") +  # Borders between segments
      
      geom_label(
        x = 4.2,
        aes(y = labelPosition, label = label),
        size = 3.5,
        fill = "#F1F2F6",
        color = "black",
        label.size = 0.4
      ) +
      
      coord_polar(theta = "y") +
      
      scale_fill_manual(values = colors) +
      
      xlim(c(2, 4)) +  # Adjust for donut effect
      
      theme_void() +
      
      theme(
        plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 2),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
      ) +
      
      labs(title = "")
  })
  
  output$age_group_gender_plot <- renderPlot({
    data <- filtered_data()
    
    age_group_data_gender <- data %>%
      group_by(Age.Group, Gender) %>%
      summarise(Count = n(), .groups = 'drop')
    
    colors_p <- c("#ff0854", "#1a55ec")  # Red and Blue for gender
    
    ggplot(age_group_data_gender, aes(x = Age.Group, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7,
               size = 0.1, linetype = "solid", alpha = 0.8) +
      scale_fill_manual(values = colors_p) +
      geom_text(aes(label = Count), 
                position = position_dodge(width = 0.7), 
                hjust = -0.05, color = "black", size = 3) +  # Horizontal label adjustment
      coord_flip() +  # Flip to horizontal bar chart
      theme_minimal(base_size = 15) +
      theme(
        axis.text.y = element_text(vjust = 0.5),
        plot.background = element_rect(fill = "#F1F2F6", color = "white", size = 5),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        axis.text.x = element_blank()
      )
    
    
  })  
  
  output$diagnosis_treemap <- renderPlot({
    data <- filtered_data()
    
    # Group by Diagnosis and count patients
    diagnosis_data <- data %>%
      group_by(CCSR.Diagnosis.Description) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count)) %>%
      slice_max(order_by = Count, n = 7)  # Select Top 7 diagnoses
    
    # Create the treemap
    treemap(
      diagnosis_data,
      index = "CCSR.Diagnosis.Description",  # Group by Diagnosis
      vSize = "Count",                       # Size of boxes based on patient count
      type = "index",                         # Just colored by index (group)
      palette = "Set3",                       # Color palette
      fontsize.labels = 14,                    # Label font size
      fontcolor.labels = "black",
      border.col = "white",                    # Border color between boxes
      title = ""
    )
  })
  
  
  # Machine Learning Model Output
  observeEvent(input$train_model, {
    output$ml_output <- renderPrint({
      data <- filtered_data()
      
      if (nrow(data) < 10) {
        return("Not enough data for training. Please adjust your filters.")
      }
      
      # Prepare data for logistic regression
      data <- data %>%
        mutate(Status = ifelse(Status == "Dead", 1, 0))
      
      # Simple logistic regression model
      model <- glm(Status ~ Age.Group + Race + APR.Severity.of.Illness.Description + APR.Medical.Surgical.Description,
                   data = data, family = "binomial")
      
      summary(model)
    })
  })
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run the app
shinyApp(ui = ui, server = server)