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
library(rpart)
library(rpart.plot)
library(caret)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data
options(shiny.maxRequestSize = 100 * 1024^2) 

# Load your dataset
ny_hospdata <- read.csv("hospdata_2022.csv", header = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define UI
ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$div(
      tags$div("SPARCS Explorer", 
               style = "font-size: 20px; font-weight: bold; line-height: 1.8;"),
      tags$div("Circulatory Conditions – NY Inpatient",
               style = "font-size: 12px; color: #f0f0f0;line-height: .5;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information Frequency", tabName = "information_frequency", icon = icon("chart-bar")),
      menuItem("Machine Learning", tabName = "machine_learning", icon = icon("robot")),
      menuItem("About Me", tabName = "about_me", icon = icon("user")),
      
      selectInput("area_service_patient", "Hospital Service Area:", 
                  choices = unique(ny_hospdata$Hospital.Service.Area),
                  selected = unique(ny_hospdata$Hospital.Service.Area)[7],
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
      
      #---------------- Information Frequency ----------------#
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
      
      #---------------- Machine Learning ----------------#
      tabItem(tabName = "machine_learning",
              box(width = 12, title = "Model Setup",
                  fluidRow(
                    column(width = 3,
                           tags$strong("Response Variable:"),
                           p("Status (Alive = 0, Dead = 1)", style = "color: #555;")
                    ),
                    column(width = 9,
                           checkboxGroupInput("ml_predictors", "Select Predictor Variables:", 
                                              choices = c("Age.Group", "APR.Severity.of.Illness.Description",
                                                          "APR.Medical.Surgical.Description", "APR.Risk.of.Mortality", "Length.of.Stay"),
                                              selected = c("Age.Group", "APR.Severity.of.Illness.Description", 
                                                           "APR.Medical.Surgical.Description", "APR.Risk.of.Mortality", "Length.of.Stay"),
                                              inline = TRUE)
                    )
                  )
              ),
              
              tabBox(width = 12, title = NULL,
                     
                     tabPanel("General Linear Model",
                              fluidRow(
                                box(width = 12,
                                    valueBoxOutput("model_lrt", width = 3),
                                    valueBoxOutput("model_accuracy", width = 3),
                                    valueBoxOutput("model_aic", width = 3),
                                    valueBoxOutput("model_bic", width = 3))
                              ),
                              fluidRow(
                                column(width = 7,
                                       box(title = "Model Coefficients Table", width = 12,
                                           DT::dataTableOutput("ml_table"))
                                ),
                                column(width = 5,
                                       box(title = "ROC Curve", width = 12,
                                           plotOutput("roc_plot", height = "375px"))
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       box(title = "Interpretation of Results", width = 12,
                                           htmlOutput("model_interpretation"))
                                )
                              )
                     ),
                     
                     tabPanel("Decision Tree",
                              fluidRow(
                                box(width = 12,
                                    valueBoxOutput("tree_accuracy", width = 3),
                                    valueBoxOutput("tree_kappa", width = 3),
                                    valueBoxOutput("tree_sensitivity", width = 3),
                                    valueBoxOutput("tree_specificity", width = 3))
                              ),
                              fluidRow(
                                column(width = 12,
                                       box(title = "Variable Importance", width = 5,
                                           DT::dataTableOutput("tree_importance_table")),
                                       box(title = "Decision Tree Plot", width = 7,
                                           plotOutput("tree_plot", height = "400px"))
                                )
                              )
                     )
              )
      ),
      
      #---------------- About Me ----------------#
      tabItem(tabName = "about_me",
              fluidPage(
                div(class = "about-section",
                    fluidRow(
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
                                 p("I am a Data Analyst with over 4 years of experience in clinical trial data management, statistical modeling, and healthcare data analytics."),
                                 p("Currently pursuing my MSc in Statistical Science for Health and Population at the University of Bologna."),
                                 p("This dashboard reflects my passion for transforming hospital data into actionable insights.")
                             )
                      )
                    ),
                    br(), hr(), br(),
                    fluidRow(
                      column(12,
                             div(class = "about-title", "📊 About This Dashboard"),
                             div(class = "about-text", br(),
                                 p("- The 'Information Frequency' tab provides an overview of patient and hospital-level summaries."),
                                 p("- The 'Machine Learning' tab enables prediction models and interpretable metrics."),
                                 br(),
                                 div(class = "about-title", "🚀 Future Development Plans"), br(),
                                 p("Expand the tool with decision trees, neural networks, and user-uploaded datasets."),
                                 p("Integrate time-to-event models for post-COVID mortality tracking."),
                                 br(),
                                 div(class = "about-title", "🤝 Collaboration & Contact"), br(),
                                 p("📧 Email: ", a("seyedmohammadkhoshroo@gmail.com", href="mailto:seyedmohammadkhoshroo@gmail.com")),
                                 p("🔗 LinkedIn: ", a("linkedin.com/in/seyed-mohammad-khoshroo-545127167", href="https://linkedin.com/in/seyed-mohammad-khoshroo-545127167", target="_blank")),
                                 p("🔗 GitHub: ", a("github.com/MohammadSmk72", href="https://github.com/MohammadSmk72", target="_blank"))
                             )
                      )
                    ),
                    hr(),
                    div(class = "about-text", "Made with ❤️ using R, Shiny, and deep curiosity.")
                )
              )
      )
    )  # Close tabItems
  )  # Close dashboardBody
)  # ✅ Close dashboardPage

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
  
  
  # Machine Learning Reactive Model
  trained_model <- reactive({
    data <- filtered_data()
    
    if (nrow(data) < 10) {
      return(NULL)
    }
    
    data <- data %>%
      mutate(Status = ifelse(Status == "Dead", 1, 0))
    
    all_predictors <- c("Age.Group", "APR.Severity.of.Illness.Description",
                        "APR.Medical.Surgical.Description", "APR.Risk.of.Mortality",
                        "Length.of.Stay")
    
    selected_predictors <- input$ml_predictors
    
    formula_full <- as.formula(paste("Status ~", paste(all_predictors, collapse = " + ")))
    formula_reduced <- if (length(selected_predictors) > 0) {
      as.formula(paste("Status ~", paste(selected_predictors, collapse = " + ")))
    } else {
      as.formula("Status ~ 1")
    }
    
    model_full <- glm(formula_full, data = data, family = "binomial")
    model_reduced <- glm(formula_reduced, data = data, family = "binomial")
    
    predicted_probs <- predict(model_reduced, type = "response")
    roc_curve <- pROC::roc(data$Status, predicted_probs)
    auc_value <- pROC::auc(roc_curve)
    
    predicted_classes <- ifelse(predicted_probs >= 0.5, 1, 0)
    accuracy <- mean(predicted_classes == data$Status, na.rm = TRUE)
    
    model_summary <- summary(model_reduced)
    odds_ratios <- exp(coef(model_reduced))
    model_aic <- AIC(model_reduced)
    model_bic <- BIC(model_reduced)
    
    LL1 <- -2 * as.numeric(logLik(model_full))
    LL2 <- -2 * as.numeric(logLik(model_reduced))
    chi_square_stat <- LL2 - LL1
    df_diff <- length(coef(model_full)) - length(coef(model_reduced))
    lrt_p_value <- pchisq(chi_square_stat, df = df_diff, lower.tail = FALSE)
    
    list(
      summary = model_summary,
      odds_ratios = odds_ratios,
      auc = auc_value,
      accuracy = accuracy,
      aic = model_aic,
      bic = model_bic,
      lrt_stat = chi_square_stat,
      lrt_p = lrt_p_value
    )
  })
  
  output$ml_table <- DT::renderDataTable({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    
    coef_summary <- result$summary$coefficients
    or_values <- result$odds_ratios
    
    # Create a tibble with exact column order and names
    coef_df <- tibble::tibble(
      Variable = rownames(coef_summary),
      Estimate = round(coef_summary[, "Estimate"], 4),
      `P-Value` = round(coef_summary[, "Pr(>|z|)"], 4),
      `Odds Ratio` = round(or_values, 4)
    )
    
    # Render datatable
    DT::datatable(
      coef_df,
      options = list(
        pageLength = 7,
        autoWidth = FALSE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$roc_plot <- renderPlot({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    
    # Recompute ROC (since we already have the model and predictions)
    data <- filtered_data()
    if (nrow(data) < 10) return(NULL)
    
    data <- data %>%
      mutate(Status = ifelse(Status == "Dead", 1, 0))
    
    selected_predictors <- input$ml_predictors
    if (length(selected_predictors) == 0) return(NULL)
    
    formula_reduced <- as.formula(paste("Status ~", paste(selected_predictors, collapse = " + ")))
    model <- glm(formula_reduced, data = data, family = "binomial")
    
    predicted_probs <- predict(model, type = "response")
    roc_obj <- pROC::roc(data$Status, predicted_probs)
    
    # Plot
    plot(roc_obj, col = "#2c3e50", lwd = 3, main = "ROC Curve")
    abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal
    legend("bottomright", legend = paste("AUC =", round(pROC::auc(roc_obj), 3)),
           col = "#2c3e50", lwd = 2)
  })
  
  output$model_interpretation <- renderUI({
    result <- trained_model()
    if (is.null(result)) return("Not enough data for interpretation.")
    
    auc <- result$auc
    acc <- result$accuracy
    summary_table <- result$summary$coefficients
    odds_ratios <- result$odds_ratios
    
    p_vals <- summary_table[, "Pr(>|z|)"]
    sig_vars <- names(p_vals)[which(p_vals < 0.05)]
    sig_vars <- setdiff(sig_vars, "(Intercept)")
    
    auc_text <- if (auc >= 0.9) {
      "an <strong>excellent</strong> ability to distinguish between patients who survived and those who did not"
    } else if (auc >= 0.8) {
      "a <strong>good</strong> level of discrimination between outcomes"
    } else if (auc >= 0.7) {
      "an <strong>acceptable</strong> discrimination capacity"
    } else {
      "a <strong>poor</strong> ability to distinguish between outcomes"
    }
    
    acc_text <- paste0("The model correctly classified <strong>", round(acc * 100, 1), "%</strong> of the patients")
    
    if (length(sig_vars) > 0) {
      or_lines <- sapply(sig_vars, function(var) {
        est <- round(summary_table[var, "Estimate"], 4)
        p <- round(summary_table[var, "Pr(>|z|)"], 4)
        or <- round(odds_ratios[var], 3)
        direction <- if (or > 1) {
          "increased"
        } else if (or < 1) {
          "decreased"
        } else {
          "no change in"
        }
        paste0("<li><strong>", var, "</strong>: OR = ", or, " (", direction, " odds of mortality)</li>")
      })
      
      or_summary <- paste0(
        "The following predictors were statistically significant (p &lt; 0.05):<ul>",
        paste(or_lines, collapse = ""),
        "</ul>"
      )
    } else {
      or_summary <- "<p>No predictors were statistically significant at the 0.05 level, so no strong associations were observed.</p>"
    }
    
    HTML(paste0(
      "<p>The logistic regression model shows ", auc_text, " (AUC = ", round(auc, 3), "). ",
      acc_text, ", which reflects strong predictive accuracy.</p>",
      or_summary,
      "<p>These insights may inform clinical decision-making, especially when used alongside expert review and validation. Additional refinement or external validation may further improve model reliability.</p>"
    ))
  })
  
  # AIC ValueBox
  output$model_aic <- renderValueBox({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    valueBox(
      value = round(result$aic, 2),
      subtitle = "AIC of Selected Model",
      icon = icon("tachometer-alt"),
      color = "orange"
    )
  })
  
  # BIC ValueBox
  output$model_bic <- renderValueBox({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    valueBox(
      value = round(result$bic, 2),
      subtitle = "BIC of Selected Model",
      icon = icon("chart-pie"),
      color = "blue"
    )
  })
  
  # AUC ValueBox
  output$model_auc <- renderValueBox({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    valueBox(
      value = round(result$auc, 2),
      subtitle = "AUC of Selected Model",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Accuracy ValueBox
  output$model_accuracy <- renderValueBox({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    valueBox(
      value = paste0(round(result$accuracy * 100, 2), "%"),
      subtitle = "Accuracy of Selected Model",
      icon = icon("check-circle"),
      color = "purple"
    )
  })
  
  # Likelihood Ratio Test ValueBox
  output$model_lrt <- renderValueBox({
    result <- trained_model()
    if (is.null(result)) return(NULL)
    valueBox(
      value = paste0("LR = ", round(result$lrt_stat, 0), "\nP = ", round(result$lrt_p, 2)),
      subtitle = "LRT: Full vs Selected",
      icon = icon("balance-scale"),
      color = ifelse(result$lrt_p < 0.05, "red", "green")
    )
  }
  )
  
  # Reactive Decision Tree model
  trained_tree <- reactive({
    data <- filtered_data()
    if (nrow(data) < 10) return(NULL)
    
    data <- data %>% mutate(Status = factor(ifelse(Status == "Dead", 1, 0)))
    
    predictors <- input$ml_predictors
    if (length(predictors) == 0) return(NULL)
    
    formula_tree <- as.formula(paste("Status ~", paste(predictors, collapse = " + ")))
    
    # Fit tree model
    rpart(formula_tree, data = data, method = "class")
  })
  
  # Tree Plot
  output$tree_plot <- renderPlot({
    model <- trained_tree()
    if (is.null(model)) return(NULL)
    rpart.plot(model, type = 4, extra = 104, box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)
  })
  
  # Tree Accuracy and Confusion Matrix
  # Tree Performance Metrics
  tree_metrics_values <- reactive({
    model <- trained_tree()
    data <- filtered_data()
    if (is.null(model) || nrow(data) < 10) return(NULL)
    
    data <- data %>% mutate(Status = factor(ifelse(Status == "Dead", 1, 0)))
    preds <- predict(model, type = "class")
    
    cm <- caret::confusionMatrix(preds, data$Status)
    cm$byClass[1:2]  # Sensitivity and Specificity
    
    list(
      accuracy = cm$overall["Accuracy"],
      kappa = cm$overall["Kappa"],
      sensitivity = cm$byClass["Sensitivity"],
      specificity = cm$byClass["Specificity"]
    )
  })
  
  # Render ValueBoxes
  output$tree_accuracy <- renderValueBox({
    m <- tree_metrics_values()
    if (is.null(m)) return(NULL)
    valueBox(
      value = paste0(round(m$accuracy * 100, 1), "%"),
      subtitle = "Accuracy",
      icon = icon("check-circle"),
      color = "purple"
    )
  })
  
  output$tree_kappa <- renderValueBox({
    m <- tree_metrics_values()
    if (is.null(m)) return(NULL)
    valueBox(
      value = round(m$kappa, 2),
      subtitle = "Kappa",
      icon = icon("balance-scale"),
      color = "olive"
    )
  })
  
  output$tree_sensitivity <- renderValueBox({
    m <- tree_metrics_values()
    if (is.null(m)) return(NULL)
    valueBox(
      value = paste0(round(m$sensitivity * 100, 1), "%"),
      subtitle = "Sensitivity",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$tree_specificity <- renderValueBox({
    m <- tree_metrics_values()
    if (is.null(m)) return(NULL)
    valueBox(
      value = paste0(round(m$specificity * 100, 1), "%"),
      subtitle = "Specificity",
      icon = icon("shield-alt"),
      color = "blue"
    )
  })
  
  
  output$tree_importance_table <- DT::renderDataTable({
  model <- trained_tree()
  if (is.null(model)) return(NULL)

  importance <- model$variable.importance
  importance_df <- data.frame(
    Variable = names(importance),
    Importance = round(importance, 2)
  ) %>%
    arrange(desc(Importance))

  DT::datatable(
    importance_df,
    rownames = FALSE,
    options = list(pageLength = 5, order = list(list(1, 'desc')))
  )
})

  
  
}  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run the app
shinyApp(ui = ui, server = server)