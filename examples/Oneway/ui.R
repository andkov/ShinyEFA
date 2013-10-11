library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("One-way General Linear Model"),
  
  # Sidebar with controls to select the variable to plot against DV  and to specify whether outliers should be included
#   predictorArray <- c("Gender"="Gender",
#                       "Age"="Age")
  sidebarPanel(
    selectInput(inputId="Variable", label="Predictor Variable:", c("Gender"="Gender", "Age"="Age", "MaritalStatus"="MaritalStatus", "ChildCount"="ChildCount", "Income"="Income" )),
    checkboxInput("ShowSubject", "Show your score", TRUE),
    checkboxInput("ShowModel", "Show Summary", TRUE),
    selectInput(inputId="Gender", label="What is your gender?", c("Male"="Male", "Female"="Female")),
    sliderInput("Age", "What is your age?", min=0, max=100, value=40),
    selectInput(inputId="MaritalStatus", label="What is your marital status?", c("Unmarried", "Married", "Engaged", "Dating", "Single", "DivorcedOnce", "DivorcedMultiple")),
#     sliderInput("ChildCount", "How many children do you have?", min=0, max=4, value=0),
    numericInput("ChildCount", "How many children do you have?", 0, min=0, max=4),
    sliderInput("Income", "What is your yearly income?", min=0, max=1000000, value=30000),
    sliderInput("Development", "What is your Development Score ?", min=200, max=300, value=250)
  ),
  

  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    tags$style(type="text/css", "h3 { color: #08088A; }"),
    tags$style(type="text/css", "h4 { color: #088A29; }"),
    h3(textOutput("IVLabel")),
    textOutput("DatasetPath"),
    h3("Sample Scores"),
    plotOutput("GGPlot"),
    h4(textOutput("Bounds")),
    h3("Subset of data:"),
    numericInput("RowCount", "Number of rows to display:", 6, min=1, max=40),
    tableOutput("View"),
    h3("GLM/ANOVA table:"),
    tableOutput("AnovaTable"),
    h3("Model summary:"),
    verbatimTextOutput("ModelSummary")
  )
))
