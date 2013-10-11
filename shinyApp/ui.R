library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Visual Rotation Tool"),
  
   sidebarPanel(
    selectInput("dataset","Select a dataset:", 
                choices="Physical","Cognitive","Emotional"),
    
    radioButtons("rotation", "Select rotation:",
                 list("Unrotated" = "none",
                      "Varimax" = "varimax",
                      "Promax" = "promax",
                      "Quartimax" = "quartimax",
                      "Quartimin" = "quartimin",
                      "Crawford-Ferguson"="CF")),
    br(),
    
    numericInput("nfactors", 
                "Select the number of factors to retain:", 
                value = 3,
                min = 1, 
                max = 10),
    br(),
    
    checkboxInput("oblique","Correlated Factors?", value=FALSE),
    
    sliderInput("kappa","Select the value of kappa:",
                value=0, min=0, max=1, step=.2)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Eigens and RMSEA", plotOutput("diagnostics")),
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Table", tableOutput("table")),
      selected="Table")
  )
))