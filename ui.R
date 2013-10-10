library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Visual Rotation Tool"),
  
   sidebarPanel(
    radioButtons("rotation", "Select rotation:",
                 list("Unrotated" = "none",
                      "Varimax" = "varimax",
                      "Promax" = "promax",
                      "Quartimax" = "quartimax",
                      "Quartimin" = "quartimin",
                      "Crawford-Ferguson"="cf")),
    br(),
    
    numericInput("nfactors", 
                "Select the number of factors to retain:", 
                value = 3,
                min = 1, 
                max = 10),
    br(),
    
    checkboxInput("oblique","Correlated Factors?", value=FALSE)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Table", tableOutput("table"))
    )
  )
))