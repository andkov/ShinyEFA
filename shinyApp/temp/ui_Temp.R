library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Rotations"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
          sliderInput("decimal", 
                "Kappa value:", 
                min = 0,
                max = 1, 
                step =.2,
                value = 0)
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Table", tableOutput("table"))
    )
  )
))

hist()