# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # HEADER #,
  headerPanel("Exploratory Factor Analysis"),
  
  # SIDEBAR #,
  sidebarPanel(
    # Chose the dataset you would like to explore
    selectInput("dataset", "Choose a dataset:", 
                choices = c("Cognitive Abilities", 
                            "Emotional Traits", 
                            "Physical Measures")),
    
    # Choose the number of factor you decided to retain
    numericInput("k",label="Retain k factors:",value=3),
    
    # Choose the rotation of the factor pattern
    radioButtons("rotation","Choose the rotation of Factor Pattern",
                 list("SVD eigenvectors"="svd",
                      "Unrotated"="none",
                      "Varimax ORTH"="Varimax", 
                      "Promax OBLQ"="promax",
                      "Bifactor ORTH"="bifactorT",
                      "Bifactor OBLQ"="bifactorQ",
                      "Crawford-Ferguson ORTH"="cfT",
                      "Crawford-Ferguson OBLQ"="cfQ")),
    
    # Select the value of kappa for the CF rotation
    sliderInput("kappa","Value of kappa for Crawford-Ferguson:",
                min=0,max=1,value=0,step=.05)
              ),
  
  # MAIN PANEL #.
  mainPanel(
    h3(textOutput("dlkafjd lkdjf a")), 
    tabsetPanel(
      tabPanel("Variables",textOutput("somedscr")),
      tabPanel("R", plotOutput("corrgram",height="600px"),h5(textOutput("dscr.data"))),
      tabPanel("Eigens",plotOutput("eigens",height="600px")),
      tabPanel("RMSEA",plotOutput("RMSEA",height="600px")),
      tabPanel("Pattern",plotOutput("patternPlot",width="80%",height="600px")), 
      tabPanel("Table",tableOutput("patternMatrix")),
      selected="Variables")          
            )
  
))