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
                choices = c("Cognitive Abilities", "Emotional Traits", "Physical Measures")),
    # Choose the number of factor you decided to retain
    numericInput("k",label="Retain k factors:",value=3),
    # Choose the rotation of the factor pattern
    radioButtons("rotation","Choose the rotation of Factor Pattern",
                 list("SVD eigenvectors"="svd",
                      "Unrotated"="none",
                      "Quartimax"="quartimax", # 1953
                      "Quartimin"="quartimin", # 1953
                      "Varimax"="varimax", # 1958
                      "Promax"="promax"  # 1964
                      ))
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