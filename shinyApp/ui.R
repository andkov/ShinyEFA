# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Exploratory Factor Analysis"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    # Chose the dataset you would like to explore
    selectInput("dataset", "Choose a dataset:", 
                choices = c("Cognitive Abilities", "Emotional Traits", "Physical Measures")),
    # Choose the number of factor you decided to retain
    numericInput("k",label="Retain k factors:",value=3),
    
#     # Choose the rotation of the principal component
#     radioButtons("PCA","Principle Components",
#                  list("Eigenvectors, VDV'"="svd"
#                       )),
  
    # Choose the rotation of the factor pattern
    radioButtons("rotation","Common Factors",
                 list("Unrotated"="none",
                      "Quartimax"="quartimax", # 1953
                      "Quartimin"="quartimin", # 1953
                      "Varimax"="varimax", # 1958
                      "Promax"="promax"  # 1964
                      ))
  ),
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("Description of the current tab")), 
    tabsetPanel(
      tabPanel("Variables",tableOutput("varNames")),
      tabPanel("R", plotOutput("corrgram",width="60%",height="400px"),h5(textOutput("dscr"))),
      tabPanel("Eigens",plotOutput("eigens",height="500px")),
      tabPanel("RMSEA",plotOutput("RMSEA",height="500px")),
      tabPanel("Pattern",plotOutput("patternPlot",width="80%",height="500px")), 
      tabPanel("Table",tableOutput("patternMatrix")),
      selected="Variables")            
  )
))