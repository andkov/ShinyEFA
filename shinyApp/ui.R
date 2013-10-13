# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)
library(GPArotation)

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
                            "Physical Measures",
                            "Uploaded Data")),
    # Choose the number of factor you decided to retain
    numericInput("k", label="Retain k factors:", value=3),
    
    # Choose the rotation of the factor pattern
    radioButtons("rotation","Choose the rotation of Factor Pattern",
                 list("SVD eigenvectors"="svd",
                      "Unrotated"="none",
                      "Varimax (T)"="Varimax", 
                      "Promax (Q)"="promax",
                      "Bifactor (T)"="bifactorT",
                      "Bifactor (Q)"="bifactorQ",
                      "Crawford-Ferguson (T)"="cfT",
                      "Crawford-Ferguson (Q)"="cfQ")),
    
    # Select the value of kappa for the CF rotation
    sliderInput("kappa","Value of kappa for Crawford-Ferguson:",
                min=0, max=1, value=0, step=.05),
    br(),
    # INput your own file
    fileInput('file1', 'Upload your own CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Blank=' ',
                   Tab='\t'),
                 'Comma') 
              ),
  
  # MAIN PANEL #.
  mainPanel(
    h3(textOutput("dlkafjd lkdjf a")), 
    tabsetPanel(
      tabPanel("Data", tableOutput("contents")),
      tabPanel("R", plotOutput("corrgram", height="600px"), h5(textOutput("dscr.data"))),
      tabPanel("Eigens", plotOutput("eigens", height="600px")),
      tabPanel("RMSEA", plotOutput("RMSEA", height="600px")),
      tabPanel("Pattern", plotOutput("patternPlot", width="80%", height="600px")), 
      tabPanel("Table", tableOutput("patternMatrix")),
      selected="Variables")          
  )
))
