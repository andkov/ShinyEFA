# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)
library(GPArotation)

# Define UI for dataset viewer application
shinyUI(
  pageWithSidebar(
  
    # HEADER #,
    headerPanel("Exploratory Factor Analysis"),
    
    # SIDEBAR #,
    sidebarPanel(
      # Chose the dataset you would like to explore
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Cognitive Abilities", 
                              "Emotional Traits", 
                              "Physical Measures",
#                               "Harman74",
                              "Thurstone"
#                               ,"Uploaded Data"
                              )),
      # Choose the number of factor you decided to retain
      numericInput("k", label="Retain k factors:", value=3),
      
      
      # Choose the rotation of the factor pattern
      radioButtons("rotation","Choose the rotation Method",
                   list(#"SVD eigenvectors"="svd",
                     "Unrotated"="none",
                     "Varimax (T)"="Varimax", 
                     "Promax (Q)"="promax",
                     "Bifactor (T)"="bifactorT",
                     "Bifactor (Q)"="bifactorQ",
                     "Crawford-Ferguson (T)"="cfT",
                     "Crawford-Ferguson (Q)"="cfQ")),
      
      # Select the value of kappa for the CF rotation
      sliderInput("kappa","Value of kappa for Crawford-Ferguson:",
                  min=0, max=1, value=0, step=.05)
#       ,
#       br(),
#       # INput your own file
#       fileInput('file1', 'Upload your own CSV file',
#                 accept=c('text/csv', 'text/comma-separated-values,text/plain')),
#       tags$hr(),
#       checkboxInput('header', 'Header', TRUE),
#       radioButtons('sep', 
#                    'Separator',
#                    c(Comma=',', Blank=' ', Tab='\t'),
#                    'Comma')
    ), # close sidebarPanel
    
    # MAIN PANEL #.
    mainPanel(
#       h3(textOutput("dlkafjd lkdjf a")), 
      tabsetPanel(id="tabcur",
                  tabPanel("Data",id="tabData", 
                           tableOutput("contents"), 
                           h5(textOutput("dscr.data"))),
                  tabPanel("Correlations",id="tabCorrelations", 
                           plotOutput("corrgramX", width="90%", height="800px"), 
                           h5(textOutput("dscr.data2"))),
                  tabPanel("Eigens",id="tabEigens", 
                           plotOutput("eigens", width="60%",height="600px")),
                  tabPanel("RMSEA",id="tabRMSEA", 
                           plotOutput("RMSEA", width="60%", height="600px")),
                  tabPanel("Components",id="tabComponents", 
                           plotOutput("patternPlotPCA", width="90%", height="850px")), 
                  tabPanel("Factors",id="tabFactors", 
                           plotOutput("patternPlotFA", width="90%", height="850px"),
                           plotOutput("corrgramF", width="50%", height="200px")), 
                  #         tabPanel("Table",id="tabTable", tableOutput("patternMatrix")),
                  selected="Data"
      ),# #Close tabsetPanel          
      imageOutput("PyramidImage", width="20%", height="80px")
    ) #Close mainPanel  
  ) #Close pageWithSidebar
) #Close shinyUI