# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)
library(GPArotation)

# shinyUI - defines/describes User Interface 
{shinyUI(
  
  # pageWithSidebar
  {pageWithSidebar(
  
      # HEADER #
      {headerPanel("Shiny Exploratory Factor Analysis")
       },# headerPanel
      
      # SIDEBAR #
      {sidebarPanel(
        
          # selectInput - "dataset" - choose dataset
          {selectInput("dataset", "Choose a dataset:", 
                      choices = c("Cognitive Abilities", 
                                  "Emotional Traits", 
                                  "Physical Measures",
                                  "Thurstone"
    #                            ,"Uploaded"
                                  ))}, # selectInput          
          # numericInput - "k" - number of retained factors
          {numericInput("k", label="Retain k factors:", value=3)
           },# numericInput          
          # radioButtons - "rotation" - select rotation method
          {radioButtons("rotation","Choose the rotation Method",
                       list(#"SVD eigenvectors"="svd",
                         "Unrotated"="none",
                         "Varimax (T)"="Varimax", 
                         "Promax (Q)"="promax",
                         "Bifactor (T)"="bifactorT",
                         "Bifactor (Q)"="bifactorQ",
                         "Crawford-Ferguson (T)"="cfT",
                         "Crawford-Ferguson (Q)"="cfQ"))
           }, # radioButtons          
          # sliderInput - "kappa" - value for Crawford-Ferguson
          {sliderInput("kappa","Value of kappa for Crawford-Ferguson:",
                      min=0, max=1, value=0, step=.05)
          } # sliderInput
          , br()
          , br()
          ,img(src="clouds_03.png",height=500,width=500)
       )},# sidebarPanel
      
      # MAIN #
      {mainPanel(
        
        # tabsetPanel
        {tabsetPanel(id="tabcur",
              tabPanel("Data",id="tabData", 
                      imageOutput("datavars", width = "100%", height = "700px")),
              tabPanel("Correlations",id="tabCorrelations", 
                             plotOutput("corrgramX", width="90%", height="700px")),
              tabPanel("Eigens",id="tabEigens", 
                       plotOutput("eigens", width="50%",height="380px"),
                       plotOutput("RMSEA", width="50%", height="380px")),
#               tabPanel("RMSEA",id="tabRMSEA", 
#                        plotOutput("RMSEA", width="60%", height="600px")),
              tabPanel("Components",id="tabComponents", 
                       plotOutput("patternPlotPCA", width="90%", height="750px")), 
              tabPanel("Factors",id="tabFactors", 
                       plotOutput("patternPlotFA", width="90%", height="750px"),
                       br(),
                       plotOutput("corrgramF", width="50%", height="200px")), 
#               tabPanel("CorrelationY",id="tabCorY", 
#                        plotOutput("corrgramF", width="100%", height="400px")),
#                       tabPanel("Table",id="tabTable", tableOutput("patternMatrix")),
                     
              selected="Data"
        )}# #Close tabsetPanel  
        
#         # imageOutput
#         {imageOutput("PyramidImage", width="20%", height="80px")
#         } # imageOutput
        
      )} # mainPanel  
      
  )} # pageWithSidebar
  
)} #Close shinyUI





























