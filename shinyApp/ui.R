# http://rstudio.github.io/shiny/tutorial/#reactivity - UI

library(shiny)
library(psych)
library(corrgram)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Exploratory Factor Analysis"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("Cognitive Abilities", "Emotional Traits", "Physical Measures"))
  ),
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("Description of the current tab")), 
    tabsetPanel(
      tabPanel("Variables"),
      tabPanel("R", plotOutput("corrgram"),h5(textOutput("dd"))),
      tabPanel("Eigens"),
      tabPanel("RMSEA"),
      tabPanel("Plot" ), 
      tabPanel("Table"),
      selected="Variables")            
  )
))