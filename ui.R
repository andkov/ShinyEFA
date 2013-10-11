# rm(list=ls(all=TRUE))

library(shiny)
# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Exploratory Factor Analysis"),
  
  sidebarPanel(
    selectInput("dataset","Select a dataset:", 
                choices="Physical","Cognitive","Emotional")),
              
  mainPanel(
    tabsetPanel(
      tabPanel("Variables"),
      tabPanel("Correlation"),
      tabPanel("Eigens"),
      tabPanel("RMSEA"),
      tabPanel("Plot" ), 
      tabPanel("Table"),
      selected="Variables"))
))          