# http://rstudio.github.io/shiny/tutorial/#reactivity - server
library(shiny)
source("dataprep.R")
#### ####
colnames.cognitive<-c("Word Meaning",
                      "Sentence Completion",
                      "Odd Words",
                      "Mixed Arithmetic",
                      "Remainders",
                      "Missing Numbers",
                      "Gloves",
                      "Boots",
                      "Hatchets")



cognitive.dd<-"The nine psychological variables from Harman (1967, p 244) are taken from unpublished class notes of K.J. Holzinger with 696 participants." 
emotional.dd<-"Eight emotional variables are taken from Harman (1967, p 164) who in turn adapted them from Burt (1939). They are said be from 172 normal children aged nine to twelve. As pointed out by Jan DeLeeuw, the Burt data set is a subset of 8 variables from the original 11 reported by Burt in 1915. That matrix has the same problem."
physical.dd<-"The Eight Physical Variables problem is taken from Harman (1976) and represents the correlations between eight physical variables for 305 girls. The two correlated clusters represent four measures of lankiness and then four measures of stockiness. The original data were selected from 17 variables reported in an unpublished dissertation by Mullen (1939)."
#### ####
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Cognitive Abilities"=cognitive,
           "Emotional Traits"=emotional,
           "Physical Measure"=physical)
  })

  datasetDescription<- reactive({
    switch(input$dataset,
           "Cognitive Abilities"=cognitive.dd,
           "Emotional Traits"=emotional.dd,
           "Physical Measures"=physical.dd)
    
  })

  output$corrgram <- renderPlot({
    
    corrgram(datasetInput(),upper.panel=panel.conf,
             lower.panel=panel.pie,type="cor",order=FALSE)})
 
  output$dd<-renderPrint({
      datasetDescription() })
})

