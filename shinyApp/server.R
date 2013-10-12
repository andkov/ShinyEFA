
library(shiny)

# Loads three classic datasets from 'psych' package by William Revelle, http://cran.r-project.org/web/packages/psych/
source("dataprep.R") # begins with rm(list=ls(all=TRUE))
# loads custom funtions by James S. Steier. Visit www.statpower.net for description and download
source(file.path(getwd(),"sourced","Steiger R library functions.txt"))
source(file.path(getwd(),"sourced","AdvancedFactorFunctions_CF.R"))

# Description of the three datasets from the "psych" package documentation, http://cran.r-project.org/web/packages/psych/psych.pdf
dscr.cognitive<- "The nine psychological variables from Harman (1967, p 244) are taken from unpublished class notes of K.J. Holzinger with 696 participants." 
dscr.emotional <- "Eight emotional variables are taken from Harman (1967, p 164) who in turn adapted them from Burt (1939). They are said be from 172 normal children aged nine to twelve. As pointed out by Jan DeLeeuw, the Burt data set is a subset of 8 variables from the original 11 reported by Burt in 1915. That matrix has the same problem."
dscr.physical <- "The Eight Physical Variables problem is taken from Harman (1976) and represents the correlations between eight physical variables for 305 girls. The two correlated clusters represent four measures of lankiness and then four measures of stockiness. The original data were selected from 17 variables reported in an unpublished dissertation by Mullen (1939)."

# Define server logic required to summarize and view the selected dataset
shinyServer( function(input, output) {
####        INPUT       ####
  # Select from three classic datasets
  datasetInput <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=cognitive,
           "Emotional Traits"=emotional,
           "Physical Measures"=physical
    )
  })
  # Choice of dataset automatically initiates the choice of description object
  datasetDescription <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=dscr.cognitive,
           "Emotional Traits"=dscr.emotional,
           "Physical Measures"=dscr.physical
    )    
  })
  # Creates the reactive object contaning the strings of dataset names to be used later
  dsTag <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"="cognitive",
           "Emotional Traits"="emotional",
           "Physical Measures"="physical"
    )    
  })
# Creates dynamic object p that contains number of variable in the choses dataset
  p <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=p.cognitive,
           "Emotional Traits"=p.emotional,
           "Physical Measures"=p.physical
    )    
  })

  n <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=n.cognitive,
           "Emotional Traits"=n.emotional,
           "Physical Measures"=n.physical
    )    
  })
# Creates dynamic object rotation that contains the name of chosen rotation
chosenRotation <- reactive({
  switch(EXPR=input$rotation,
         "V from SVD"="svd",
         "Unrotated"="none",
         "Quartimax"="quartimax", # 1953
         "Quartimin"="quartimin", # 1953
         "Varimax"="varimax", # 1958
         "Promax"="promax", # 1964
  )    
})


####        OUTPUT     ####
# attaches data description to the bottom of tabPanel("R") in mainPanel
  output$dscr <- renderPrint ({
    datasetDescription() 
  })
# produces correlelogram of the selected dataset
  output$corrgram <- renderPlot({
    corrgram(datasetInput(),upper.panel=panel.conf, lower.panel=panel.pie,type="cor",order=TRUE)
  }) 
# produces eigen plots
  output$eigens<-renderPlot({
    R<-datasetInput()
    Scree.Plot(R)
  })
# produces RMSEA plots
  output$RMSEA<-renderPlot({
    R<-datasetInput()
    FA.Stats(R,n.factors=1:4,n.obs=get(paste0("n.",dsTag())), RMSEA.cutoff=0.05)
  })
# selectes the number of variables in the chosen dataset
  output$p<-renderText({ 
    p()
  })
# Prints the plot of the pattern matrix
  output$patternPlot<-renderPlot(function(){
    A <- factanal(covmat=datasetInput(),n.obs=n(),factors=input$k,maxit=1000,rotation="none")
    F<-A$loadings[1:p(),]
    F<-cbind(F,matrix(numeric(0),p(),p()-input$k))
    colnames(F)<-paste0("F",1:ncol(R))
    source("patternPlot.R")
})

})

