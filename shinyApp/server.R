rm(list=ls(all=TRUE))
library(datasets)
library(ggplot2) # load ggplot
library(GPArotation)
library(psych)
library(plotrix)
library(sem)
library(stats)
library(corrplot)

# # Descriptions of the tabsets
source(file.path(getwd(), "sourced", "SteigerRLibraryFunctions.txt"))
source(file.path(getwd(), "sourced", "AdvancedFactorFunctions_CF.r"))
source("dataprep.R") # begins with rm(list=ls(all=TRUE)) 

# Define server logic required to summarize and view the selected dataset
shinyServer( function(input, output) {
  ########################################
  #### INPUT ####
  ########################################
# Creates the reactive object contaning the strings of dataset names to be used later
  dsTag <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"="cognitive",
           "Emotional Traits"="emotional",
           "Physical Measures"="physical",
#            "Harman74"="Harman74",
           "Thurstone"="Thurstone",
           "Uploaded Data"="uploaded"
    )    
  })
# Dataset
  datasetInput <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=cognitive,
           "Emotional Traits"=emotional,
           "Physical Measures"=physical,
#            "Harman74"=Harman74,
           "Thurstone"=Thurstone,
           "Uploaded Data"=uploaded
    )
  }) 
# Dataset description
  datasetDescription <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=dscr.cognitive,
           "Emotional Traits"=dscr.emotional,
           "Physical Measures"=dscr.physical,
#            "Harman74"=dscr.Harman74,
           "Thurstone"=dscr.Thurstone
    )    
  })
# Number of observed variables
  p <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=p.cognitive,
           "Emotional Traits"=p.emotional,
           "Physical Measures"=p.physical,
#            "Harman74"= p.Harman74,
           "Thurstone"=p.Thurstone,
           "Uploaded Data"=p.uploaded
    )    
  }) # p
# Sample size
  n <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=n.cognitive,
           "Emotional Traits"=n.emotional,
           "Physical Measures"=n.physical,
#            "Harman74"=n.Harman74,
           "Thurstone"=n.Thurstone,
           "Uploaded Data"=n.uploaded
    )    
  }) # n
# Rotation
  rotationInput <- reactive({
    switch(EXPR=input$rotation,
           none="none",
           Varimax="Varimax", # 1958
           promax="promax",
           bifactorT="bifactorT",
           bifactorQ="bifactorQ",
           cfT="cfT",
           cfQ="cfQ"
    )    
  }) # rotationInput
  
  imageFileName <- reactive({
    switch(EXPR=input$tabcur,
           "Data"=           "clouds_03.png", 
           "Correlations"=   "clouds_R_03.png",
           "Eigens"=         "clouds_D_03.png", 
           "RMSEA"=          "clouds_D_03.png",
           "Components"=     "clouds_V_03.png",
           "Factors"=        "clouds_L_03.png",
           "Table"=          "clouds_L_03.png"   
  )}) # imageFileName             

inputDatavars <- reactive({
  switch(EXPR=input$dataset,
         "Cognitive Abilities"="cognitive_03.png",
         "Emotional Traits"="emotional_03.png",
         "Physical Measures"="physical_03.png",
         "Thurstone"="Thurstone_03.png"
  )    
}) # datasetDescription
    
########################################
#### OUTPUT ####
########################################
# dataset description
  output$datavars <- renderImage({
    filePath <- inputDatavars()
    list(src=file.path(getwd(), "images", filePath), alt="Description of the dataset")
  }, deleteFile=FALSE)
# data description
  output$dscr.data <- renderPrint ({
    cat(datasetDescription())
  })
  output$dscr.data2 <- renderPrint ({
    cat(datasetDescription())
  })
# tabset description
  output$dscr.tabset <- renderPrint({
    print(c("Description of the current tabset"))
  }) 
#  correlelogram for observed variables
  output$corrgramX <- renderPlot({
    corrgram(datasetInput(), 
             upper.panel=panel.conf, 
             lower.panel=panel.pie, 
             type="cor", order=TRUE
             )
  }) 
#  correlelogram for observed variables
output$corrgramF <- renderPlot({
  R <- datasetInput() # the choice of the dataset in ui.R
  k <- input$k # the choice of the number of factors to retain from ui.R
  n.obs <- n()  # choice of the dataset defines  n - its sample size
  p <- p() # the choice of dataset defines p - its number of variables
  source("rotationDecision.R", local=TRUE) # input$rotation -> factanla -> GPArotation
  graphToShow <- corrgram(Phi, 
                          upper.panel=panel.conf, 
                          lower.panel=panel.pie, 
                          type="cor", order=TRUE)
  print(graphToShow) #Print that graph.
}) 
# eigen plots
  output$eigens <- renderPlot({
    R <- datasetInput()
    Scree.Plot(R)
  })
# produces RMSEA plots
  output$RMSEA <- renderPlot({
    R <- datasetInput()
    FA.Stats(R, n.factors=1:input$k, n.obs=get(paste0("n.", dsTag())), RMSEA.cutoff=0.05)
  })
# selects the number of variables in the chosen dataset
  output$p <- renderText({ 
    p()
  })

# Pyramid Image
  output$PyramidImage <- renderImage({
    filePath <- imageFileName()
    list(src=file.path(getwd(), "images", filePath), alt="Matrix decomposition options")
  }, deleteFile=FALSE )

 output$patternPlotPCA <- renderPlot({  
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    V <- base::svd(R)$v
    FPM <- V[, 1:k] # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    rownames(FPM) <- rownames(datasetInput())
    colnames(FPM) <- paste0("V", 1:p) # V, not F because these are components, not factors
    source("patternPlot.R", local=TRUE) #Defines the function to produce a graph; usus FMP to create ggplot
    graphToShow <- fpmFunction(FPM.matrix=FPM, mainTitle=NULL) #Call/execute the function defined above. # mainTitle="from output$patternPlotPCA"    # uncomment line to customize title
    print(graphToShow) #Print that graph.
  }) #Close patternPlotPCA

output$patternPlotFA <- renderPlot({  
  R <- datasetInput() # the choice of the dataset in ui.R
  k <- input$k # the choice of the number of factors to retain from ui.R
  n.obs <- n()  # choice of the dataset defines  n - its sample size
  p <- p() # the choice of dataset defines p - its number of variables
  source("rotationDecision.R",local=TRUE) # input$rotation -> factanla -> GPArotation
  source("patternPlot.R",local=TRUE) # uses FMP to create ggplot
  graphToShow <- fpmFunction(FPM.matrix=FPM, mainTitle=NULL) #Call/execute the function defined above.
  print(graphToShow) #Print that graph.  
}) #Close patternPlotFA

output$contents <- renderTable({
# if(datasetInput()==uploaded){
#     inFile <- input$file1 #use anywhare in server.R
#     if( is.null(inFile) )
#       return(NULL)
#     read.csv(inFile$datapath, header=input$header, sep=input$sep)
# }else{
  datasetInput()
# } 
}) # Displaces the data that was uploaded

output$patternMatrix <- renderTable({
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    
    ## IF --
    if( input$rotation=="svd" ) {
      V <- base::svd(R)$v
      FPM <- V[, 1:k] # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      rownames(FPM) <- rownames(datasetInput())
      colnames(FPM) <- paste0("V", 1:p) #Andrey, should this be 'F' instead of 'V'?
      return( FPM )
    } 
    else if( input$rotation=="promax" ) { 
      A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
      A <- GPromax(A$loadings, pow=3) #FPM <- promax(A, pow)$loadings
      FPM <- A$Lh # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      return( FPM )
    } 
    else if( input$rotation=="none" ) { 
      A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
      FPM <- A
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      return( FPM )
    } 
    else if( input$rotation %in% c("cfT","cfQ") ) { 
      A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
      L <- A$loadings
      FPM <- eval(parse(text=paste0(rotationInput(),"(L,Tmat=diag(ncol(L)),kappa=input$kappa,normalize=FALSE, eps=1e-5, maxit=1000)")))
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM,matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      return( FPM )
    } 
    else if( input$rotation==rotationInput() ) { 
      A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
      L <- A$loadings
      FPM <- eval(parse(text=paste0(rotationInput(),"(L, Tmat=diag(ncol(L)), normalize=FALSE, eps=1e-5, maxit=1000)")))
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      return( FPM )
    }  
  })#Close patternMatrix --FPM table (Factor Pattern Matrix)
}) #Close ShinyServer
