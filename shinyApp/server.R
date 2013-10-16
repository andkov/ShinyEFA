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
# dscr.R<-c("Correlogram of the observed variables")
# dscr.eigens<- c("Eigenvalues from the diagonal of D in VDV'")
source(file.path(getwd(), "sourced", "SteigerRLibraryFunctions.txt"))
source(file.path(getwd(), "sourced", "AdvancedFactorFunctions_CF.r"))
source("dataprep.R") # begins with rm(list=ls(all=TRUE)) 
dscr.cognitive <- "from psych() documentation n\ The nine psychological variables from Harman (1967, p 244) are taken from unpublished class notes of K.J. Holzinger with 696 participants." 
dscr.emotional <- "Eight emotional variables are taken from Harman (1967, p 164) who in turn adapted them from Burt (1939). They are said be from 172 normal children aged nine to twelve. As pointed out by Jan DeLeeuw, the Burt data set is a subset of 8 variables from the original 11 reported by Burt in 1915. That matrix has the same problem."
dscr.physical <- "The Eight Physical Variables problem is taken from Harman (1976) and represents the correlations between eight physical variables for 305 girls. The two correlated clusters represent four measures of lankiness and then four measures of stockiness. The original data were selected from 17 variables reported in an unpublished dissertation by Mullen (1939)."
dscr.Thurstone<- "Thurstone, L.L. (1947). Multiple Factor Analysis. Chicago: University of Chicago Press.."

# Define server logic required to summarize and view the selected dataset
shinyServer( function(input, output) {
####        INPUT       ####

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
  }) # datasetInput

# Dataset description
  datasetDescription <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=dscr.cognitive,
           "Emotional Traits"=dscr.emotional,
           "Physical Measures"=dscr.physical,
#            "Harman74"=dscr.Harman74,
           "Thurstone"=dscr.Thurstone
    )    
  }) # datasetDescription
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
# # What tabset is it?
  currentTabset <- reactive({
    print(input$tabcur)
    switch(EXPR=input$tabcur,
           "Data"=           "FApyramid_03.png", 
           "Correlations"=   "FApyramid_R_03.png",
           "Eigens"=         "FApyramid_D_03.png", 
           "RMSEA"=          "FApyramid_D_03.png",
           "Components"=     "FApyramid_V_03.png",
           "Factors"=        "FApyramid_L_03.png",
           "Table"=          "FApyramid_L_03.png"
         
    #Add the other tab names
   
  )}) # currentTabset
             

inputDatavars <- reactive({
  switch(EXPR=input$dataset,
         "Cognitive Abilities"="cognitive_03.png",
         "Emotional Traits"="emotional_03.png",
         "Physical Measures"="physical_03.png",
         "Thurstone"="Thurstone_03.png"
  )    
}) # datasetDescription
    
  


####        OUTPUT ####
# # some description
#   output$somedscr <- renderPrint ({
#     print("Some Description")
#   })


# dataset description
output$datavars <- renderImage({
  filePath <- inputDatavars()
  list(src=file.path(getwd(), "images", filePath), alt="Description of the dataset"
  #    list(src=file.path(getwd(), "images/FApyramid_03.png"), alt="Matrix decomposition options")
  ,contentType = 'image/png',
  width = 400,
  height = 100
  )
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
  # Reactive code
  R <- datasetInput() # the choice of the dataset in ui.R
  k <- input$k # the choice of the number of factors to retain from ui.R
  n.obs <- n()  # choice of the dataset defines  n - its sample size
  p <- p() # the choice of dataset defines p - its number of variables
  source("rotationDecision.R",local=TRUE) # input$rotation -> factanla -> GPArotation
#   graphToShow <-     corrplot(Phi, method="shade",
#                               addCoef.col="black",addcolorlabel="no",order="AOE")
  graphToShow <-  corrgram(Phi, 
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
# selectes the number of variables in the chosen dataset
  output$p <- renderText({ 
    p()
  })

# Pyramid Image
 output$PyramidImage <- renderImage({
   #print(currentTabset())
   filePath <- currentTabset()
   #    list(src=file.path(getwd(), "images", filePath), alt="Matrix decomposition options")
      list(src=file.path(getwd(), "images", "clouds_03.png"), alt="Matrix decomposition options")
#    img(src=file.path(getwd(), "images", filePath),height=500,width=500)
#    img(src="clouds_03.png",height=500,width=500)
 }, deleteFile=FALSE, )

 output$patternPlotPCA <- renderPlot({  
    # Reactive code
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    # procedures
    V <- base::svd(R)$v
    FPM <- V[, 1:k] # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    rownames(FPM) <- rownames(datasetInput())
    colnames(FPM) <- paste0("V", 1:p) # V, not F because these are components, not factors
#     FPM
    # output
    source("patternPlot.R", local=TRUE) #Defines the function to produce a graph; usus FMP to create ggplot
    graphToShow <- fpmFunction(FPM.matrix=FPM, mainTitle=NULL
                                # "from output$patternPlotPCA"    # uncomment line to customize title
                               ) #Call/execute the function defined above.
    print(graphToShow) #Print that graph.
  }) # FPM plot (Factor Pattern Matrix)

output$patternPlotFA <- renderPlot({  
  # Reactive code
  R <- datasetInput() # the choice of the dataset in ui.R
  k <- input$k # the choice of the number of factors to retain from ui.R
  n.obs <- n()  # choice of the dataset defines  n - its sample size
  p <- p() # the choice of dataset defines p - its number of variables
  source("rotationDecision.R",local=TRUE) # input$rotation -> factanla -> GPArotation
  source("patternPlot.R",local=TRUE) # usus FMP to create ggplot
  graphToShow <- fpmFunction(FPM.matrix=FPM, mainTitle=NULL
#                                "from output$patternPlotFA"      # uncomment line to customize title
                             ) #Call/execute the function defined above.
  print(graphToShow) #Print that graph.
  
}) # FPM plot (Factor Pattern Matrix)

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
    # Reactive code
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    
#     k<-4
#     n.obs<-n.cognitive
    ## IF --
    if( input$rotation=="svd" ) {
      V <- base::svd(R)$v
      FPM <- V[, 1:k] # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      rownames(FPM) <- rownames(datasetInput())
      colnames(FPM) <- paste0("V", 1:p) #Andrey, should this be 'F' instead of 'V'?
      FPM # THE OUTPUT
    } 
    else if( input$rotation=="promax" ) { 
      A <- stats::factanal(factors = k, covmat=R, 
                   rotation="none", control=list(rotate=list(normalize=TRUE)))
#       FPM <- promax(A, pow)$loadings
      A <- GPromax(A$loadings, pow=3)
      FPM <- A$Lh # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      FPM # THE OUTPUT
    } 
    else if( input$rotation=="none" ) { 
      A <- stats::factanal(factors = k, covmat=R, 
                   rotation="none", control=list(rotate=list(normalize=TRUE)))
      FPM <- A
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      FPM  # THE OUTPUT
    } 
    else if( input$rotation %in% c("cfT","cfQ") ) { 
      A <- stats::factanal(factors = k, covmat=R, 
                   rotation="none", control=list(rotate=list(normalize=TRUE)))
      L <- A$loadings
      FPM <- eval(parse(text=
                        paste0(rotationInput(),"(L,Tmat=diag(ncol(L)),kappa=input$kappa,normalize=FALSE, eps=1e-5, maxit=1000)")))
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM,matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      FPM  # THE OUTPUT
    } 
    else if( input$rotation==rotationInput() ) { 
      A <- stats::factanal(factors = k, covmat=R, 
                   rotation="none", control=list(rotate=list(normalize=TRUE)))
      L <- A$loadings
      FPM <- eval(parse(text=
        paste0(rotationInput(),"(L, Tmat=diag(ncol(L)), normalize=FALSE, eps=1e-5, maxit=1000)")))
      FPM <- FPM$loadings # FPM - Factor Pattern Matrix
      FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
      colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
      FPM  # THE OUTPUT
    }

  
  })# FPM table (Factor Pattern Matrix)
}) # end of ShinyServer
#### end of OUTPUT
