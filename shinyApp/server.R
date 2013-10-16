rm(list=ls(all=TRUE))
library(datasets)
library(ggplot2) # load ggplot
library(GPArotation)
library(psych)
library(plotrix)
library(sem)
library(stats)
library(corrplot)
library(corrgram)

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
  
#   diagonalPanel <- function (x = 0.5, y = 0.5, txt, cex, font, srt, ...) {
#     panel.txt( x, y, txt, font, srt, cex = 3, ...)
#   }
#   panelPie <- function (x, y, corr = NULL, col.regions, ...) {
#     oldPar <- par(pty="s") #Set parameters for base graphics
#     panel.pie(x, y, corr, col.regions, pty="s", cex=10,  ...) 
#     par(oldPar) #Reset to the pre-existing graphic parameters
#   }
  corrplotCustom <- function (corr, lower="number", upper="circle", tl.pos=c("d", 
    "lt", "n"), diag=c("n", "l", "u"), bg="white", addgrid.col="gray", ...)  {
    
    diag <- match.arg(diag)
    tl.pos <- match.arg(tl.pos)
    n <- nrow(corr)
    corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
    corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
    if (diag == "n" & tl.pos != "d") {
      symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
    }
  }
#  correlelogram for observed variables
  output$corrgramX <- renderPlot({
    #     oldPar <- par(pty="m") #Set parameters for base graphics
    #     corrgram(datasetInput(),
    #              upper.panel=panel.conf, 
    #              lower.panel=panel.pie, #panel.pie, 
    #              type="cor", order=TRUE)
    #     par(oldPar) #Reset to the pre-existing graphic parameters
    #     corrplot(datasetInput(), order = "AOE", cl.pos = "b", tl.pos = "d")
    #corrplot.mixed(datasetInput(), lower = "pie", upper = "number")
    #corrplot.mixed(datasetInput(), lower = "pie", upper = "number", addgrid.col="gray19")
    
    corrplotCustom(datasetInput(), order="AOE", lower="pie", upper="number", 
                   title="Correlation Among Observed Variables", line=-1, 
                   tl.col="black", addCoef.col="black", cl.cex=2)
  }) 
#  correlelogram for factors
  output$corrgramF <- renderPlot({
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    source("rotationDecision.R", local=TRUE) # input$rotation -> factanla -> GPArotation
#     oldPar <- par(pty="s") #Set parameters for base graphics
#     corrgram(Phi,
#              upper.panel=panel.conf, 
#              lower.panel=panel.pie, #panel.pie, 
#              type="cor", order=TRUE)
#     par(oldPar) #Reset to the pre-existing graphic parameters
    corrplotCustom(Phi, order="AOE", lower="pie", upper="number", 
                   title="Correlation Among Factors", line=-1, 
                   tl.col="black", addCoef.col="black", cl.cex=1, )
  }) 
  FA.StatsGG <- function(Correlation.Matrix,n.obs,n.factors,conf=.90,
                         maxit=1000,RMSEA.cutoff=NULL,
                         main="RMSEA Plot",sub=NULL){
    runs <- length(n.factors)  
    R <- Correlation.Matrix
    maxfac <- max(n.factors)
    res <- matrix(NA,runs,8)
    roots <- eigen(R)$values
    for(i in 1:runs){
      output <- factanal(covmat=R,n.obs=n.obs,factors=n.factors[i],maxit=maxit)
      X2 <- output$STATISTIC
      df <- output$dof
      ci <- rmsea.ci(X2,df,n.obs,conf)
      pvar <- sum(roots[1:n.factors[i]])
      v <- c(n.factors[i],pvar,X2,df,1-pchisq(X2,df),ci$Point.Estimate,
             ci$Lower.Limit,ci$Upper.Limit)
      
      res[i,] <- v
    }
    colnames(res)=c("Factors","Cum.Eigen","Chi-Square","Df","p.value", "RMSEA.Pt","RMSEA.Lo","RMSEA.Hi")
    ds <- data.frame(FactorID=n.factors, Rmsea=res[,6], Lower=res[,7], Upper=res[,8])
    g <- ggplot(ds, aes(x=FactorID, y=Rmsea, ymin=Lower, ymax=Upper)) +
      annotate("rect", ymax=RMSEA.cutoff, ymin=-Inf, xmin=-Inf, xmax=Inf, fill="#F4A58255") +#rgb(1, 0, 0, alpha=.1,maxColorValue=1)) +
      geom_line(size=1.5, color="#0571B0", na.rm = TRUE) +
#       geom_point(size=5, color="#92C5DE", na.rm = TRUE) +
      geom_errorbar(width=0.05, size=1.5, color="#92C5DE") +
      scale_x_continuous(breaks=n.factors) +
      scale_y_continuous(expand=c(0,0)) + 
      labs(title=main, x="Number of Factors", y="RMSEA") +
      theme_bw() +
      theme(panel.grid.minor=element_blank()) + 
      theme(plot.title=element_text(color="gray30", size=30)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
      theme(axis.title=element_text(color="gray30", size=18)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
      theme(axis.text.x=element_text(color="gray50", size=18, vjust=1.3)) + #(eg, V1, V2,...)
      theme(axis.text.y=element_text(color="gray50", size=18))  #(eg, 0.5, 1.0)
    
    print(g)
    
    return(res)
  }
  Scree.PlotGG <- function(R, main="Scree Plot",sub=NULL){
    roots <- eigen(R)$values
    x <- 1:dim(R)[1]    
    ds <- data.frame(x=x, roots=roots)
    g <- ggplot(ds, aes(x=x, y=roots)) +
      annotate("rect", ymax=1, ymin=-Inf, xmin=-Inf, xmax=Inf, fill="#F4A58255") +#rgb(1, 0, 0, alpha=.1,maxColorValue=1)) +
      geom_line(size=1.5, color="#0571B0", na.rm = TRUE) +
      geom_point(size=5, color="#92C5DE", na.rm = TRUE)+
      scale_x_continuous(breaks=x) +
      scale_y_continuous(expand=c(0,0)) +
      labs(title=main, x="Component Number", y="Eigenvalue") +
      theme_bw() +
      theme(panel.grid.minor=element_blank()) + 
      theme(plot.title=element_text(color="gray30", size=30)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
      theme(axis.title=element_text(color="gray30", size=18)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
      theme(axis.text.x=element_text(color="gray50", size=18, vjust=1.3)) + #(eg, V1, V2,...)
      theme(axis.text.y=element_text(color="gray50", size=18))  #(eg, 0.5, 1.0)
    
    print(g)
  }
# eigen plots
  output$eigens <- renderPlot({
    R <- datasetInput()
#     oldPar <- par(tcl=0, mgp=c(1.1, .1, 0), mar=c(2.1, 2.1, 1.1, .1), bty="l") #Set parameters for base graphics
#     Scree.Plot(R)
#     par(oldPar) #Reset to the pre-existing graphic parameters
    Scree.PlotGG(R)
  })
# produces RMSEA plots
  output$RMSEA <- renderPlot({
    R <- datasetInput()
#     oldPar <- par(tcl=0, mgp=c(1.1, .1, 0), mar=c(2.1, 2.1, 1.1, .1), bty="l") #Set parameters for base graphics
#     FA.Stats(R, n.factors=1:input$k, n.obs=get(paste0("n.", dsTag())), RMSEA.cutoff=0.05)
#     par(oldPar) #Reset to the pre-existing graphic parameters
    FA.StatsGG(R, n.factors=1:input$k, n.obs=get(paste0("n.", dsTag())), RMSEA.cutoff=0.05)
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
