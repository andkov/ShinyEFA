rm(list=ls(all=TRUE))
library(datasets)
library(ggplot2) # load ggplot
library(GPArotation)
library(psych)
library(plotrix)
library(sem)
library(stats)

# uploaded <- reactive(data[data$x == input$file1, ])
# Loads three classic datasets from 'psych' package by William Revelle, http://cran.r-project.org/web/packages/psych/
# source("dataprep.R") # begins with rm(list=ls(all=TRUE))
# loads custom funtions by James S. Steier. Visit www.statpower.net for description and download
# source(file.path(getwd(), "sourced", "Steiger R library functions.txt"))
# source(file.path(getwd(), "sourced", "AdvancedFactorFunctions_CF.R"))

# Description of the three datasets from the "psych" package documentation, http://cran.r-project.org/web/packages/psych/psych.pdf
# dscr.cognitive <- "The nine psychological variables from Harman (1967, p 244) are taken from unpublished class notes of K.J. Holzinger with 696 participants." 
# dscr.emotional <- "Eight emotional variables are taken from Harman (1967, p 164) who in turn adapted them from Burt (1939). They are said be from 172 normal children aged nine to twelve. As pointed out by Jan DeLeeuw, the Burt data set is a subset of 8 variables from the original 11 reported by Burt in 1915. That matrix has the same problem."
# dscr.physical <- "The Eight Physical Variables problem is taken from Harman (1976) and represents the correlations between eight physical variables for 305 girls. The two correlated clusters represent four measures of lankiness and then four measures of stockiness. The original data were selected from 17 variables reported in an unpublished dissertation by Mullen (1939)."

# # Descriptions of the tabsets
# dscr.R<-c("Correlogram of the observed variables")
# dscr.eigens<- c("Eigenvalues from the diagonal of D in VDV'")
source(file.path(getwd(), "sourced", "Steiger R library functions.txt"))
source(file.path(getwd(), "sourced", "AdvancedFactorFunctions_CF.R"))
source("dataprep.R") # begins with rm(list=ls(all=TRUE)) 
dscr.cognitive <- "The nine psychological variables from Harman (1967, p 244) are taken from unpublished class notes of K.J. Holzinger with 696 participants." 
dscr.emotional <- "Eight emotional variables are taken from Harman (1967, p 164) who in turn adapted them from Burt (1939). They are said be from 172 normal children aged nine to twelve. As pointed out by Jan DeLeeuw, the Burt data set is a subset of 8 variables from the original 11 reported by Burt in 1915. That matrix has the same problem."
dscr.physical <- "The Eight Physical Variables problem is taken from Harman (1976) and represents the correlations between eight physical variables for 305 girls. The two correlated clusters represent four measures of lankiness and then four measures of stockiness. The original data were selected from 17 variables reported in an unpublished dissertation by Mullen (1939)."


# Define server logic required to summarize and view the selected dataset
shinyServer( function(input, output) {
####        INPUT       ####

# if there is an uploaded file
# load it into object uploaded.data
# and execute source(dataprep2), which contains code for extra dataset
# if not, source (dataprep), the original data prep


# Creates the reactive object contaning the strings of dataset names to be used later
  dsTag <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"="cognitive",
           "Emotional Traits"="emotional",
           "Physical Measures"="physical",
           "Uploaded Data"="uploaded"
    )    
  })
# Dataset
  datasetInput <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=cognitive,
           "Emotional Traits"=emotional,
           "Physical Measures"=physical,
           "Uploaded Data"=uploaded
    )
  })
# Dataset description
  datasetDescription <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=dscr.cognitive,
           "Emotional Traits"=dscr.emotional,
           "Physical Measures"=dscr.physical
    )    
  })
# # Tabset description
# tabsetDescription <- c("The description of the current tabset")
# tabsetDescription <- reactive({
#   switch(EXPR=input$rotation,
#          "SVD eigenvectors"=dscr.cognitive,
#          "Unrotated"=dscr.,
#          "Quartimax"="quartimax", # 1953
#          "Quartimin"="quartimin", # 1953
#          "Varimax"="varimax", # 1958
#          "Promax"="promax"  # 1964
#   )    
# })
# Number of observed variables
  p <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=p.cognitive,
           "Emotional Traits"=p.emotional,
           "Physical Measures"=p.physical,
           "Uploaded Data"=p.uploaded
    )    
  })
# Sample size
  n <- reactive({
    switch(EXPR=input$dataset,
           "Cognitive Abilities"=n.cognitive,
           "Emotional Traits"=n.emotional,
           "Physical Measures"=n.physical,
           "Uploaded Data"=n.uploaded
    )    
  })
# Rotation
  rotationInput <- reactive({
    switch(EXPR=input$rotation,
           svd="svd",
           none="none",
           Varimax="Varimax", # 1958
           promax="promax",
           bifactorT="bifactorT",
           bifactorQ="bifactorQ",
           cfT="cfT",
           cfQ="cfQ"
    )    
  })
# # What tabset is it?
# currentTabset <- reactive({
#     switch(EXPR=)
# if input$Data pyr.A
# if input$Correlations then pyr.R
# if input$Eigens
# RMSEA
# Patterns
# })

####        OUTPUT     ####
# some description
  output$somedscr <- renderPrint ({
    print("Some Description")
  })

# data description
  output$dscr.data <- renderPrint ({
    datasetDescription() 
  })
# tabset description
  output$dscr.tabset <- renderPrint({
    print(c("Description of the current tabset"))
  })
#  correlelogram 
  output$corrgram <- renderPlot({
    corrgram(datasetInput(), upper.panel=panel.conf, lower.panel=panel.shade, type="cor", order=TRUE)
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
 output$PyramidImage<-renderImage({
   get(file.path(getwd(),"images/FApyramid_03.png"))
 })


 output$patternPlot <- renderPlot({  
    # Reactive code
    R <- datasetInput() # the choice of the dataset in ui.R
    k <- input$k # the choice of the number of factors to retain from ui.R
    n.obs <- n()  # choice of the dataset defines  n - its sample size
    p <- p() # the choice of dataset defines p - its number of variables
    # procedures
    A <- stats::factanal(covmat=R, n.obs=n.obs, factors=k, maxit=1000, rotation="none")
    FPM <- A$loadings[1:p, ] # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
    A <- stats::factanal(covmat=R, n.obs=n.obs, factors=k, maxit=1000, rotation="none")
    F <- A$loadings[1:p, ]
    F <- cbind(F, matrix(numeric(0), p, p-k))
    colnames(F) <- paste0("F", 1:ncol(R))
    # output
    # Data prep for ggplot
    dsFORp <- reshape2::melt(FPM, id.vars=rownames(FPM))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
    dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
    dsFORp$positive <- dsFORp$Loading >= 0 # is factor loading positive? color coded in ggplot
    dsFORp$Loading <- abs(as.numeric(dsFORp$Loading)) # Long form
    # The colors for negative and positve values of factor loadings for ggplot
    colors <- c("darksalmon" ,"lightskyblue")
    title <- "Basic Title"
    # Graph definition
    pp <- ggplot(dsFORp, aes(x=Factor, y=Loading, fill=positive)) +
      ggtitle(title) + 
      geom_bar(stat="identity") +
      scale_fill_manual(values=colors) +
      scale_y_continuous(limits=c(0,1)) +
      theme(axis.text.x=element_text(angle=0, hjust=.5)) +
      facet_grid(Variable ~ .)
    print(pp)
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
      FPM <- promax(A, pow)$loadings
#       A <- GPromax(A$loadings, pow=3)
#       FPM <- A$Lh # FPM - Factor Pattern Matrix
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
})
