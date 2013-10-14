# output$patternPlot <- renderPlot({
rm(list=ls(all=TRUE))
library(psych)
data(Harman)
R<-Harman.Holzinger
  # Reactive code 
#       R <- datasetInput() # the choice of the dataset in ui.R
#       k <- input$k # the choice of the number of factors to retain from ui.R
#       n.obs <- n()  # choice of the dataset defines  n - its sample size
#       p <- p() # the choice of dataset defines p - its number of variables
  # Reactive code 
        R <- R # the choice of the dataset in ui.R
        k <- 4 # the choice of the number of factors to retain from ui.R
        n.obs <- 696  # choice of the dataset defines  n - its sample size
        p <- ncol(R) # the choice of dataset defines p - its number of variables
  
  
  # procedures
  A <- stats::factanal(covmat=R, n.obs=n.obs, factors=k, maxit=1000, rotation="none")
  FPM <- A$loadings[1:p, ] # FPM - Factor Pattern Matrix
  FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
  colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs

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