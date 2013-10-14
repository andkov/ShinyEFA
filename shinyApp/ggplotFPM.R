# output$patternPlot <- renderPlot({
rm(list=ls(all=TRUE))
options("scipen"=10, "digits"=5) 
library(psych)
library(ggplot2)
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

roundingDigits <- 2 #Let the user define?
stripSize <- 12  #Let the user define?
title <- NULL #"Basic Title" #Read from the dataset name or rotation?
# output
  # Data prep for ggplot
dsFORp <- reshape2::melt(FPM, id.vars=rownames(FPM))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
dsFORp$Positive <- ifelse(dsFORp$Loading >= 0, "Positive", "Negative") #Or see Recipe 10.8
dsFORp$LoadingAbs <- abs(dsFORp$Loading) # Long form
dsFORp$LoadingPretty <- round(abs(dsFORp$Loading), roundingDigits) # Long form
dsFORp$VariablePretty <- gsub(pattern="_", replacement="\n", x=dsFORp$Variable)
# colors <- c("FALSE"="darksalmon" ,"TRUE"="lightskyblue") # The colors for negative and positve values of factor loadings for ggplot
colorsFill <- c("Positive"="#A6CEE3" ,"Negative"="#B2DF8A") # The colors for negative and positve values of factor loadings for ggplot
colorsColor <- c("Positive"="#00CEE3" ,"Negative"="#00DF8A") # The colors for negative and positve values of factor loadings for ggplot


  # Graph definition
pp <- ggplot(dsFORp, aes(x=Factor, y=LoadingAbs, fill=Positive, color=Positive, label=LoadingPretty)) +
  geom_bar(stat="identity") +
  geom_text(y=0, vjust=-.1) +
  scale_color_manual(values=colorsColor, guide="none") +
  scale_fill_manual(values=colorsFill) +
#   scale_fill_discrete(h=c(0,360)+15, c=100, l=65, h.start=0, direction=1, na.value="grey50") + #http://docs.ggplot2.org/0.9.3/scale_hue.html
  scale_y_continuous(limits=c(0,1.2), breaks=c(.5, 1), expand=c(0,0)) +
  facet_grid(VariablePretty ~ .) +
  labs(title=title, x="FFF", y="Loadings (Absolute)", fill=NULL) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) + 
#   theme(axis.label=element_text(color="gray30")) +  
  theme(axis.text.y=element_text(color="gray50")) +   
  theme(strip.text.y=element_text(angle=0, size=stripSize))

{
  if( k < p ) {
    pp <- pp + theme(legend.position=c(1, 0), legend.justification=c(1, 0)) 
    pp <- pp + theme(legend.background=element_rect(fill="gray70"))
  }
  else {
    pp <- pp + theme(legend.position="left")
  }
}
print(pp)




# # Below the top
# ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
#   geom_bar(stat="identity") +
#   geom_text(aes(label=Weight), vjust=1.5, colour="white")



