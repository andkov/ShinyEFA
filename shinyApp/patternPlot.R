
fpmFunction <- function( FPM.matrix, mainTitle=NULL ){ 
  
  roundingDigits <- 2 #Let the user define?
  stripSize <- 12  #Let the user define?
 
  # output
  # Data prep for ggplot
  dsFORp <- reshape2::melt(FPM.matrix, id.vars=rownames(FPM.matrix))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
  dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
  dsFORp$Positive <- ifelse(dsFORp$Loading >= 0, "Positive", "Negative") #Or see Recipe 10.8
  dsFORp$LoadingAbs <- abs(dsFORp$Loading) # Long form
  dsFORp$LoadingPretty <- round(abs(dsFORp$Loading), roundingDigits) # Long form
  dsFORp$VariablePretty <- gsub(pattern="_", replacement="\n", x=dsFORp$Variable)
  # colors <- c("FALSE"="darksalmon" ,"TRUE"="lightskyblue") # The colors for negative and positve values of factor loadings for ggplot
  colorsFill <- c("Positive"="#A6CEE3" ,"Negative"="#B2DF8A") # The colors for negative and positve values of factor loadings for ggplot
  colorsColor <- c("Positive"="black" ,"Negative"="black") # The colors for negative and positve values of factor loadings for ggplot
  
  
  # Graph definition
  pp <- ggplot(dsFORp, aes(x=Factor, y=LoadingAbs, fill=Positive, color=Positive, label=LoadingPretty)) +
    geom_bar(stat="identity") +
    geom_text(y=0, vjust=-.1) +
    scale_color_manual(values=colorsColor, guide="none") +
    scale_fill_manual(values=colorsFill) +
    #   scale_fill_discrete(h=c(0,360)+15, c=100, l=65, h.start=0, direction=1, na.value="grey50") + #http://docs.ggplot2.org/0.9.3/scale_hue.html
    scale_y_continuous(limits=c(0,1), breaks=c(.5, 1), expand=c(0,0)) +
    facet_grid(VariablePretty ~ .) +
    labs(title=mainTitle, x="FFF", y="Loadings (Absolute)", fill=NULL) + 
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
  
  
  return( pp )
}