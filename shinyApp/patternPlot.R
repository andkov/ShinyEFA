
fpmFunction <- function( FPM.matrix, mainTitle=NULL ){ 
  # Data prep for ggplot
  dsFORp <- reshape2::melt(FPM.matrix, id.vars=rownames(FPM.matrix))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
  dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
  dsFORp$positive <- dsFORp$Loading >= 0 # is factor loading positive? color coded in ggplot
  dsFORp$Loading <- abs(as.numeric(dsFORp$Loading)) # Long form
  # The colors for negative and positve values of factor loadings for ggplot
  colors <- c("darksalmon" ,"lightskyblue")
  title <- "Basic Title"
  # Graph definition
  pp <- ggplot(dsFORp, aes(x=Factor, y=Loading, fill=positive)) +
    labs(title=mainTitle) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=colors) +
    scale_y_continuous(limits=c(0,1)) +
    theme(axis.text.x=element_text(angle=0, hjust=.5)) +
    facet_grid(Variable ~ .)
  return( pp )
}