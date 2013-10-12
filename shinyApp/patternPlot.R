require(ggplot2)

# A <- factanal(covmat=R,n.obs=n.obs,factors=k,maxit=1000,rotation="none")
# F<-A$loadings[1:p,]
# F<-cbind(F,matrix(numeric(0),p,p-k))
# colnames(F)<-paste0("F",1:ncol(R))

# # Transforms dataset into a long format to be used in ggplot
dsFORp <- reshape2::melt(F, id.vars=rownames(F))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable",Var2="Factor",value="Loading"))
dsFORp$positive <- dsFORp$Loading >= 0 # is factor loading positive? color coded in ggplot
dsFORp$Loading<-abs(as.numeric(dsFORp$Loading)) # Long form

# The colors for negative and positve values of factor loadings
colors<- c("darksalmon" ,"lightskyblue")
title<-"Basic Title"

pp<-ggplot(dsFORp, aes(x=Factor, y=Loading, fill=positive))+
  ggtitle(title)+ 
  geom_bar(stat="identity")+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text.x =element_text(angle=0,hjust=.5))+
  facet_grid(Variable~.)
print(pp)