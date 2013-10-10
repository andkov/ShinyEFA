rm(list=ls(all=TRUE))

library(ggplot2)
fpm<-read.csv("data/fpm.csv")
str(fpm)
colors<- c("darksalmon" ,"lightskyblue")
title<-"Basic Title"

fpm<-fpm[which(fpm$Rotation=="varimax"),]

dsFORp <- reshape2::melt(fpm, id.vars=c("Oblique","Rotation","Kappa","Varname"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsFORp <- plyr::rename(dsFORp, replace=c(variable="factor",value="loading"))
dsFORp$positive <- dsFORp$loading >= 0 # positive value?
dsFORp$loading<-abs(as.numeric(dsFORp$loading))
head(dsFORp,20)
str(dsFORp)


p<-ggplot(dsFORp, aes(x=factor, y=loading, fill=positive))+
  ggtitle(title)+ 
  geom_bar(stat="identity")+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text.x =element_text(angle=0,hjust=.5))+
  facet_grid(Varname~.)
p