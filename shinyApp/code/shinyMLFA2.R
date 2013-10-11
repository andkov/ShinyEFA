library(shiny)
runApp("shinyapp")

pattern<-read.csv("shinyapp/fpm.csv")

colors<- c("darksalmon" ,"lightskyblue")
title<- paste0("Shiny pattern")
ylims<-c(0,3)           # max for eigenvalue plot 
width<-450              # width of pattern in pixels
height<-900             # height of pattern in pixels
width2<-300            # width of scree in pixels
height2<-200            # height of scree in pixels

dsLong <- reshape2::melt(pattern, id.vars=c("Varname","kappa"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsLong <- plyr::rename(dsLong, replace=c(variable="factor",value="loading"))
dsFORp<-dsLong
dsFORp$positive <- dsFORp$loading >= 0 # positive value?
dsFORp$loading<-abs(dsFORp$loading)
head(dsFORp,20)
str(dsFORp)

pT<-ggplot(dsFORp, aes(x=pc, y=value, fill=positive))+
  ggtitle(title)+ 
  geom_bar()+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text.x =element_text(angle=60,hjust=1))+
  facet_grid(measure~.)
pT

#######

# str(fit$CF)
# pattern<-fit$CF$F
# write.table(x=pattern,sep=",",file="C:/wamp/www/project-folder/data/pat1.csv")
# rownames(fit$CFq$F)
# fit$CFq$Phi

#####

runExample("03_reactivity")




