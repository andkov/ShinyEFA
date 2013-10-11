# library(shiny)
# runApp("shinyapp")

#install.packages("GPArotation","psych")
library(GPArotation)
data(Harman.8)
library(psych)
data(Harman)

##### Adding rotations to AdvancedFactorFunctions #####
# AthleticsData <- read.csv(file.path(getwd(),"datasets","AthleticsData.csv"))
# R<-cor(AthleticsData) # Correlation matrix, 8 physical attribues of 305 girsl (from Harman, 1976)

source(file.path(getwd(),"code/sourced","Steiger R library functions.txt"))
source(file.path(getwd(),"code/sourced","AdvancedFactorFunctions_CF.R"))
# source(file.path(getwd(),"code/sourced","AdvancedFactorFunctions.R"))

R<-as.matrix(Harman.Holzinger)
Scree.Plot(R)
FA.Stats(R,n.factors=1:4,n.obs=305, RMSEA.cutoff=0.05)
mlfa.out<-MLFA(R,n.factor=3, n.obs=305)
fit<-mlfa.out

# For quick access of individual solution from MLFA
str(fit)
fit$Unrotated
fit$Varimaxye
fit$Promax
fit$Quartimin
fit$Bifactor
fit$BifactorOblique
fit$CF
fit$CFcust
fit$CFq
fit$CFqcust

pattern<-fit$Unrotated$F
oldpat<-pattern
pathScree<-file.path(getwd(),"code","sourced","scree.R")
pathPattern<-file.path(getwd(),"code","sourced","factor pattern.R")
#          R            Correlation/Covariance matrix - R  
#      F      F'        Grahm-Factors                 - F
#   VD1/2   D1/2V'      Principle Component patterns  - pcPattern
#   V     D     V'      Earhart-Young decomposition   - V, D   
#      Rv = cv          Eigenvalues and Eigenvectors  
######   Producing graphs   # Ctrl+Alt+E - Run from line to end
# palette <- choose_palette() # run to initiate a dialogue to choose palette
# colors<-palette(2) # assign the vector containing the N colorcodes 
colors<- c("darksalmon" ,"lightskyblue")
#choose where the file will be stored
pathImageOut<-file.path(getwd(),"shinyapp/images")
pattern<-F   # matrix for the factor pattern
drawing<- "F"  # name of file with graph

title<- paste0("Shiny pattern")
ylims<-c(0,3)           # max for eigenvalue plot 
width<-450              # width of pattern in pixels
height<-900             # height of pattern in pixels
width2<-300            # width of scree in pixels
height2<-200            # height of scree in pixels

source(pathPattern) #produces the graph of pattern loadings


Dplus<- Dplus   # matrix with eigenvalues
title2<- paste0("Scree plot ",whatsolution," rotation : ",whatrotation)
title3<- paste0("Var Explnd from ",whatsolution,".","rotation - ",whatrotation)
source(pathScree) 
#######

# str(fit$CF)
# pattern<-fit$CF$F
# write.table(x=pattern,sep=",",file="C:/wamp/www/project-folder/data/pat1.csv")
# rownames(fit$CFq$F)
# fit$CFq$Phi

#####




