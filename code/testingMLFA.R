rm(list=ls(all=TRUE))
# install.packages("GPArotation")
# install.packages("psych")
library(GPArotation)
data(Harman.8)
library(psych)
data(Harman)

##### Adding rotations to AdvancedFactorFunctions #####
AthleticsData <- read.csv(file.path(getwd(),"shinyApp/data","AthleticsData.csv"))
R<-cor(AthleticsData) # Correlation matrix, 8 physical attribues of 305 girsl (from Harman, 1976)

source(file.path(getwd(),"code/sourced","Steiger R library functions.txt"))
source(file.path(getwd(),"code/sourced","AdvancedFactorFunctions_CF.R"))
# source(file.path(getwd(),"code/sourced","AdvancedFactorFunctions.R"))

# R<-as.matrix(Harman.Holzinger)
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
fit$CFq

# L<-fit$Varimax$F
L<-fit$Promax$F

cfT<-cfT(L, Tmat=diag(ncol(L)), kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)
cfQ<-cfQ(L, Tmat=diag(ncol(L)), kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)
LcfT<-cfT$loadings
LcfQ<-cfQ$loadings
L
LcfT
LcfQ






