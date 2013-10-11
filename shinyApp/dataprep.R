rm(list=ls(all=TRUE))
library(shiny)
library(datasets)
library(ggplot2) # load ggplot
library(GPArotation)
data(Harman8)
library(psych)
data(Harman)
# Correlations of eight physical variables (from Harman, 1966), N = 305
physical<-Harman.8
n.physical<-305
write.csv(physical,"physical.csv")
# Harman.Holzinger: 9 x 9 correlation matrix of cognitive ability tests, N = 696.
cognitive<-Harman.Holzinger
n.cognitive<-696
write.csv(cognitive,"cognitive.csv")
# Harman.Burt: a 8 x 8 correlation matrix of “emotional" items. N = 172
emotional <-Harman.Burt
n.emotional<-172
write.csv(emotional,"emotional.csv")
# ##### Adding rotations to AdvancedFactorFunctions #####
# AthleticsData <- read.csv(file.path(getwd(),"shinyApp/data","AthleticsData.csv"))
# R<-cor(AthleticsData) # Correlation matrix, 8 physical attribues of 305 girsl (from Harman, 1976)
rm(list=setdiff(ls(),c("physical","cognitive","emotional")))

# # to be used in shinyApp
# source("Steiger R library functions.txt"))
# source("AdvancedFactorFunctions_CF.R"))
# to be used for testing
source(file.path(getwd(),"code/sourced","Steiger R library functions.txt"))
source(file.path(getwd(),"code/sourced","AdvancedFactorFunctions_CF.R"))

R<-physical #input$dataset

# R<-as.matrix(Harman.Holzinger)
screePlot<-Scree.Plot(R)
FA.Stats(R,n.factors=1:4,n.obs=305, RMSEA.cutoff=0.05)
mlfa.out<-MLFA(R,n.factor=3, n.obs=305)
fit<-mlfa.out