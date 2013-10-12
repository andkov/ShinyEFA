library(datasets)
library(ggplot2) # load ggplot
library(psych)
library(plotrix)
library(sem)

getwd()
setwd("C:/Users/kovalav/Documents/GitHub/ShinyEFA/shinyApp")
# Loads three classic datasets from 'psych' package by William Revelle, http://cran.r-project.org/web/packages/psych/
source("dataprep.R") # begins with rm(list=ls(all=TRUE))
# loads custom funtions by James S. Steier. Visit www.statpower.net for description and download
source(file.path(getwd(),"sourced","Steiger R library functions.txt"))
source(file.path(getwd(),"sourced","AdvancedFactorFunctions_CF.R"))

# Reactive code
# R<-input$dataset # the choice of the dataset in ui.R
# k<-input$k # the choice of the number of factors to retain from ui.R
# n.obs<-n()  # choice of the dataset defines  n - its sample size
# p<-p() # the choice of dataset defines p - its number of variables

# Reactive must resolve in such hard code operationalization as example:
R<-cognitive # What dataset?
k<-5 # How many factors/latent variables?
n.obs<-n.cognitive # How big is sample size?
p<-p.cognitive # How many observed variables?
# The initial analysis
A <- factanal(covmat=R,n.obs=n.obs,factors=k,maxit=1000,rotation="none")
F<-A$loadings[1:p(),]
F<-cbind(F,matrix(numeric(0),p(),p()-input$k))
colnames(F)<-paste0("F",1:ncol(R))