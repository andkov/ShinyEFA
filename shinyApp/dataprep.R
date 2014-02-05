
library(datasets)
library(ggplot2) # load ggplot
library(psych)
library(plotrix)
library(sem)
library(GPArotation)



data(Harman)


# Harman.Holzinger: 9 x 9 correlation matrix of cognitive ability tests, N = 696.
cognitive <- Harman.Holzinger
n.cognitive <- 696
p.cognitive <- nrow(cognitive)
vars.cognitive <- c("Word.Mean",
                     "Sent.Compl",
                     "Odd.Words",
                     "Mix.Arith",
                     "Remainders",
                     "Miss.Num.",
                     "Gloves",
                     "Boots",
                     "Hatchets")
colnames(cognitive) <- vars.cognitive
rownames(cognitive) <- vars.cognitive

# Harman.Burt: a 8 x 8 correlation matrix of “emotional" items. N = 172
emotional <- Harman.Burt
vars.emotional <- c("Sociability",
                      "Sorrow",
                      "Tenderness",
                      "Joy ",
                      "Wonder",
                      "Disgust ",
                      "Anger",
                      "Fear")
colnames(emotional) <- vars.emotional
rownames(emotional) <- vars.emotional
n.emotional <- 172
p.emotional <- nrow(emotional)
# emotional matrix is not positive definity due to original typo
# see explanations in the psych package documentation under Harman.Burt
emotional["Tenderness", "Sorrow"] <- .81
emotional["Sorrow", "Tenderness"] <- .81

# Harman.8: 8 x 8 correlation matrix of physical measures of 305 girls
physical <- Harman.8
vars.physical <- c("Height",
                 "Arm span",
                 "Length of forearm",
                 "Length of lower leg",
                 "Weight",
                 "Bitrochanteric diameter",
                 "Chest girth",
                 "Ches width")
vars.physycal <- colnames(Harman.8)
colnames(physical) <- vars.physycal
rownames(physical) <- vars.physycal
n.physical <- 305
p.physical <- nrow(physical)

#  24 psychological tests, N=145, Harman p.125 
Harman74<-as.matrix(datasets::Harman74.cor$cov)
vars.Harman74<-colnames(Harman74)
n.Harman74<-145
p.Harman74<-nrow(Harman74)

AthleticsData <- read.csv("http://statpower.net/Content/319SEM/Lecture%20Notes/AthleticsData.csv")

#  from GPArotation, no information provided in the package
# data(Thurstone)
Thurstone<-AthleticsData # Change AthleticData bakc to Thurstone 
vars.Thurstone<-colnames(Thurstone)
n.Thurstone<-200 # not sure of the number
p.Thurstone<-nrow(Thurstone)


# rm(list=setdiff(ls(), c("cognitive", "emotional", "physical",
#                        "vars.cognitive", "vars.emotional","vars.physical",
#                        "n.cognitive", "n.emotional", "n.physical",
#                        "p.cognitive", "p.emotional", "p.physical")))


