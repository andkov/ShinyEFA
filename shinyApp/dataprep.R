rm(list=ls(all=TRUE))
library(datasets)
library(ggplot2) # load ggplot
library(psych)
data(Harman)
# Harman.Holzinger: 9 x 9 correlation matrix of cognitive ability tests, N = 696.
cognitive<-Harman.Holzinger
n.cognitive<-696
p.cognitive<-nrow(cognitive)
colnames.cognitive<-c("Word Meaning",
                     "Sentence Completion",
                     "Odd Words",
                     "Mixed Arithmetic",
                     "Remainders",
                     "Missing Numbers",
                     "Gloves",
                     "Boots",
                     "Hatchets")
colnames(cognitive)<-colnames.cognitive
rownames(cognitive)<-colnames.cognitive



# Harman.Burt: a 8 x 8 correlation matrix of “emotional" items. N = 172
emotional <-Harman.Burt
# colnames.emotional<-c("Sociability",
#                       "Sorrow",
#                       "Tenderness",
#                       "Joy ",
#                       "Wonder",
#                       "Disgust ",
#                       "Anger",
#                       "Fear")
# colnames(emotional)<-colnames.emotional
# rownames(emotional)<-colnames.emotional
n.emotional<-172
p.emotional<-nrow(emotional)
# emotional matrix is not positive definity due to original typo
# see explanations in the psych package documentation under Harman.Burt
emotional["Tenderness","Sorrow"]<-.81
emotional["Sorrow","Tenderness"]<-.81
# colnames(emotional)<-c("v1","v2","v3","v4","v5","v6","v7","v8")

rm(list=setdiff(ls(),c("cognitive","emotional", "political")))


