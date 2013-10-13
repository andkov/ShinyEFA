
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
vars.cognitive <- c("Word Meaning",
                    "Sentence Completion",
                    "Odd Words",
                    "Mixed Arithmetic",
                    "Remainders",
                    "Missing Numbers",
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
                   "Chest width")
vars.physycal <- colnames(Harman.8)
colnames(physical) <- vars.physycal
rownames(physical) <- vars.physycal
n.physical <- 305
p.physical <- nrow(physical)

# The data uploaded by the user
uploaded<-cor(uploaded.data)
vars.uploaded<-colnames(uploaded)
n.uploaded<-nrow(uploaded.data)
p.uploaded<-ncol(uploaded.data)

rm(list=setdiff(ls(), c("cognitive", "emotional", "physical","uploaded",
                        "vars.cognitive", "vars.emotional","vars.physical","vars.uploaded",
                        "n.cognitive", "n.emotional", "n.physical","n.uploaded",
                        "p.cognitive", "p.emotional", "p.physical","p.uploaded"
                        )))


