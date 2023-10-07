######################################
## Perceptual and Preference Mapping#
######################################

## Install Packages (if needed)
install.packages("data.table")

## Load Packages and Set Seed
library(data.table)
set.seed(1)

## Read in perception and preference data
per <- read.csv(file.choose()) ## Choose Perceptions.csv file
head(per)
pref <- read.csv(file.choose()) ## Choose Preferences.csv file
head(pref)

## Run Principle Components Analysis on Perceptions
pca <- prcomp(per[,2:length(per)], retx=TRUE, scale=TRUE)
summary(pca)
pca

## Perceptual Map Data - Attribute Factors and CSV File
attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")
factor1 <- pca$rotation[,1]*pca$sdev[1]; factor2 <- pca$rotation[,2]*pca$sdev[2]; path <- rep(1, nrow(attribute))
pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))
pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))
pca_attributes <- rbind(pca_factors, pca_origin)
write.csv(pca_attributes, file = file.choose(new=TRUE), row.names = FALSE) ## Name file Perceptions_Attributes.csv

## Perceptual Map Data - Brand Factors and CSV File
score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores <- subset(cbind(per, score1, score2), select = c(Brand, score1, score2))
write.csv(pca_scores, file = file.choose(new=TRUE), row.names = FALSE) ## Name file Perceptions_Scores.csv

## Preference Map Data - Respondent Preferences and CSV File
pref1 <- data.matrix(pref[,2:ncol(pref)])%*%(cbind(score1,score2))
pref1[,1] <- (pref1[,1]/max(abs(pref1[,1]))); pref1[,2] <- (pref1[,2]/max(abs(pref1[,2])))
preferences <- subset(cbind(pref, pref1, preference = rep(1,nrow(pref))), select = c(Respondents, score1, score2, preference))
write.csv(preferences, file = file.choose(new=TRUE), row.names = FALSE) ## Name file Preference_Scores.csv
