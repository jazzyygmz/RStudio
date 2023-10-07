
install.packages("data.table")

library(data.table)
set.seed(1)

per <- read.csv(file.choose()) 
head(per)


pca <- prcomp(per[,2:length(per)], retx=TRUE, scale=TRUE)
summary(pca)
pca

attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")
factor1 <- pca$rotation[,1]*pca$sdev[1]; factor2 <- pca$rotation[,2]*pca$sdev[2]; path <- rep(1, nrow(attribute))
pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))
pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))
pca_attributes <- rbind(pca_factors, pca_origin)

write.csv(pca_attributes, file = file.choose(new=TRUE), row.names = FALSE) 

score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores <- subset(cbind(per, score1, score2), select = c(Restaurant, score1, score2))

## Name file Preference_Scores.csv
write.csv(preferences, file = file.choose(new=TRUE), row.names = FALSE) 
