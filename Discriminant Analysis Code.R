##################################################
## Discriminant Analysis and Classification in R #
##################################################

## Install Packages (if needed)
install.packages("MASS")

## Load Packages and Set Seed
library(MASS)
set.seed(1)

## Read in Segment Data and Classification Data 
seg <- read.csv(file.choose()) ## Choose Segmentation Results.csv file
class <- read.csv(file.choose()) ## Choose Retail Classification Data.csv file

## Run Discriminant Analysis to predict segment placement as a function of the descriptors
## Only 5 discriminant functions will be returned since there are equal number of descriptors as segments
fit <- lda(segment ~ married + own_home + household_size + income + age, data = seg)
fit ## print the summary statistics of your discriminant analysis

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))
## Because at least one of the discriminant functions is significant, we know that at least some of our descriptors are helpful at discriminating our segments

## Check Disciminant Model Fit (Confusion Matrix)
pred.seg <- predict(fit)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
## This is our Prospect Database - we are trying to predict which segment they are a part of
pred.class <- predict(fit, class)$class
tclass <- table(pred.class)
tclass # print table

## Add Predicted Segment to Classification Data
class.seg <- cbind(class, pred.class)
write.csv(class.seg, file = file.choose(new=TRUE), row.names = FALSE) ## Name file Classification Prediction.csv
