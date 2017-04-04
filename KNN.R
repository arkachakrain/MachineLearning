require(caTools) || install.packages("caTools")
library(caTools)

require(class) || install.packages("class")
library(class)

require(gmodels) || install.packages("gmodels")
library(gmodels)

#Read The Data file
wbsc <- read.csv("C:/Work/Knowledge/Rprogram/Datasets/wisc_bc_data.csv",stringsAsFactors = FALSE)
summary(wbsc)
wbsc <- wbsc[-1]
#summary(wbsc)
#str(wbsc)
# Create a Normalize function with MIn-Max Normalizer
normalize <- function(x){
  return ((x-min(x)/max(x)-min(x)))
}
wbsc_n <- as.data.frame(lapply(wbsc[2:31],normalize))


str(wbsc_n)
str(wbsc)
wbsc_n$diagnosis <- wbsc$diagnosis
str(wbsc_n)

# Training and Test Data Set - CATool Package
set.seed(144)
spl <- sample.split(wbsc_n$diagnosis , 0.7)
train <- subset(wbsc_n, spl == TRUE)
test <- subset(wbsc_n , spl == FALSE)
str(wbsc_train)

wbsc_train <- train[1:30]
wbsc_test <- test[1:30]
wbsc_train_labels <- train$diagnosis
wbsc_test_labels <-test$diagnosis
# Run KNN - Package Class

wbcd_test_pred <- knn(train = wbsc_train, test = wbsc_test,
                      cl = wbsc_train_labels, k = 21)  
CrossTable(x = wbsc_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

########################Z Score Normalization ############################
########################Accuracy Improved################################
wbsc_z <- as.data.frame(scale(wbsc))
wbsc_z$diagnosis <- wbsc$diagnosis
str(wbsc_z)
str(wbsc)

# Training and Test Data Set - CATool Package
set.seed(144)
spl <- sample.split(wbsc_z$diagnosis , 0.7)
train <- subset(wbsc_z, spl == TRUE)
test <- subset(wbsc_z , spl == FALSE)

wbsc_train <- train[1:30]
wbsc_test <- test[1:30]
wbsc_train_labels <- train$diagnosis
wbsc_test_labels <-test$diagnosis
# Run KNN - Package Class

wbcd_test_pred <- knn(train = wbsc_train, test = wbsc_test,
                      cl = wbsc_train_labels, k = 21)  
CrossTable(x = wbsc_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#####################Improve MOdel Accuracy#############################
################Try diff values of K####################################

#####Accuracy 95% with K=1 False Postive = 4 False Negative = 4
wbcd_test_pred <- knn(train = wbsc_train, test = wbsc_test,
                      cl = wbsc_train_labels, k = 1)
CrossTable(x = wbsc_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#####Accuracy 96.5% with K=3 False Negative =6 False Positive = 0
wbcd_test_pred <- knn(train = wbsc_train, test = wbsc_test,
                      cl = wbsc_train_labels, k = 3)
CrossTable(x = wbsc_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#####Accuracy 96.5% with K=3 False Negative =6 False Positive =0
wbcd_test_pred <- knn(train = wbsc_train, test = wbsc_test,
                      cl = wbsc_train_labels, k = 5)
CrossTable(x = wbsc_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)
