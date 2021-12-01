library(e1071)  # for machine learning methods
#install.packages("mlr")
library(mlr)
library(caret)

library(datasets)
library(ggplot2)
library(MASS) 


MyPath = "/Users/junjiexie/Documents/gu校内生活/additionalData/"
RecordDatasetName = "pres16results.csv"

setwd(MyPath)
RecordDF_all <- read.csv(RecordDatasetName, stringsAsFactors = TRUE)
head(RecordDF_all)

RecordDF_A_all <- subset.data.frame(RecordDF_all, select = c("cand", "st", "pct_report", "votes", "total_votes", "pct", "lead"))
nrow(RecordDF_A_all)
set.seed(250)
RecordDF_A = RecordDF_A_all[sample(nrow(RecordDF_A_all), 5000), ]
head(RecordDF_A)
RecordDF_A <- na.omit(RecordDF_A)
rownames(RecordDF_A) <- 1:nrow(RecordDF_A)

samplerownums<- sample(nrow(RecordDF_A),floor(nrow(RecordDF_A)*0.3))
samplerownums
(Testset <- RecordDF_A[samplerownums,])
(TestLabels <- Testset['lead'])
Testset_nolabel<- subset(Testset,select = -c(lead))
TestLabels
Trainset <- RecordDF_A[-samplerownums,]

## Polynomial Kernel...
tuned_cost <- tune(svm,lead~.,data=Trainset,
                   kernel="polynomial", 
                   ranges=list(cost=c(.01,.1,.2,.3)))
summary(tuned_cost)  ## This shows that the best cost is .1
SVM_fit_P <- svm(lead~., data=Trainset, 
                 kernel="polynomial", cost=.2, 
                 scale=TRUE)
print(SVM_fit_P)
(pred_P <- predict(SVM_fit_P, Testset_nolabel, type="class"))
(Ptable <- table(predict=pred_P, real=Testset$lead))
Ptable

## Radial Kernel...
tuned_cost <- tune(svm,lead~.,data=Trainset,
                   kernel="radial", 
                   ranges=list(cost=c(.01,.1,.2,.3)))
summary(tuned_cost)
SVM_fit_P_2 <- svm(lead~., data=Trainset, 
                 kernel="radial", cost=.01, 
                 scale=TRUE)
print(SVM_fit_P_2)
(pred_P_2 <- predict(SVM_fit_P_2, Testset_nolabel, type="class"))
(Ptable_2 <- table(predict=pred_P_2, real=Testset$lead))
Ptable_2

  ## Linear Kernel...
tuned_cost <- tune(svm,lead~.,data=Trainset,
                   kernel="linear", 
                   ranges=list(cost=c(.01,.1,.2,.3)))
summary(tuned_cost)  ## This shows that the best cost is .1
SVM_fit_P_3 <- svm(lead~., data=Trainset, 
                 kernel="linear", cost=.3, 
                 scale=TRUE)
print(SVM_fit_P_3)
(pred_P_3 <- predict(SVM_fit_P_3, Testset_nolabel, type="class"))
(Ptable_3 <- table(predict=pred_P_3, real=Testset$lead))
Ptable_3

