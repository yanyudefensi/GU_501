##################
# Naive Bayes
# Record Data
#
# 
##################################
##
##  RECORD DATA IS HERE:
##  https://drive.google.com/file/d/18dJPOiiO9ogqOibJppc0lsDiQ2-bQs0f/view?usp=sharing
#########################################################
## Gates
#########################################################
library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
##ONCE: install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

library(naivebayes)
#Loading required packages
#install.packages('tidyverse')
library(tidyverse)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('caret')
library(caret)
#install.packages('caretEnsemble')
library(caretEnsemble)
#install.packages('psych')
library(psych)
#install.packages('Amelia')
library(Amelia)
#install.packages('mice')
library(mice)
#install.packages('GGally')
library(GGally)
library(e1071)
library(randomForest)
library(pROC)


####### USE YOUR OWN PATH ############
# setwd("C:\\Users\\profa\\Documents\\RStudioFolder_1\\DrGExamples\\ANLY501\\DATA\\")
# ##
# ## Raad in the dataset
# 
# StudentDataFile="SummerStudentDataset_Record_Mixed_Labeled.csv"
# head(StudentDF<-read.csv(StudentDataFile))


MyPath = "/Users/junjiexie/Documents/gu校内生活/additionalData/"
RecordDatasetName = "pres16results.csv"

setwd(MyPath)
RecordDF_all <- read.csv(RecordDatasetName, stringsAsFactors = TRUE)
head(RecordDF_all)

RecordDF_A_all <- subset.data.frame(RecordDF_all, select = c("cand", "st", "pct_report", "votes", "total_votes", "pct", "lead"))
nrow(RecordDF_A_all)
RecordDF_A = RecordDF_A_all[sample(nrow(RecordDF_A_all), 5000), ]
head(RecordDF_A)
RecordDF_A <- na.omit(RecordDF_A)

str(RecordDF_A)

###########################################################
##
##          Prepare the record data for 
##          analysis. Create testing and
##          training sets.
##
##          REMEMBER - we are assuming here that
##          the record dataset is already 100% clean
################################################################
## Record Data..................
## MAKE test and train data

# str(StudentDF)
# ## If necessary - correct data types
# StudentDF$Decision <- as.factor(StudentDF$Decision)
# StudentDF$Gender <- as.factor(StudentDF$Gender)
# StudentDF$State <- as.factor(StudentDF$State)


(Size <- (as.integer(nrow(RecordDF_A)/4)))  ## Test will be 1/4 of the data
(SAMPLE <- sample(nrow(RecordDF_A), Size, replace = FALSE))

(DF_Test<-RecordDF_A[SAMPLE, ])
(DF_Train<-RecordDF_A[-SAMPLE, ])
##
## REMOVE the labels and KEEP THEM
##   
## 

##################################### REMOVE AND SAVE LABELS...
## Copy the Labels
(DF_Test_Labels <- DF_Test$lead)
## Remove the labels
DF_Test_NL<-DF_Test[ , -which(names(DF_Test) %in% c("lead"))]
(DF_Test_NL[1:5, 1:5])
## Check size
(ncol(DF_Test_NL))
#(DF_Test_Student_NL)
## Train...--------------------------------
## Copy the Labels
(DF_Train_Labels <- DF_Train$lead)
## Remove the labels
DF_Train_NL<-DF_Train[ , -which(names(DF_Train) %in% c("lead"))]
(DF_Train_NL[1:5, 1:5])
## Check size
(ncol(DF_Train_NL))
#(DF_Train_Student_NL)

###############################################################
##
##                         NAIVE BAYES
##
###############################################################

############
## What do we have?

## ----------------------------
## For Record data, we have:
##-----------------------------
## DF_Test_Student_NL  ## Testset
## DF_Train_Student_NL  ## Training set
## Label name is "Decision"
## Test labels:
## DF_Test_Student_Labels
## DF_Train_Student_Labels
######################################


-------------------------------------------------------------
  
  ## Just FYI......................if memory or overflow issues....
  ## memory.limit()
  #data=DF_Train[,1:5000]
  #(data[1:5, 1:5])
  ##
  
#################################################################
##
####### RUN Naive Bayes --------------------------------------------
##
######################################################################

## TEXT DATA
## https://www.rdocumentation.org/packages/e1071/versions/1.7-3/topics/naiveBayes

# NB_e1071_2 = naiveBayes(DF_Train_NL, DF_Train_Labels, laplace = 1)
NB_e1071_2 = naiveBayes(DF_Train_NL, DF_Train_Labels, laplace = 1)
NB_e1071_2 = naiveBayes(DF_Train_NL, DF_Train_Labels, laplace = 1)
NB_e1071_Pred <- predict(NB_e1071_2, DF_Test_NL,type='class')

#feature importance
VariableImportancePlot <- randomForest(as.factor(lead) ~. , data = DF_Train, importance=TRUE)
varImpPlot(VariableImportancePlot)

#confusion matrix
table <- data.frame(confusionMatrix(DF_Test_Labels, NB_e1071_Pred)$table)
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "Good", "Bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

confusionMatrix <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 25, size = 8) +
  scale_fill_manual(name = " ", values = c(Good = "#2FC70A", Bad = "#F8100C")) +
  scale_alpha(name = " ") +
  theme_classic() +
  xlim(rev(levels(table$Reference))) +
  scale_y_discrete(name = "Predicted", limits = c("Donald Trump","Hillary Clinton")) + 
  scale_x_discrete(name = "Actual", position = "top") +
  #theme(legend.position = " ") +
  theme(text=element_text(size=25,  family="sans")) + 
  ggtitle("Confusion Matrix") +
  theme(plot.title = element_text(size = 25, family="sans", face = "bold"))

confusionMatrix

#ROC Curve
ROC_C <- as.numeric(NB_e1071_Pred)
ROC_C <- ROC_C - 1
ROC_C
probNvb <- ROC_C
ROCNvb <- roc(as.numeric(DF_Test_Labels),probNvb)
plot(ROCNvb, col = "#02babc", family = "sans", cex = 2, main = "Naïve-Bayes ROC Curve")

#Test Accuracy
misClassError <- mean(as.numeric(DF_Test_Labels) != as.numeric(NB_e1071_Pred))
print(paste('Test-set Accuracy =',1-misClassError))

#########################################
## Variable importance and other NB option
###############################################
## The following may only run if you convert from DF to matrix...
## I will not do that here - but FYI...if you have a lot of data
# memory.limit()
# memory.limit(1e+09)
# library(mlbench)
# library(caret)
#############################################################




########################################### Feature importance....

#-----------------------------------------------------------------
##
## OTHER METHODS - there are many!
## https://rpubs.com/maulikpatel/224581
##

#############################################################
##
## OTHER OPTIONS
##
#############################################################

############### This example uses cross validation (CV) AND feature Imp
x <- DF_Train_Student_NL 
y <- DF_Train_Student_Labels
model = train(x,y,'nb')
#,trControl=trainControl(method='cv',number=10))
model$results

Predict <- predict(model,DF_Test_Student_NL )
table(Predict,DF_Test_Student_Labels)
#Plot Variable performance
X <- varImp(model)
plot(X)