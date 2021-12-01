#######################################


## LIBRARIES
library(rpart)   ## FOR Decision Trees
library(rattle)  ## FOR Decision Tree Vis
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(network)
library(ggplot2)
##If you install from the source....
#Sys.setenv(NOAWT=TRUE)
## ONCE: install.packages("wordcloud")
library(wordcloud)
## ONCE: install.packages("tm")

library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
#library(SnowballC)

library(proxy)
## ONCE: if needed:  install.packages("stringr")
library(stringr)
## ONCE: install.packages("textmineR")
library(textmineR)
library(igraph)
library(caret)
#library(lsa)


###############
## Read in the dataset you want to work with....
#################################

# MyPath="/Users/junjiexie/Documents/gu校内生活/additionalData/"
# #StudentDataName="SummerStudentDataset_Record_Mixed_Labeled.csv"
# RecordDatasetName="Labeled_ThreeClasses_Mammals_Fish_Reptiles3.csv"

MyPath = "/Users/junjiexie/Documents/gu校内生活/additionalData/"
RecordDatasetName = "pres16results.csv"


setwd(MyPath)
RecordDF_all <- read.csv(RecordDatasetName, stringsAsFactors = TRUE)
head(RecordDF_all)

RecordDF_A <- subset.data.frame(RecordDF_all, select = c("cand", "st", "pct_report", "votes", "total_votes", "pct", "lead"))
nrow(RecordDF_A)
RecordDF_A = RecordDF_A[sample(nrow(df), 5000), ]
# DecordDF_A <- subset(RecordDF_all, select = -c('county', 'fips'))


##########################################
##
##  Let's start with the record dataset
##
##  We need to split it into a TRAINING
##  and a TESTING set  - AND -
##  we will remove the label and save it.
##  Finally, we will remove and save the
##  Name of the animal.
#########################################

## While we do this - let's check data types

str(RecordDF_A)
#RecordDF_A$label<-as.factor(RecordDF_A$label)

## We MUST convert the label (called label) into type FACTOR!
## If you do not do this, your modeling will not work as well
## (or at all)
## I did this above using stringsAsFactors=TRUE


#####################
## Our data is already clean and it is MIXED
## data. I will not normalize it.
######
## What is mixed data? It is data made up of many data types
##
#####################################################

## However, let's explore just a little bit to look for
## BALANCE in the variables AND in the label
##
##  !!!!!! We will also need to remove the "name" column
##   Why??

########################################################

## Simple tables

apply(RecordDF_A, 2, table)  # 2 means columns

## NOTE: Our data and label are balanced pretty well.

## Think about what you see. Are there columns to remove
## from the data?
##

## Here is a fancy method to use a function to
## create a bar graph for each variable.

## Define the function on any dataframe input x
GoPlot <- function(x) {

  G <- ggplot(data = RecordDF_A, aes(.data[[x]], y = "")) +
    geom_bar(stat = "identity", aes(fill = .data[[x]]))

  return(G)
}

## Use the function in lappy
lapply(names(RecordDF_A), function(x) GoPlot(x))


##################################################
##
##  Next - let's look at the DF and remove
## columns we do not want to use in our model
##
###################################################

#RecordDF_A

## We MUST remove the name column or the model will
## not be good. Think about why.
#
# (AnimalName <- RecordDF_A$name)
# (RecordDF_A <- RecordDF_A[-c('country', 'fips')])

############################################
##
## Next - split into TRAIN and TEST data
##
## !!!! Sampling Matters !!!
##
## In our case, we will use random sampling without
## replacement.
##
## Why without replacement?
##
## !!!! IMPORTANT - always clean, prepare, etc. BEFORE
## splitting data into train and test. NEVER after.
##
######################################################
(DataSize = nrow(RecordDF_A)) ## how many rows?
(TrainingSet_Size <- floor(DataSize * (3 / 4))) ## Size for training set
(TestSet_Size <- DataSize - TrainingSet_Size) ## Size for testing set

## Random sample WITHOUT replacement (why?)
## set a seed if you want it to be the same each time you
## run the code. The number (like 1234) does not matter
set.seed(1234)

## This is the sample of row numbers
(MyTrainSample <- sample(nrow(RecordDF_A),
                         TrainingSet_Size, replace = FALSE))

## Use the sample of row numbers to grab those rows only from
## the dataframe....
(MyTrainingSET <- RecordDF_A[MyTrainSample,])
table(MyTrainingSET$lead)

## Use the NOT those row numbers (called -) to get the
## other row numbers not in the training to use to create
## the test set.

## Training and Testing datasets MUST be disjoint. Why?
(MyTestSET <- RecordDF_A[-MyTrainSample,])
table(MyTestSET$lead)

##Make sure your Training and Testing datasets are BALANCED

###########
## NEXT -
## REMOVE THE LABELS from the test set!!! - and keep them
################################################

(TestKnownLabels <- MyTestSET$lead)
(MyTestSET <- MyTestSET[, -which(names(MyTestSET) %in% c("lead"))])


###################################################
##
##     Decision Trees
##
##      First - train the model with your training data
##
##      Second - test the model - get predictions - compare
##               to the known labels you have.
###########################################################
MyTrainingSET
str(MyTrainingSET)

## This code uses rpart to create decision tree
## Here, the ~ .  means to train using all data variables
## The MyTrainingSET#label tells it what the label is called
## In this dataset, the label is called "label".

DT <- rpart(MyTrainingSET$lead ~ ., data = MyTrainingSET, method = "class")
summary(DT)

## Let's make another tree...here we will use cp
DT2 <- rpart(MyTrainingSET$lead ~ ., data = MyTrainingSET, cp = .27, method = "class")
## The small cp the larger the tree if cp is too small you have overfitting
summary(DT2)

plotcp(DT) ## This is the cp plot

## Let's make a third tree - here we use cp = 0 and
## "information" instead of the default which is GINI
DT3 <- rpart(MyTrainingSET$lead ~ .,
             data = MyTrainingSET, cp = 0, method = "class",
             parms = list(split = "information"), minsplit = 2)
## The small cp the larger the tree if cp is too small you have overfitting
summary(DT3)


## Let's make a 4th tree - but here, we will only use SOME
## of the variables in the dataset to train the model
DT4 <- rpart(MyTrainingSET$lead ~ pct_report + votes + total_votes,
             data = MyTrainingSET, cp = 0, method = "class",
             parms = list(split = "information"), minsplit = 2)
## The small cp the larger the tree if cp is too small you have overfitting
summary(DT4)

#################################################################
## Extra notes about the output/summary
## - Root Node Error x  - X Error  - is the cross-validated error rate,
## which is a more objective measure of predictive accuracy
##  - Root Node Error x  - Rel Error -  is the resubstitution
## error rate (the error rate computed on the training sample).

## Variable Importance: The values are calculate by summing up all the
## improvement measures that each variable contributes
## RE: the sum of the goodness of split measures for each split
## for which it was the primary variable

## in Summary, the variable importance sums to 100

## NOTE: variable.importance is a named numeric vector giving
## the importance of each variable. (Only present
## if there are any splits.)
## When printed by summary.rpart these are rescaled to
## add to 100.
###########################################################
DT3$variable.importance  ## before re-eval to add to 100

############################################################
##
## Predict the Testset using all 4 trees -
## Let's see what we get.
## We will build a tree and a confusion matrix for all 4
##
###############################################################
##
## DT---------------------------------
(DT_Prediction = predict(DT, MyTestSET, type = "class"))
## Confusion Matrix
table(DT_Prediction, TestKnownLabels) ## one way to make a confu mat
## VIS..................
fancyRpartPlot(DT)

## DT2-----------------------------
### Example two with cp - a lower cp value is a bigger tree
(DT_Prediction2 = predict(DT2, MyTestSET, type = "class"))
## ANother way to make a confusion matrix
caret::confusionMatrix(DT_Prediction2, TestKnownLabels, positive = "true")
fancyRpartPlot(DT2)
## Example three with information gain and lower cp

##DT3---------------------------------------------------------
(DT_Prediction3 = predict(DT3, MyTestSET, type = "class"))
confusionMatrix(DT_Prediction3, TestKnownLabels, positive = "true")
rattle::fancyRpartPlot(DT3, main = "Decision Tree", cex = .5)


##DT4---------------------------------------------------------
(DT_Prediction4 = predict(DT4, MyTestSET, type = "class"))
confusionMatrix(DT_Prediction4, TestKnownLabels, positive = "true")
rattle::fancyRpartPlot(DT4, main = "Decision Tree", cex = .5)

