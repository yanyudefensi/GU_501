

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, naivebayes, tidymodels)

MyPath = "/Users/junjiexie/Documents/gu校内生活/additionalData/"
RecordDatasetName = "pres16results.csv"


setwd(MyPath)
RecordDF_all <- read.csv(RecordDatasetName, stringsAsFactors = TRUE)
head(RecordDF_all)

RecordDF_A_all <- subset.data.frame(RecordDF_all, select = c("pct_report", "votes", "total_votes", "pct", "lead"))
nrow(RecordDF_A_all)
RecordDF_A = RecordDF_A_all[sample(nrow(RecordDF_A_all), 5000), ]
head(RecordDF_A)

glimpse(RecordDF_A)

set.seed(42)
data_split <- initial_split(RecordDF_A, prop=0.8)
train <- training(data_split)
test <- testing(data_split)

nb_clf <- naive_bayes(lead ~., data = train, usekernel = T)
train_pred <- predict(nb_clf, train)
test_pred <- predict(nb_clf, test)

table(Reality=test$lead, Predict=test_pred)