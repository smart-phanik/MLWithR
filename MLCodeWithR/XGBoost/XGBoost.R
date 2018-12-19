# XGBoost
# Problem : Customer Churn


dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[4:14] 

# Encoding the categorical variables as factors and setting the factors as numeric
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))


# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Exited, SplitRatio = 0.80)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

# Fitting XGBoost into  the trianing set
#install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-11]),# removing dependent variable
                      label = training_set$Exited, nrounds = 10)

library(caret)
# 10 differnt folds for the training set
folds = createFolds(training_set$Exited, k = 10) # specifying the dependent variable
# Implementing k fold  alg now
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ] # taking the whole training set but without the test fold
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-11]),# removing dependent variable
                       label = training_set$Exited, nrounds = 10)
  Y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  Y_pred = (Y_pred >= 0.5)
  cm = table(test_fold[, 11], Y_pred)
  accuracy = (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] +cm[1, 2] + cm[2, 1])
  return(accuracy)
} )

accuracy = mean(as.numeric(cv))

