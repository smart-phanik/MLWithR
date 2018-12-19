# Grid Search

# Importing dataset
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[, 3:5] # this is for creating some subsets


# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

#Feature Scaling
training_set[,1:2] = scale(training_set[, 1:2]) 
test_set[,1:2] = scale(test_set[, 1:2])


# Fitting  Kernel to the Training Set
"""
library(e1071)
classifier = svm(formula = Purchased ~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial') # this is Guassian Kernel


# Predicting the Test Set Results

Y_pred = predict(classifier, newdata = test_set[-3])"""


# Making confusion Matrix
cm = table(test_set[, 3], Y_pred)

# Applying K-Fold-Corss Validation
#install.packages('caret')
library(caret)
# 10 differnt folds for the training set
folds = createFolds(training_set$Purchased, k = 10) # specifying the dependent variable
# Implementing k fold  alg now
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ] # taking the whole training set but without the test fold
  test_fold = training_set[x, ]
  classifier = svm(formula = Purchased ~.,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'radial')
  Y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], Y_pred)
  accuracy = (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] +cm[1, 2] + cm[2, 1])
  return(accuracy)
} )

accuracy = mean(as.numeric(cv))


# Applying Grid Search , to find the best model and parameters
# for parameter tuning we should use caret package

library(caret)
classifier = train(form = Purchased ~., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
Y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
Y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))