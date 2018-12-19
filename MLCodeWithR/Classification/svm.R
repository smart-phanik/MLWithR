# SVM

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


# Creating and Fitting  Classifier to the Training Set
library(e1071)
classifier = svm(formula = Purchased ~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test Set Results

prob_pred = predict(classifier, type = 'response', newdata = test_set[-3]) 

# Making confusion Matrix
cm = table(test_set[, 3], Y_pred)


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
Y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
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
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))