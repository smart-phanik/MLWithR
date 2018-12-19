# Decsion Tree Classification

# Importing dataset
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[, 3:5] # this is for creating some subsets

# Encoding the target feature as factor bcoz we will get argument length erro in computing confusion matrix.
dataset$Purchased = factor(dataset$Purchased , levels = c(0,1))

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


# Fitting  Classifier to the Training Set
#install.packages('rpart')
library(rpart)
classifier = rpart(formula = Purchased ~.,
                   data = training_set)


# Predicting the Test Set Results
#Y_pred = predict(classifier, newdata = test_set[-3]) # this give us the matrix
Y_pred = predict(classifier, newdata = test_set[-3], type = 'class') # this give us the vector
#Y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making confusion Matrix
cm = table(test_set[, 3], Y_pred)


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
#Y_grid = predict(classifier, newdata = grid_set)
Y_grid = predict(classifier, newdata = grid_set, type = 'class')  # this gives the vector
plot(set[, -3],
     main = 'Decsion Tree (Training set)',
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
plot(set[, -3], main = 'Decsion Tree (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))