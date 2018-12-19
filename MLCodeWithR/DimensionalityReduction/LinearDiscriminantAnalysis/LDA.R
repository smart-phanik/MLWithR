# LDA : considers the classes of dependent variable , which make this model supervised

dataset = read.csv("Wine.csv")



# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)
training_set = as.data.frame(predict(lda, training_set))# as.data.frame used to convert matrix into dataframe
training_set = training_set[c(5, 6, 1)] # re arraging the columns
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(5, 6, 1)]
          



# Fitting SVM Model to the Training Set
install.packages('e1071')
library(e1071)

classifier = svm(formula = class ~., # dependent variable becomes class bcoz of lda theroem
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test Set Results

Y_pred = predict(classifier,  newdata = test_set[-3]) 

# Making confusion Matrix
cm = table(test_set[, 3], Y_pred)


# Visualising the Training set results
# Here we have 3 classes so we have to change according to it to represent a color
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2') # this naming convention  important
Y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
# adding the 3rd color and aslo applying trick to satisfy ifelse conditions
points(grid_set, pch = '.', col = ifelse(Y_grid == 2, 'deepskyblue', ifelse(Y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))


# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
Y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(Y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(Y_grid == 2, 'deepskyblue', ifelse(Y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
