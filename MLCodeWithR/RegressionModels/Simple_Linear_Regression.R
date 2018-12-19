# Simple Linear Regression


# Importing dataset
dataset = read.csv("Salary_Data.csv")
#dataset = dataset[, 2:3] # this is for creating some subsets


# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
#training_set[,2:3] = scale(training_set[, 2:3]) # We are taking only numeric values keep in mind we use factor to convert String into Numeric
#test_set[,2:3] = scale(test_set[, 2:3])

# Fitting Simple Linear Regression into Training Set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# Predicting the Test set results

Y_pred = predict(regressor, newdata = test_set)

# Visualising the Training Set Results
#install.packages(ggplot2)
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle("Salary vs Experience(Training Set)") +
  xlab('Years of Experience') +
  ylab('Salary')

# Visualising the test set

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle("Salary vs Experience(Test Set)") +
  xlab('Years of Experience') +
  ylab('Salary')



