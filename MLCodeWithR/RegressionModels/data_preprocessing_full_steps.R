# Data Preprocessing

# Importing dataset
dataset = read.csv("Data.csv")

# Taking care of Missing values
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age,FUN = function(x) mean(x,na.rm = T)),
                     dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(y) mean(y,na.rm = T)),
                        dataset$Salary)

# Encoding the categorical variables Country,Purchase
dataset$Country = factor(dataset$Country,
                         levels = c('France','Spain','Germany'),
                         labels = c(1,2,3))

dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No','Yes'),
                           labels = c(0,1))

# Splitting data into Training and Test set
install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
training_set[,2:3] = scale(training_set[, 2:3]) # We are taking only numeric values keep in mind we use factor to convert String into Numeric
test_set[,2:3] = scale(test_set[, 2:3])
