# Random Foresh Regression

# Importing dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, 2:3] # this is for creating some subsets


# Fitting the Random Forest Regression Model to the dataset
#install.packages('randomForest')
#library(randomForest)
set.seed(123)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 100) # increase ntree to get better prediction results
# Prediciting a new  result
Y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the  Random Forest Regression Model results with hight resolution and smooth curve

X_grid = seq(min(dataset$Level), max(dataset$Level), 0.01) # 1 to 10 getting incr by 0.1

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             colour = "red") +
  geom_line(aes(x= X_grid , y = predict(regressor, newdata = data.frame(Level = X_grid))), 
            colour = "blue") + # for predicting
  ggtitle("Truth or Bluff (Regression Model)") +
  xlab("Level") +
  ylab("Salary")