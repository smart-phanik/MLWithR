# Decision Tree in R

# Importing dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, 2:3] # this is for creating some subsets


# Fitting the Decison Tree Regression Model to the dataset
install.packages('rpart')
library(rpart)

# Create a Regressor
#regressor = rpart(formula = Salary ~., data = dataset) # check this with graph

regressor = rpart(formula = Salary ~.,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# Prediciting a new 
Y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the  Decision Tree Regression Model results
# ggplot() +
#   geom_point(aes(x = dataset$Level , y = dataset$Salary), 
#              colour = "red") +
#   geom_line(aes(x = dataset$Level , y = predict(regressor, newdata = dataset)), 
#             colour = "blue") + # for predicting
#   ggtitle("Truth or Bluff (Regression Model)") +
#   xlab("Level") +
#   ylab("Salary")


# Visualising the  Decision Regression Model results with hight resolution and smooth curve

X_grid = seq(min(dataset$Level), max(dataset$Level), 0.1) # 1 to 10 getting incr by 0.1

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             colour = "red") +
  geom_line(aes(x = X_grid, y = predict(regressor, newdata = data.frame(Level = X_grid))), 
            colour = "blue") + # for predicting
  ggtitle("Truth or Bluff (Decision Tree Regression Model)") +
  xlab("Level") +
  ylab("Salary")
