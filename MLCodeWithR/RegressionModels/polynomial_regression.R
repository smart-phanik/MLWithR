# Polynomial Regression

# problem : Identifying whether employee saying correct salary or bluffing

# Importing dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, 2:3] # this is for creating some subsets


# Splitting data into Training and Test set
# we are not splitting bcoz dataset is small
#install.packages('caTools') # if this is present then comment
# library(caTools)
# set.seed(123) # like putting random_state in python
# split = sample.split(dataset$Profit, SplitRatio = 0.8)
# training_set = subset(dataset,split == T)
# test_set = subset(dataset, split == F)

# Feature Scaling
#training_set[,2:3] = scale(training_set[, 2:3]) # We are taking only numeric values keep in mind we use factor to convert String into Numeric
#test_set[,2:3] = scale(test_set[, 2:3])

# Fitting multiple linear regression into training set

#regressor = lm(formula = Profit ~ R.D.Spend + Admnistration + Marketing.Spend + State)
# or another way


# Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ ., 
             data = dataset )

# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2# adding new coloumn with squares of level
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~.,
              data = dataset)

# Visualising the Linear Regression results
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             colour = "red") +
  geom_line(aes(x = dataset$Level , y = predict(lin_reg, newdata = dataset)), 
            colour = "blue") + # for predicting
  ggtitle("Truth or Bluff (Linear Regression)") +
  xlab("Level") +
  ylab("Salary")


# Visualising the Polynomial Regression results
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             colour = "red") +
  geom_line(aes(x = dataset$Level , y = predict(poly_reg, newdata = dataset)), 
            colour = "blue") + # for predicting
  ggtitle("Truth or Bluff (polynomial Regression)") +
  xlab("Level") +
  ylab("Salary")

# Prediciting a new result with Linear Regression
Y_pred = predict(lin_reg, data.frame(Level = 6.5)) # calculating for level 6.5

# Prediciting a new result with Polynomial Regression
Y_pred = predict(poly_reg, data.frame(Level = 6.5,
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))
