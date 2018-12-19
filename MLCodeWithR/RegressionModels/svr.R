# SVR

# Importing dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, 2:3] # this is for creating some subsets


# Fitting the SVR Model to the dataset
install.packages('e1071')
library(e1071)

regressor = svm(formula = Salary ~.,
                data = dataset,
                type = 'eps-regression')


# Prediciting a new result with  Regression
Y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the  SVR Model results
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             colour = "red") +
  geom_line(aes(x = dataset$Level , y = predict(regressor, newdata = dataset)), 
            colour = "blue") + # for predicting
  ggtitle("Truth or Bluff (SVR Model)") +
  xlab("Level") +
  ylab("Salary")


