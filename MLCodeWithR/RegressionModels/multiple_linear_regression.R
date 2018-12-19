# Mutiple Lienar Regression

# Data Preprocessing

# Please look into data_preprocessing_full_steps.R 
#file to get all the stops for data preprocessing.
#The code removed here to make this code reusable

# Importing dataset
dataset = read.csv("50_Startups.csv")
#dataset = dataset[, 2:3] # this is for creating some subsets

# Encoding the categorical variables Country,Purchase
dataset$State = factor(dataset$State,
                         levels = c('New York','California','Florida'),
                         labels = c(1,2,3))

# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
#training_set[,2:3] = scale(training_set[, 2:3]) # We are taking only numeric values keep in mind we use factor to convert String into Numeric
#test_set[,2:3] = scale(test_set[, 2:3])

# Fitting multiple linear regression into training set

#regressor = lm(formula = Profit ~ R.D.Spend + Admnistration + Marketing.Spend + State)
# or another way

regressor = lm(formula = Profit ~ ., data = training_set)

# Predicting the test set results
Y_pred = predict(regressor, newdata = test_set)

# Building optimal model with Backward Elimination
# we need to take independent variable which have P or Significance level< 0.5

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data =  dataset)
summary(regressor)

# Remove State because it has more P value
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data =  dataset)
summary(regressor)

# Remove Administration because it has more P value
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data =  dataset)
summary(regressor)

# Remove Marketing.Spend because it has more P value
regressor = lm(formula = Profit ~ R.D.Spend , data =  dataset)
summary(regressor)

