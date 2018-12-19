# Data Preprocessing

# Please look into data_preprocessing_full_steps.R 
#file to get all the stops for data preprocessing.
#The code removed here to make this code reusable

# Importing dataset
dataset = read.csv("Data.csv")
#dataset = dataset[, 2:3] # this is for creating some subsets


# Splitting data into Training and Test set
install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

# Feature Scaling
#training_set[,2:3] = scale(training_set[, 2:3]) # We are taking only numeric values keep in mind we use factor to convert String into Numeric
#test_set[,2:3] = scale(test_set[, 2:3])
