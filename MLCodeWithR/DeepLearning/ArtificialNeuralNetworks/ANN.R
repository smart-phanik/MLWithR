# Artificial Nerual Network
# Problem : Customer Churn in Banks , 0 -> customer stays, 1-> customer leaves the bank
# this belongs to a classification problem

# Part 1 : Data Preprocessing
dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[4:14] 

# Encoding the categorical variables as factors and setting the factors as numeric
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                      levels = c('Female', 'Male'),
                                      labels = c(1, 2)))


# Splitting data into Training and Test set
#install.packages('caTools') # if this is present then comment
library(caTools)
set.seed(123) # like putting random_state in python
split = sample.split(dataset$Exited, SplitRatio = 0.80)
training_set = subset(dataset,split == T)
test_set = subset(dataset, split == F)

#Feature Scaling
training_set[-11] = scale(training_set[-11]) 
test_set[-11] = scale(test_set[-11])


# Fitting  the ANN  to the Training Set
install.packages('h2o')
library(h2o)
# Establising a connection
# h2o allows to connceting to new machine to run our deep learning modelss
h2o.init(nthreads = -1) # -1 optimise no of cores in the system


# Create the classifier
# Adding the input layer and the first hidden layer
# Choosing number of layers is based on avg(no of input layers + no of output layers)
# for this eg : No of input layers = 11 (bcoz it contain 11 independent variable)
# output layer = 1  bcoz of 1 dependent variable
# no fo layers = (10+1)/2 = 5.5 , rounding 6
# relu - rectifier funtion for hidden layer
# we need to specify input_dim only for first hidden layer

classifier = h2o.deeplearning(y = 'Exited',
                              training_frame = as.h2o(training_set),# training_frame expects h2o converted frame
                              activation = 'Rectifier',
                              hidden = c(6,6), # each for one hidden layer, we are going to create 2 hidden layers
                              epochs = 100,
                              train_samples_per_iteration = -2) 

# Predicting the Test Set Results

Y_pred = h2o.predict(classifier,
                 newdata = as.h2o(test_set[-11])) # removing dependent variable purchase

# Transforming probablities into 0 and 1 basing on threshold 0.5
Y_pred = ifelse(Y_pred > 0.5, 1, 0) 
      # (or)
#Y_pred = (prob_pred > 0.5) 

Y_pred = as.vector(Y_pred) # Converting h2o object to vector 

# Making confusion Matrix
cm = table(test_set[,11], Y_pred)

accuracy = (1523 + 186)/2000  # 

# disconnection
h2o.shutdown()
