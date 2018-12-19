# Apriori
# Implemeting algorithm to a store to look for the optimsation of sales...
# like cerals and milk should be in single basket or placing both next to each other

# Data Preprocessing

# Importing dataset
dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
install.packages('arules')
library(arules)
dataset = read.transactions('Market_Basket_Optimisation.csv', 
                            sep = ',', rm.duplicates = T)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10) #topN gives most highest number

# Training the apriori on the dataset
# calc of support 3*7/7500 based on assumption = 0.0028 rounding to 0.003
# here 3 is 3 time product has been purchased in day
# 7 is total days in a week and 7500 is total no.of transactions as per dataset
# confidence can be giver with aribitary choice , so we will go with default
#rules = apriori(data= dataset, parameter = list(support = 0.003, confidence = 0.8)) 
# we will divide by 2 bcoz with 0.8 we obeserved 0 rules

rules = apriori(data= dataset, parameter = list(support = 0.003, confidence = 0.4))

# Visualise
inspect(sort(rules, by = 'lift')[1:10]) # identifying 10 first rules which contain highest lift

# Changing the confidence to be more precise  dividing by 2
rules = apriori(data= dataset, parameter = list(support = 0.003, confidence = 0.2))

# Visualise 10 first rules
inspect(sort(rules, by = 'lift')[1:10]) 

#------ Repeating step by increasing confidence 4*7/7500 = 0.004

rules = apriori(data= dataset, parameter = list(support = 0.004, confidence = 0.2))

# Visualise
inspect(sort(rules, by = 'lift')[1:10])
