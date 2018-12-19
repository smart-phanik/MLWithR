# Eclat model

# Implemeting algorithm to a store to look for the optimsation of sales...
# like cerals and milk should be in single basket or placing both next to each other

# Importing a dataset
dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
#install.packages('arules')
library(arules)
dataset = read.transactions('Market_Basket_Optimisation.csv', 
                            sep = ',', rm.duplicates = T)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10) #topN gives most highest number

# Training the Eclat on the dataset
# calc of support 3*7/7500 based on assumption = 0.0028 rounding to 0.003
# here 3 is 3 time product has been purchased in day
# 7 is total days in a week and 7500 is total no.of transactions as per dataset
# confidence can be giver with aribitary choice , so we will go with default
#rules = apriori(data= dataset, parameter = list(support = 0.003, confidence = 0.8)) 
# we will divide by 2 bcoz with 0.8 we obeserved 0 rules

rules = eclat(data= dataset, parameter = list(support = 0.004, minlen = 2))

# Visualise
inspect(sort(rules, by = 'support')[1:10])
