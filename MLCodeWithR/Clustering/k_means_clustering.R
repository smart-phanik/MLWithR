#K Means Clustering

# Importing dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Using Elbow method to find optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


# Fitting K_Means to dataset
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

# Visusalising 
#install.packages('cluster')
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste("Cluster of clients"),
         xlab = "Annual Income",
         ylab = " Spending Income")
