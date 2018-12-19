# Hierarical Clustering 

# Importing dataset
dataset = read.csv('Mall_Customers.csv')
X = dataset[4:5]

# Using Dendrogram to find optimal number of clusters

dendrogram = hclust(dist(X, method = 'euclidean'),
                    method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distancd')

# Fitting HC into the dataset

hc = hclust(dist(X, method = 'euclidean'),
                    method = 'ward.D')
Y_hc = cutree(hc, 5,) # look the parameters for the method

# Visualise the clusters using clusplot

library(cluster)
clusplot(X,
         Y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste("Cluster of clients"),
         xlab = "Annual Income",
         ylab = " Spending Income")


