# K - Means - Clustering

# Importting the mall dataset
dataset <- read.csv("Mall_Customers.csv")
X <- dataset[4:5]

# Using the elbow methodto find the optimal numer of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X, i)$withinss)
plot(1:10, wcss, type = "b", main = paste('Clusters of clients', xlab = 'Number of clusters', ylab = 'WCSS'))
# Optimal k = 5 by the elbow method

# Applying k-means to the mall dataset with k = 5
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

# Visualising the clusters
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of clients'),
         xlab = 'Anual income',
         ylab = 'Spending score')
