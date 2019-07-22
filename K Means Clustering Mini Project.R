install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)
data(wine, package="rattle.data")
head(wine)
#
# Exercise 1: Remove the first column from the data and scale
New <- scale(wine[-1])  
head(New)
# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)
# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.
library(NbClust)
set.seed(1234)
nc <- NbClust(New, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
#
# Exercise 3: How many clusters does this method suggest? 3
#
# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
fit.km <- kmeans(New, 3, nstart=26)
#
# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine[,1],fit.km$cluster)
#
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
clusplot(New, fit.km$cluster, main='2D',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# This appears to be a good clustering because the majority of data points are in their clusters. 
