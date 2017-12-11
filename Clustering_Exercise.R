# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine2<-scale(wine[-1])
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

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

wssplot(wine2)


# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

## This plot shows 15 clusters' situations. There is a bend as number of clusters is 3. 
## It suggest the number of clusters we should use in kmeans function is 3.
## wssplot function compute average (Euclidean) distance between all cluster members with clusters' number from 1 to 15.
## We can see from the plot, before number of clusters with 3, the average distance drop with a big range,
## but after that, the distance drop with much smaller range. 


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

## Through barplot with NbClust function, we can see when number of clusters if three, 
## it has been chosen by more than 14 criteria which is obviously higher than other numbers.
## Then this methos suggest three clusters.


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
set.seed(1234)
fit.km <- kmeans(wine2,centers=3,nstart=15)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine$Type,fit.km$cluster)

## The table function shows all 59 observations with wine$Type=1 matched with cluster 1;
## 65 observations with wine$Type=2 matched with cluster 2, 3 matched with cluster 1, 3 matched with cluster 3;
## ALl 48 observations with wine$Type=3 matched with cluster 3;
## The overall accuracy is (59+65+48)/178=0.9663;
## It's a good clustering.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(wine2,fit.km$cluster,color = TRUE,col.p = fit.km$cluster,labels = 2)

## On this plot, there are three clusters of points which color are red,green and black. 
## Each cluster has minimun variance between cluster memberships.
## Distancess from green cluster to red cluster and to black cluster are as big as possible.
## Red cluster overlap a bit with black cluster.
## This is a good clustering.
