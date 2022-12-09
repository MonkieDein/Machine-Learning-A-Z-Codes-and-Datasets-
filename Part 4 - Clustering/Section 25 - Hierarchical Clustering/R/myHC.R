# Unsupervised: No Dependent variable to predict
#
# Hirarchical Clustering
# Each points is a cluster by itself. Repeatedly Combine closest cluster 
# until there is only 1 cluster. 
# Can also stop after some dissimilarity-threshold.
# Closest <- Minimize within Cluster Variance 'ward.D'
#
setwd("~/Desktop/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 4 - Clustering/Section 25 - Hierarchical Clustering/R")
df = read.csv("mall.csv")
X = df[c("Annual.Income..k..","Spending.Score..1.100.")]

# Using Dendogram to find optimal number of clusters
dendogram = hclust(dist(X,method = 'euclidean'),method = 'ward.D')
plot(dendogram, main = paste('Dendogram'),xlab = 'Customers',ylab='Euclidean Distance')

# Fitting hierarchical clustering to the mall dataset
hc = hclust(dist(X,method = 'euclidean'),method = 'ward.D')
y_hc = cutree(hc,5)

# Visualization Clustering
library(cluster)
clusplot(X,y_hc,lines=0,shade=T,color=T,labels=2,
         plotchar=F,span=T,main="Clusters of clients",xlab = "Annual Income",
         ylab="Spending Score")
