# Unsupervised: No Dependent variable to predict
#
# K-Means clustering 
# Clustering is a unsupervised method which helps decision maker to 
# group similar datas. decide K total cluster. Randomize Algorithm.
#
# (1) Assign point closer to a centroid, (2) calculate new centroid
# Repeat till converge.
# Use Elbow method for WCSS (Within Cluster Sum of Square).
#
# K-mean algorithm is not deterministic, dependent on the initialization.
# K-mean++ assign higher probability for point that is further.
# Still non-deterministic but generally perform well.
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 4 - Clustering/Section 24 - K-Means Clustering/R")

df = read.csv("mall.csv")
X = df[c("Annual.Income..k..","Spending.Score..1.100.")]

# Using elbow method to find the optinal number f clusters
set.seed(7)
wcss = vector()
for(i in 1:10){
  wcss[i] = sum(kmeans(X,i)$withinss)
}
plot(1:10, wcss,type="b",main = 'Elbow methods for WCSS',
     xlab = "Number of Clusters", ylab="WCSS")

# Apply K-means to the mall dataset
set.seed(29)
kmu = kmeans(X,5,iter.max = 300,nstart=10)

# Visualization Clustering
library(cluster)
clusplot(X,kmu$cluster,lines=0,shade=T,color=T,labels=2,
         plotchar=F,span=T,main="Clusters of clients",xlab = "Annual Income",
         ylab="Spending Score")

