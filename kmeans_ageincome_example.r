#K-means using mall customer data from kaggle.com
library(ggplot2)
library(tidyverse)

#Read the data, clean up variable names
Mall_Customers <- read.csv("C:/Users/jameswag/Dropbox (University of Michigan)/Teaching/Stat Methods II/2023 Winter/Lectures/09 Classification/Mall_Customers.csv", stringsAsFactors=TRUE)
names(Mall_Customers)<-c("CustomerID","Gender","Age","Annual.Income","Spending.Score")

#Review the data, subset to the 2 variables of interest, standardize these variables.
head(Mall_Customers,n=5)
Mall_Cust2<-Mall_Customers[,c(3:4)]
Mall_Cust2_std<-scale(Mall_Cust2)
summary(Mall_Cust2_std)

#Ask for 4 groups
km.sol<-kmeans(Mall_Cust2_std, 4, nstart=25)
km.sol

#Add the cluster variable back to the data
cluster <- as.data.frame(as.factor(km.sol$cluster))
Mall_Cust3<-cbind(Mall_Cust2_std,cluster)
head(Mall_Cust3)
names(Mall_Cust3)<-c("Age","Annual.Income","Cluster")

#Make a figure
ggplot() +  geom_point(data = Mall_Cust3, 
             mapping = aes(x = Age, 
                           y = Annual.Income, 
                           colour = Cluster)) +
            geom_point(mapping = aes_string(x = km.sol$centers[, "Age"], 
                                            y = km.sol$centers[, "Annual.Income"]),
                                            color = "red", size = 4)

#Same problem, but look at interim solutions
my.centers<- matrix(c(3.0,-1.5,2.0,-0.5,0.0,-2.0,-1.0,-1.0,2.0,2.0,0.0,1.0), ncol=2)

Kmeans_Iteration <- function(iter=1) {
  km.sol1<-kmeans(Mall_Cust2_std, my.centers, iter.max=iter)
  km.sol1
  cluster <- as.data.frame(as.factor(km.sol1$cluster))
  Mall_Cust4<-cbind(Mall_Cust2_std,cluster)
  head(Mall_Cust4)
  names(Mall_Cust4)<-c("Age","Annual.Income","Cluster")
  titl<-paste("Iteration ", iter)
  ggplot() +  geom_point(data = Mall_Cust4, 
                         mapping = aes(x = Age, 
                                       y = Annual.Income, 
                                       colour = Cluster)) +
    geom_point(mapping = aes_string(x = km.sol1$centers[, "Age"], 
                                    y = km.sol1$centers[, "Annual.Income"]),
               color = "red", size = 4) + ggtitle(titl) 
}
Kmeans_Iteration(iter=1)
Kmeans_Iteration(iter=2)
Kmeans_Iteration(iter=3)


#Using unstandardized data
#Ask for 4 groups
km.sol.unstd<-kmeans(Mall_Cust2, 4, nstart=25)
km.sol.unstd

#Add the cluster variable back to the data
cluster <- as.data.frame(as.factor(km.sol.unstd$cluster))
Mall_Cust5<-cbind(Mall_Cust2,cluster)
head(Mall_Cust5)
names(Mall_Cust5)<-c("Age","Annual.Income","Cluster")

#Make a figure
ggplot() +  geom_point(data = Mall_Cust5, 
                       mapping = aes(x = Age, 
                                     y = Annual.Income, 
                                     colour = Cluster)) +
  geom_point(mapping = aes_string(x = km.sol.unstd$centers[, "Age"], 
                                  y = km.sol.unstd$centers[, "Annual.Income"]),
             color = "red", size = 4)

table(Mall_Cust5$Cluster,Mall_Cust3$Cluster)#Many cases end up with different group

#Make a plot of withinss by K using a function from:
#https://uc-r.github.io/kmeans_clustering
wss <- function(k) {
  kmeans(Mall_Cust2_std, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")