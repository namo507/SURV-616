#K-means using mall customer data from kaggle.com
library(ggplot2)
library(tidyverse)

#Read the data, clean up variable names
beer <- read.csv("Beer.csv")

#Review the data, subset to the 2 variables of interest, standardize these variables.
head(beer,n=5)
beer2<-beer[,c(2:5)]
beer2_std<-scale(beer2)
summary(beer2_std)

#Ask for 4 groups
km.sol<-kmeans(beer2_std, 4, nstart=25)
km.sol

#Add the cluster variable back to the data
cluster <- as.data.frame(as.factor(km.sol$cluster))
beer3<-cbind(beer,cluster)
head(beer3)

#Make a figure
ggplot() +  geom_point(data = beer3, 
             mapping = aes(x = Price, 
                           y = Calories, 
                           colour = km.sol$cluster))


#Using unstandardized data
#Ask for 4 groups
km.sol.unstd<-kmeans(beer2, 4, nstart=25)
km.sol.unstd

#Add the cluster variable back to the data
cluster <- as.data.frame(as.factor(km.sol.unstd$cluster))
beer5<-cbind(beer2,cluster)
head(beer5)

#Make a figure
ggplot() +  geom_point(data = beer5, 
                       mapping = aes(x = Price, 
                                     y = Calories, 
                                     colour = as.factor(km.sol.unstd$cluster)))


