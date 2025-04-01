mydata <- "D:\\Teaching\\Survmeth686\\Winter 2024\\Lectures\\12 Multivariate Normal\\"

#Example 3 comparing a mean to a hypothetical mean
library(DescTools)

#Data from slide 32, in-class slides
n=917
xbar=matrix(c(448,409),2)
mu0=matrix(c(450,400),2)
s=c(14755,11031,11031,14666)
s=matrix(s,2)
sinv=solve(s)

round(s%*%sinv,1)

#Calculating T2 as on slide 39
T2=n*t(xbar-mu0) %*% sinv %*% (xbar-mu0)
T2

#Check the reference distribution and do calculations on bottom of slide 41
fval=qf(0.05,2,915,lower.tail=FALSE)
916*2/915*fval


#Desctools example (slide 40)
nutrient <- read.table(paste(mydata,"nutrient.txt",sep=""), quote="\"", comment.char="")
names(nutrient)<-c('id',"calcium","iron","protein","vita","vitc")
nutrient<-nutrient[,-1]
summary(nutrient)

mu0<-matrix(c(1000,15,60,800,75))#The null hypothesis
T2.Desc<-HotellingsT2Test(nutrient,mu=mu0)
T2.Desc
T2.Desc$statistic