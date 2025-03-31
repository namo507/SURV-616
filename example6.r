library(DescTools)
#Possible to use DescTools when you have the full dataset.

#Example 6 MVN, see pages 19-20 in old class notes

#Set up the data, X-bar and Y-bar from slide 44 in-class slides
#S-poolked is on slide 45. n's on our slide 43
Xbar<-c(72.412776,484.69287,425.40541)
Spooled<-matrix(c(626.5464,2321.339,2347.649,2321.339,13696.948,10571.159,2347.6492,10571.159,14474.518), ncol = 3)
Ybar<-c(62.186275,418.80392,396.45098)
SpInv<-solve(Spooled)
n<-917 #total sample size
n1<-407 #sample size for men
n2<-510 #sample size for women

#Hotelling's T^2, following slide 48
#note that [1/n_1 + 1/n_2]^-1 = (n_1*n_2)/(n_1+n_2)
(T2=n1*n2*t(Xbar-Ybar) %*% SpInv %*% (Xbar-Ybar)/(n1+n2))


#Corrected the degrees of freedom
fval=qf(0.05,3,913,lower.tail=FALSE)
915*3/913*fval


