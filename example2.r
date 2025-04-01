#Example 2, using the  data presented on in-class slide 12
(mu=c(67,448,409))


(Sigma <- matrix(c(652,2485,2418,2485,14755,11031,2418,11031,14666), ncol = 3))


#Now creating submatrices
(Sigma_11<-652)

(Sigma_12<-matrix(c(2485,2418),ncol=2))

(Sigma_21<-matrix(c(2485,2418),ncol=1))


(Sigma_22<-matrix(c(14755,11031,11031,14666),ncol=2))

#Calculate the conditional mean on slide 19 in the in-class slides
(mu_2cond1<-mu[2]+Sigma[2,1]/Sigma[1,1]*(30-mu[1]))


#Calculate the conditional variance on slide 20
#Recall that conditional variances don't depend on the value of the mean
#We can calculate the conditional covariance from the elements of Sigma_2cond1
(Sigma_2cond1<-Sigma_22 - Sigma_21 %*% solve(Sigma_11) %*% Sigma_12)

