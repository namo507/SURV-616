#Demonstration of iterative fitting of logistic regression model.
#Using example from Faraway, p118-120
library(faraway)
data(bliss)
#library(xtable)

#Following creates latex output for slides
#xtable(bliss)

#Start with glm and logistic regression.
#We will emulate this.
logitmod<-glm(cbind(dead,alive)~conc,family=binomial,data=bliss)
summary(logitmod)$coef

y<-bliss$dead/30
mu<-y

#First iteration.
eta<-logit(mu)
z<-eta+(y-mu)/(mu*(1-mu))
w<-30*mu*(1-mu)
linmod<-lm(z~conc,weights=w,bliss)
coef(linmod)

#Loop through several times.
for (i in 1:5){
  eta<-linmod$fit               #predicted values eta from the linear model
  mu<-ilogit(eta)               #Update the predicted mu's (inverse logit function)
  z<-eta+(y-mu)/(mu*(1-mu))     #Here, mu and y differ
  w<-30*mu*(1-mu)               #New weights
  linmod<-lm(z~bliss$conc,weights=w)
  cat(i,coef(linmod),"\n")
}

summary(linmod)$coef       #Coefficients correct, StdErr's wrong
summary(logitmod)$coef

#Let's look at convergence and convergence criteria
str(logitmod)
logitmod$iter
logitmod$control

help(glm)
help(glm.control)

#Still need to get correct variance estimates. Two methods:
#1. Create a variance estimate use var(beta)=(X'WX)^{-1}phi, where phi=1
xm<-model.matrix(linmod)
wm<-diag(w)
sqrt(diag(solve(t(xm) %*% wm %*% xm)))

#2. In the linear model, var(beta)=(X'WX)^{-1}*sigma^2, so just divide by sigma^2
summary(linmod)$coef[,2]/summary(linmod)$sigma # second column of coef is the StdErr

#Now test model fit
#Print out the residual deviance
summary(logitmod)
#Compare to a chi-sq null distribution
1-pchisq(deviance(logitmod),df.residual(logitmod))

#Now compare the null model to the model with the single predictor ''conc''
anova(logitmod,test="Chi")

#Now look at a model nested within another model.
logitmod2<-glm(cbind(dead,alive)~conc+ I(conc^2),family=binomial,bliss)
anova(logitmod,logitmod2,test="Chi")

#Look at the Pearson residuals
residuals(logitmod,"pearson")
sum((residuals(logitmod,"pearson"))^2)

#Look at the deviance residuals
residuals(logitmod) #these are the default residual
sum((residuals(logitmod))^2)

#Look at the working residuals
residuals(linmod)
residuals(logitmod,"working")

#Get the difference in the coefficients when each case is dropped
influence(logitmod)$coef

#Verify the coefficients by deleting the first row
testdat<-bliss[2:5,]
glm(cbind(dead,alive)~conc,family=binomial,data=testdat)

#Overdispersion. Specify scale parameter.
#Calculate Pearson X^2 divided by (n-p)
d<-sum(logitmod$weights * logitmod$residuals^2)/logitmod$df.residual
summary(logitmod,dispersion=d,correlation=TRUE,symbolic.cor=TRUE)
