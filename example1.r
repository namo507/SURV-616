library(plyr)
library(ggplot2)
library(doBy)
library(arm)

orings <- read.table("D:\\Teaching\\Survmeth686\\Winter 2024\\Lectures\\04 Logistic\\spacshu.dat", quote="\"")
names(orings)<-c("temp","damage")

orings
table(orings)


logitmod<-glm(damage ~temp, family=binomial, orings)
summary(logitmod)

deviance(logitmod)
df.residual(logitmod)

#Week 5
#calculate the deviance residuals
orings$p_hat<-predict(logitmod,orings,type="response")
orings$e<-orings$damage-orings$p_hat
orings$d <- sign(orings$e)*sqrt(-2*(orings$damage*log(orings$p_hat) + (1 - orings$damage)*log(1 - orings$p_hat)))
# or 
orings$d.alt <- residuals(logitmod) 
head(orings)

sum(orings$d^2)
deviance(logitmod)

#Week 5
#ROC curve.
library(ROCR)
library(AUC)
orings$m1.yhat <- predict(logitmod, orings, type = "response")
orings$m1.yhat

m1.scores <- prediction(orings$m1.yhat, orings$damage)
auc(roc(m1.scores@predictions[[1]],m1.scores@labels[[1]]))

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")



###############################
#Now create some of the plots from the slides
plot(logitmod$linear.predictors~orings$temp,xlab='Temperature',ylab='Log Odds')


#example1.pdf and example1d.pdf
temp.mean<-ddply(orings, c("temp"), summarize,  damage=mean(damage))

ggplot(temp.mean, aes(x=temp, y=damage)) +
  geom_point(size=6,shape=19)  + ylim(0,1) + xlim(15,85) +
  theme(axis.title.x = element_text(face="bold", size=20),
          axis.text.x  = element_text(size=16)) +
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16)) +
  xlab("Temperature") + ylab("Probability of Damage")
ggsave(file="C:/Users/james/Dropbox/Teaching/2016 Winter/Lectures/05 Logistic/example1.png") 

plot(temp.mean$damage~temp.mean$temp,orings,xlim=c(15,85),ylim=c(0,1),xlab="Temperature",ylab="Probability of Damage")
x<-seq(15,85,1)
logit.x<-1/(1+exp(-(5.08498-0.11560*x)))
lines(x,logit.x)

#create a plot of residuals from a linear model
linmod<-lm(data=orings,damage~temp)
oring.resid<-resid(linmod)
plot(oring.resid~orings$temp)#this pot is used in the slides as linmod_res.png

#create an empirical logit plot. 
orings2<-orings
orings2$temp.grp[orings2$temp< 60] <- 55
orings2$temp.grp[orings2$temp >= 60 & orings2$temp <65 ] <- 60
orings2$temp.grp[orings2$temp >= 65 & orings2$temp <70 ] <- 65
orings2$temp.grp[orings2$temp >= 70 & orings2$temp <75 ] <- 70
orings2$temp.grp[orings2$temp>=75] <- 75

orings3<-summaryBy(damage ~ temp.grp, data = orings2,
          FUN = function(x) { c(sum = sum(x), cnt=length(x)) } )
orings3$p<-orings3$damage.sum/(orings3$damage.cnt)
orings3$emp.logit<-log(orings3$p/(1-orings3$p))
orings3<-orings3[-3,]
plot(orings3$temp.grp,orings3$emp.logit)

#emp_logit.png
ggplot(orings3, aes(x=temp.grp, y=emp.logit)) +
  geom_point(shape=16,size=5) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  theme(axis.title.x = element_text( size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text( size=20),
        axis.text.y  = element_text(size=16))
		
#A function that will create bins and an empirical logit plot
empLogitPlot <- function(x, y, nclass = floor(sqrt(length(x)))) {
  require(arm)
  require(ggplot2)
  
  logit <- function (x, eps = 0.05) log((eps + x)/(1 - x + eps))
  
  binned.df <- as.data.frame(binned.resids(x = x, y = y, nclass = nclass)[[1]])
  
  p <- qplot(x = xbar, y = logit(ybar), data = binned.df, geom = c("point", "smooth"), method = "lm", se = FALSE) + 
    ylim(min(logit(binned.df$ybar)), max(logit(binned.df$ybar)))
  return(p)
}

#Binned residual plot
logitmod<-glm(damage ~temp, family=binomial, orings)

orings$phat<-predict(logitmod,type="response")
orings$resid<-resid(logitmod,type="response")

binnedplot(orings$phat,orings$resid)
