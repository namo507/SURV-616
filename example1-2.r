#Example 1, survival analysis
library(survival)
library(survminer)

table1<-matrix(c(seq(1:10),5,6,8,18,16,14,7,2,4,9,rep(1,10)),ncol=3,nrow=10,byrow=FALSE)
colnames(table1)<- c("CaseID", "T", "censor")
table1<-as.data.frame(table1)

surv1 <- survfit(Surv(time=T,event=censor)~1, 
                 data=table1)
surv1
summary(surv1)
plot(surv1, xlab="Weeks", 
     ylab="Survival Probability",
     conf.int=TRUE)

ggsurvplot(surv1,data=table1)

