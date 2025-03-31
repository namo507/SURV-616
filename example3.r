#Example 3, survival analysis
library(survival)
library(survminer)

example3 <- read.delim("C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2021 Winter/Lectures/09 Survival Analysis/example3.dat", header=F)
colnames(example3)<-c('weeks','group','censored')

#coding for R is 1=event, 0=censored
#I have a variable 'censored' which is 1 then the case is censored.
#Therefore, need to recode it.
example3$event <-abs(example3$censored-1)

surv3 <- survfit(Surv(weeks,event)~ group, data=example3)
surv3
summary(surv3)

plot(surv3, xlab="Weeks", 
     ylab="Survival Probability",
     conf.int=FALSE,lty=2:3)
legend(20, .9, c("6-MP", "Placebo"),lty=2:3) 

ggsurvplot(surv3,data=example3, legend.title = "Treatment",
           legend.labs = c("6-MP", "Placebo"),
           conf.int=TRUE,
           risk.table=TRUE, 
           palette = c("#E7B800", "#2E9FDF"))

#Log-rank test for comparing two groups (treatments)
surv3_comp <- survdiff(Surv(weeks,event)~ group, data=example3)
surv3_comp

