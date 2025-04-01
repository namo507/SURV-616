#Creating outputs for Lecture 10 on Neural Networks
#JW 03-02-2022
library(nnet)
library(devtools)
library(caret)
library(neuralnet)
library(NeuralNetTools)
library(reshape)
#import the function from Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

#Step 1- Create the simulation data

set.seed(4322)

Free<-rbinom(1000,1,0.3)
Interest<-rnorm(1000,mean=3,sd=1)
Incentive<-sample(c(0,5,10,15),1000,replace=TRUE)
x<-data.frame(cbind(Free,Interest,Incentive))

#Here, I want to generate a participation decision
#That is a function of the inputs
pre.participate<-as.data.frame(Free*3+Interest/2+Incentive/5)
pre.pct<-pre.participate/max(pre.participate)

Participate<-as.numeric(runif(100,min=0,max=1)>pre.pct)
df<-data.frame(cbind(x,Participate))

#Step 3- Fit a logistic model
test.mod<-glm(Participate~Free + Incentive+Interest,data=df,family="binomial")
summary(test.mod)

df.std<-scale(x)
df.std<-data.frame(cbind(df.std,Participate))

#Step 4- Partition the data into training and test sets (small data here)
trainIndex <- createDataPartition(df.std$Participate, p = .8,
                                  list = FALSE,
                                  times = 1)
df.std.train <- df.std[ trainIndex,]
df.std.test <- df.std[-trainIndex,]

#Step 5- Fit a feed-forward NN on the training dataset
#Using the nnet function as shown in Faraway
bestrss<-10000
for (i in 1:100){
mod1<-nnet(Participate~Free+Interest+Incentive,df.std.train,size=1,entropy=TRUE)
cat(mod1$value,"\n")
if (mod1$value<bestrss){
   bestmod1<-mod1
   bestrss<-mod1$value}
  }
summary(bestmod1)
plot(bestmod1)
plot(bestmod1,bias=FALSE)

bestmod1$value
#The R2 for the fit is:
1-bestmod1$value/sum((df.std.train$Participate-mean(df.std.train$Participate))^2)

#Variable importance (using NeuralNetTools)
garson(bestmod1)


# Step 6- Predict the test data
#Predict the test data
bestmod1.test<-predict(bestmod1,new=df.std.test,type='class')
table(df.std.test$Participate,bestmod1.test)

#Creates the 'raw values'
bestmod1.raw<-predict(bestmod1,new=df.std.test,type='raw')
hist(bestmod1.raw)

#export raw values and predictors for use in spreadsheet
to_csv<-cbind(df.std.test,bestmod1.raw)
write.csv(to_csv,file="C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2023 Winter/Lectures/10 Classification/partricipate_test_raw.csv")


#Step 7 - Now using the neuralnet function, and the rep function to search across starting weights
mod2<-neuralnet(Participate ~ Free + Interest + Incentive,
                data=df.std,
                rep=100,
                err.fct="sse")

plot(mod2,rep="best")

#Print the best model weights and error
mod2$result.matrix[,which.min(mod2$result.matrix[1,])]


#Step 7 - Now using the nnet function, and the rep function to search across starting weights
#Using the nnet function as shown in Faraway
bestrss<-10000
for (i in 1:100){
  mod1<-nnet(df.std[,c(1:3)],df.std[,4],size=1,entropy=TRUE,decay=0.001)
  cat(mod1$value,"\n")
  if (mod1$value<bestrss){
    bestmod2<-mod1
    bestrss<-mod1$value}
}
summary(bestmod2)
plot.nnet(bestmod2)
bestmod2$value


#Now, using weight decay and cross-validation
### creating a grid of tuning parameters
nnetTunegrid <- expand.grid(.size=1, #not exploring size
                             .decay = seq(0,2,0.05))
cv_count<-5

# set seeds array for cross validation
#seeds <- setSeeds(cv_count, cv_repeats, nrow(nnetTunegrid), seedVal)

# Define cross-validation experiment
numFolds = trainControl(method = "LGOCV", #Leave-group out cross-validation
                        number = cv_count)


train.Participate<-factor(df.std.train[,4],levels=c(0,1),labels=c("No","Yes"))

nnetFit <- train(x = df.std.train[,c(1:3)], y = train.Participate,
                 method = "nnet",
                 #preProc = preProcessing,
                 trControl = numFolds,
                 tuneGrid = nnetTunegrid, #grid of tuning parameters
                 maxit = 500, # max iterations for nnet only
                 metric = "Accuracy")
ggplot(nnetFit)
plot(nnetFit$results[,2],nnetFit$result[,3])

nnetFit$results

#Return the maximum accuracy then maximum kappa
nnetFit$results[which.max(nnetFit$results$Accuracy),]
nnetFit$results[which.max(nnetFit$results$Kappa),]
#Solution: decay=.3

#Predict the test and examine accuracy
test.Participate<-as.numeric(predict(nnetFit,df.std.test[,c(1:3)]))-1
table(test.Participate,df.std.test[,4])



