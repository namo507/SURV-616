#Worked example from Lecture 10
library(nnet)
library(devtools)
library(caret)
library(neuralnet)
library(NeuralNetTools)
library(DataExplorer)
#import the function from Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

#load(file = "C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2023 Winter/Lectures/10 Classification/worked_example.RData")

#Read in train set (from Kaggle.com)
train.tmp <- read.csv("C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2023 Winter/Lectures/10 Classification/train.csv", stringsAsFactors=TRUE)

#Kaggle.com is a competition. They provide an unlabelled test set.
#We won't be using this. If you come up with a good solution, you can make predictions
#on the test set and submit those to Kaggle.com.
#test <- read.csv("C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2022 Winter/Lectures/10 Classification/test.csv", stringsAsFactors=TRUE)

###########################################
#Partition the labeled data into training and test sets (small data here)
#"test" is our holdout sample. It is labelled. So we can use it to evaluate accuracy
#of our final model. 
trainIndex <- createDataPartition(train.tmp$price_range, p = .75,
                                  list = FALSE,
                                  times = 1)
train <- train.tmp[ trainIndex,]
test <- train.tmp[-trainIndex,]
#check for stratified samples
table(test$price_range)

###############################################
#Feature Engineering.
#Start by examining the training data.
#Review the data. Here I'm using DataExplorer package functions.
plot_str(train)
introduce(train)
plot_intro(train)
#plot_missing(train)
plot_bar(train)#factors
plot_histogram(train)
qq_data<-train[,c("battery_power","clock_speed","fc","int_memory","mobile_wt",
                  "pc","px_height","px_width","ram","sc_h","sc_w","talk_time")]
plot_qq(qq_data) 
plot_correlation(na.omit(train))
#Create data with no missingness for PCA
pca_df<-na.omit(train)
#variance cap of 0.95 let to 76 p components, nrow, ncol limits the presentation of components to the specified number in specified grid
plot_prcomp(pca_df,variance_cap=0.95,nrow=2,ncol=2)
plot_boxplot(train,by="price_range")

####################################
#Feature engineering on train and test data frames. 
#Set up factors and standardize the data.
#(The neuralnet function includes standardization and that's another way to handle this step).
blue<-as.data.frame(class.ind(as.factor(train$blue)))
names(blue)<-c("blue1","blue2")
dual_sim<-as.data.frame(class.ind(as.factor(train$dual_sim)))
names(dual_sim)<-c("dual_sim1","dual_sim2")
four_g<-as.data.frame(class.ind(as.factor(train$four_g)))
names(four_g)<-c("four_g1","four_g2")
three_g<-as.data.frame(class.ind(as.factor(train$three_g)))
names(three_g)<-c("three_g1","three_g2")
touch_screen<-as.data.frame(class.ind(as.factor(train$touch_screen)))
names(touch_screen)<-c("touch_screen1","touch_screen2")
wifi<-as.data.frame(class.ind(as.factor(train$wifi)))
names(wifi)<-c("wifi1","wifi2")
price_range<-as.data.frame(class.ind(as.factor(train$price_range)))
names(price_range)<-c("price_range1","price_range2","price_range3","price_range4")

#Standardize
train.std<-scale(train[, c(1,3,5,7:17)])

#Put factors and continuous variables back together
train.ind <- cbind(train.std, blue, dual_sim,four_g,three_g,touch_screen,wifi,price_range)

#Set up the test data
#Set up factors
blue<-as.data.frame(class.ind(as.factor(test$blue)))
names(blue)<-c("blue1","blue2")
dual_sim<-as.data.frame(class.ind(as.factor(test$dual_sim)))
names(dual_sim)<-c("dual_sim1","dual_sim2")
four_g<-as.data.frame(class.ind(as.factor(test$four_g)))
names(four_g)<-c("four_g1","four_g2")
three_g<-as.data.frame(class.ind(as.factor(test$three_g)))
names(three_g)<-c("three_g1","three_g2")
touch_screen<-as.data.frame(class.ind(as.factor(test$touch_screen)))
names(touch_screen)<-c("touch_screen1","touch_screen2")
wifi<-as.data.frame(class.ind(as.factor(test$wifi)))
names(wifi)<-c("wifi1","wifi2")
price_range<-as.data.frame(class.ind(as.factor(test$price_range)))
names(price_range)<-c("price_range1","price_range2","price_range3","price_range4")

#Standardize
test.std<-scale(test[, c(1,3,5,7:17)])

#Put factors and continuous variables back together
test.ind <- cbind(test.std, blue, dual_sim,four_g,three_g,touch_screen,wifi,price_range)


######################
#Start with a neuralnet example.
#Write the formula
nms<-names(train.ind)
frmla <- as.formula(paste("price_range1 + price_range2 + price_range3 + price_range4 ~",
                      paste(nms[!nms %in% c("price_range1","price_range2","price_range3","price_range4")], collapse = " + ")))
frmla

#build a neural network. 
#With 25 input features, I chose 10 nodes on a single hidden layer (hidden=10).
#I specify that 10 repetitions (rep=10) be tried to see which converges to a best solution.
mods<-neuralnet(frmla,data=train.ind,
                rep=10,act.fct="logistic",
                linear.output=FALSE,lifesign="minimal",
                hidden=10)
plot(mods,rep="best")
#Look at the distribution of error -- is 10 enough repetitions?
mods$result.matrix[1,]
#Print the best model weights and error
mods$result.matrix[,which.min(mods$result.matrix[1,])]
#Here, I get the rep number for the rep with lowest error
which.min(mods$result.matrix[1,])

#compare predicted and observed on the training data
bestmod1.train<-as.data.frame(predict(mods,rep=5,newdata=train.ind))
bestmod1.train<-cbind(bestmod1.train,train.ind[,c(27:30)])
table(round(bestmod1.train[,1],0),bestmod1.train[,5])
table(round(bestmod1.train[,2],0),bestmod1.train[,6])
table(round(bestmod1.train[,3],0),bestmod1.train[,7])
table(round(bestmod1.train[,4],0),bestmod1.train[,8])


#now create predictions on the test data and compare to observed
bestmod1.test<-as.data.frame(predict(mods,rep=1,newdata=test.ind),rep=which.min(mods$result.matrix[1,]))
bestmod1.test<-cbind(bestmod1.test,test.ind[,c(27:30)])
hist(bestmod1.test$V1)
table(round(bestmod1.test$V1,0),bestmod1.test$price_range1)
table(round(bestmod1.test$V2,0),bestmod1.test$price_range2)
table(round(bestmod1.test$V3,0),bestmod1.test$price_range3)
table(round(bestmod1.test$V4,0),bestmod1.test$price_range4)

#Compute overall accuracy
#First, just check to see if any cases get more than one range predicted
bestmod1.test$test.sum<-bestmod1.test$price_range1+bestmod1.test$price_range2+bestmod1.test$price_range3+bestmod1.test$price_range4
table(bestmod1.test$test.sum)
#Second, create categorical version
bestmod1.test$pred_price_range[round(bestmod1.test$V1,0)==1]<-0
bestmod1.test$pred_price_range[round(bestmod1.test$V2,0)==1]<-1
bestmod1.test$pred_price_range[round(bestmod1.test$V3,0)==1]<-2
bestmod1.test$pred_price_range[round(bestmod1.test$V4,0)==1]<-3
#Add the original variables
bestmod1.test$obs_price_range<-test$price_range

#Now look at accuracy using confusionMatrix function from caret package
xtab<-table(bestmod1.test$pred_price_range,bestmod1.test$obs_price_range)
confusionMatrix(xtab)


#####################################
#Now use caret train-control to identify best size and weight decay.
#
#I will search a grid of possible sizes and weight decays.
#Need to choose a limited number of options for the grid. 
#Here I have 5 options for size and 11 options for weight decay.
#5x11=55 different options to test. On my laptop, this ran for about 20 minutes.
#
#I could reduce the time by set "maxit" to 50 in the call to the "train" function
#without much loss. Very little change occured after 50 iterations
#(the function outputs the value after every 10 iterations, so you can see the 
#rate of convergence.)
#
#Using k-fold (k=10) cross-validation to pick best size and weight decay.
#####################################
nnetTunegrid <- expand.grid(.size=seq(1,20,4), 
                            .decay = seq(0,1,0.1))
cv_count<-10

# Define cross-validation experiment
numFolds = trainControl(method = "LGOCV", #Leave-group out cross-validation
                        number = cv_count)

#Format the output layer as a factor with levels 0-3
train.Price_Range<-factor(train$price_range,levels=c(0,1,2,3))
#train function is function from caret. 
#Note that I am using it to call the nnet function (method="nnet")
nnetFit <- train(x = train.ind[,c(1:26)], y = train.Price_Range,
                 method = "nnet",
                 trControl = numFolds,
                 tuneGrid = nnetTunegrid,
                 maxit = 500, #max iterations for nnet only, set lower (50) to reduce computation
                 metric = "Accuracy") # default metric is RMSE, 
#for classification I chose "Accuracy". "Kappa" is another option offered 
#by the train function. Custom metrics also possible.
ggplot(nnetFit)
#Return the maximum accuracy then maximum kappa
nnetFit$results[which.max(nnetFit$results$Accuracy),]
nnetFit$results[which.max(nnetFit$results$Kappa),]
#Solution: decay=.3

#Now make predictions on the test data (our holdout sample)
#Predict the test and examine accuracy
test.Price_Range<-as.numeric(predict(nnetFit,test.ind[,c(1:26)]))-1
cv_xtab<-table(test.Price_Range,test$price_range)
confusionMatrix(cv_xtab)

#Finally, a method for looking at variable importance.
# varimp function (from the caret package, requires nnet object)
imp<-varImp(nnetFit)
imp
plot(imp$importance[,1])#feature 11, ram, dominates


##############################
#We could have used ordered logistic regression as an alternative approach.
#Ordered logistic regression predicts probabilities for an ordinal scale.
#That's what we have here. So, see how that method performs compared to the
#neural network approach. 

#Use ordered logistic regression and ram as a predictor
library(MASS)
train$price_range <- factor(train$price_range, levels = c("0", "1", "2", "3"), ordered = TRUE) 
o.logit<-polr(price_range~ram,data=train)
summary(o.logit)
test$olog_pred<-predict(o.logit,newdata=test)
xtab2<-table(test$price_range,test$olog_pred)
#Not nearly as good as neural network
confusionMatrix(xtab2)

save.image(file = "C:/Users/jw/Dropbox (University of Michigan)/Teaching/Stat Methods II/2023 Winter/Lectures/10 Classification/worked_example.RData")