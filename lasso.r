#Using Lasso for variable selection.
#load necessary packages
library(glmnet)
library(caret)
library(ROCR)
library(tidyverse)
library(DataExplorer)
library(plyr)

#############
#1. Read the data and format
#############

#read the gps dataset.
gps <- read.csv("/Users/namomac/Desktop/SURV-616/gps.csv")

dim(gps)
#define factors;
  # Convert GENDER variable to a factor
gps$GENDER <- as.factor(gps$GENDER)
  # Convert MARSTAT variable to a factor
gps$MARSTAT <- as.factor(gps$MARSTAT)
  # The AGE13 variable is already commented out, presumably to skip conversion
# gps$AGE13 <- as.factor(gps$AGE13)
  # Convert NONNATIV variable to a factor
gps$NONNATIV <- as.factor(gps$NONNATIV)
  # Convert ETHNIC variable to a factor
gps$ETHNIC <- as.factor(gps$ETHNIC)
  # Convert HHTYPE variable to a factor
gps$HHTYPE <- as.factor(gps$HHTYPE)
  # Convert CHILDREN variable to a factor
gps$CHILDREN <- as.factor(gps$CHILDREN)
  # Convert PHONE variable to a factor
gps$PHONE <- as.factor(gps$PHONE)
  # Convert HASJOB variable to a factor
gps$HASJOB <- as.factor(gps$HASJOB)
  # Convert SOCALL variable to a factor
gps$SOCALL <- as.factor(gps$SOCALL)
  # Convert DISABALL variable to a factor
gps$DISABALL <- as.factor(gps$DISABALL)
  # Convert REGION variable to a factor
gps$REGION <- as.factor(gps$REGION)
  # The URBAN variable is already commented out, presumably to skip conversion
# gps$URBAN <- as.factor(gps$URBAN)
  # The HOUSEVAL variable is already commented out, presumably to skip conversion
# gps$HOUSEVAL <- as.factor(gps$HOUSEVAL)
  # The PNONNAT1 variable is already commented out, presumably to skip conversion
# gps$PNONNAT1 <- as.factor(gps$PNONNAT1)
  # Convert RESPONSE variable to a factor
gps$RESPONSE <- as.factor(gps$RESPONSE)

##############
# 2. Univariate/Bivariate analysis of data
##############

  # Use DataExplorer package to explore the data structure
plot_str(gps)

  # Introduce function provides a summary of the dataset
introduce(gps)

  # Plot basic introduction plots
plot_intro(gps)

#Missing Observations refer to the number of entries or data points that are not present 
#where data was expected. This metric identifies the extent of missing data in each column or overall in the dataset.

  # Plot missing data patterns
plot_missing(gps)

  # Plot bar charts for factor variables
plot_bar(gps) # factors
  
  # Plot histograms for numerical variables
plot_histogram(gps)

  # Select specific columns for QQ plots
qq_data <- gps[, c("HOUSEVAL", "PROB", "POPWGT")]

  # Generate QQ plots, consider log transformations if needed
plot_qq(qq_data) # use log transformations?

#Straight Line: 
#If the points on the plot fall approximately along the reference line, your data are likely normally distributed.

#Systematic Deviations:
  #Heavy Tails: If the points deviate upward at the ends (tails), your data may have longer tails than the normal distribution (leptokurtic).
  #Light Tails: If the points deviate downward at the ends, your data may have shorter tails (platykurtic).

#S-Shape or Curve:
  #An S-shape can indicate skewness:
    #Right/Positive Skew: Points bend upwards at the right.

        # Load necessary libraries
        # (These might not be needed just for this basic example, but useful for additional distributions)
        library(stats)
        
        # Generate right-skewed data using the exponential distribution
        set.seed(123)  # For reproducibility
        right_skewed_data <- rexp(100, rate = 1)  # Generate right-skewed data with an exponential distribution
        
        # Q-Q plot for the right-skewed data
        qqnorm(right_skewed_data, main="Q-Q Plot of Right-Skewed Data")
        qqline(right_skewed_data, col="red", lwd=2)  # Add a reference line

        plot_histogram(right_skewed_data)
        plot_histogram(log(right_skewed_data))
        
        qqnorm(log(right_skewed_data), main="Q-Q Plot of Log Right-Skewed Data")
        qqline(log(right_skewed_data), col="red", lwd=2)  # Add a reference line
        

    #Left/Negative Skew: Points bend upwards at the left.

        # Load necessary libraries
        # (These might not be needed just for this basic example, but useful for additional distributions)
        library(stats)
        
        # Generate left-skewed data using a transformation of normal data
        set.seed(123)  # For reproducibility
        normal_data <- rnorm(100)  # Generate normal data
        left_skewed_data <- -1 * normal_data^2  # Apply a transformation to introduce left skewness
        
        # Q-Q plot for the left-skewed data
        qqnorm(left_skewed_data, main="Q-Q Plot of Left-Skewed Data")
        qqline(left_skewed_data, col="red", lwd=2)  # Add a reference line

        plot_histogram(left_skewed_data)
        
        qqnorm(exp(left_skewed_data), main="Q-Q Plot of Exp Left-Skewed Data")
        qqline(exp(left_skewed_data), col="red", lwd=2)  # Add a reference line

# Display the plot

  # Plot correlation matrix excluding missing values
plot_correlation(na.omit(gps))

  # Create a dataset with no missing data for PCA
pca_df <- na.omit(gps)
dim(gps)
dim(pca_df)

  # Plot PCA with a variance cap of 0.95, controlling presentation with nrow and ncol
plot_prcomp(pca_df, variance_cap = 0.95, nrow = 2, ncol = 2)

  # Plot boxplots by AGE3 variable
plot_boxplot(gps, by = "AGE3")

  # The following is not generally useful for factors, recommend using boxplots instead
plot_scatterplot(gps, by = "POPWGT", sampled_rows = 2000)

  # Create_report function will generate a comprehensive data report including all these plots
  # It is configurable but also has a default approach
  # Additionally, it includes functions for feature engineering -- handling missing values, grouping sparse categories, and dropping features

##################
#3. Create holdout sample (gps.test)
##################

# Split the sample into training and testing datasets

  # CreateDataPartition function is used to generate a partition index
trainIndex <- createDataPartition(gps$RESPONSE, p = .75,
                                  list = FALSE, # Return the index as a matrix
                                  times = 1) # Number of resamples is set to 1
  # Subset the gps data for training using the generated indices
gps.train <- gps[trainIndex,]
  # Subset the gps data for testing by excluding the training indices
gps.test <- gps[-trainIndex,]

###################
#4. Choose lambda for LASSO using cross-validation
###################

# Set up the data for the glmnet function

# Create a model matrix for the categorical variables using one-hot encoding without a reference level
xfactors <- model.matrix(RESPONSE ~ GENDER + MARSTAT + NONNATIV + ETHNIC + HHTYPE 
                         + CHILDREN + PHONE + HASJOB + SOCALL + DISABALL + REGION, data = gps.train,
                         contrasts.arg = lapply(gps[, c(1, 2, 25, 33, 31, 29, 16, 24, 20, 21, 15)], contrasts, contrasts = FALSE))[,-1]

gps[, c(1, 2, 25, 33, 31, 29, 16, 24, 20, 21, 15)][1:2,]

is.factor(gps$GENDER)
is.factor(gps$MARSTAT) 
is.factor(gps$NONNATIV) 
is.factor(gps$ETHNIC)
is.factor(gps$HHTYPE)
is.factor(gps$CHILDREN)
is.factor(gps$PHONE)
is.factor(gps$HASJOB) 
is.factor(gps$SOCALL) 
is.factor(gps$DISABALL)
is.factor(gps$REGION)
is.factor(gps$RESPONSE)

gps.train$CHILDREN<-as.factor(gps.train$CHILDREN)

xfactors <- model.matrix(RESPONSE ~ GENDER + MARSTAT + NONNATIV + ETHNIC + HHTYPE 
                         + CHILDREN + PHONE + HASJOB + SOCALL + DISABALL + REGION, data = gps.train,
                         contrasts.arg = lapply(gps[, c(1, 2, 25, 33, 31, 29, 16, 24, 20, 21, 15)], contrasts, contrasts = FALSE))[,-1]


  # Combine the continuous variables with the encoded factors into a single matrix
x <- as.matrix(cbind(gps.train$AGE13, gps.train$URBAN, gps.train$HOUSEVAL, gps.train$PNONNAT1, gps.train$HHSIZE, xfactors))

  # Rename the first few columns for clarity
colnames(x)[1:5] <- c("AGE13", "URBAN", "HOUSEVAL", "PNONNAT1", "HHSIZE")

  # Extract the response variable and convert it to an integer
y <- as.integer(gps.train[, c(36)])

# gps.train[, c(36)]

  # Estimate the model over a range of lambda values using the glmnet function
  # Default is to standardize predictors (standardize = TRUE)
g1 <- glmnet(x, y, family = "binomial")

  # Plot the coefficients against L1 Norm
plot(g1, label = TRUE)
 

  # Plot the coefficients against log(lambda)
plot(g1, xvar = c("lambda"), label = TRUE)
  
  #This plot will help you examine how each predictor's contribution 
  #(as indicated by the coefficient paths) changes across different 
  #values of λ

  # X-Axis - Log(lambda):
  
  #Displays the logarithm of the penalty parameter λ. 
  # This axis represents different levels of 
  #regularization applied to the model.A smaller λ
  #(moving to the right) indicates less regularization, 
  # allowing coefficients to have larger absolute values.

  #Y-Axis - Coefficients:
  
  #Shows the coefficients of the predictors in the logistic regression 
  #model.
  #Coefficients are estimated log odds associated with each predictor.

  #Coefficient Paths:
  
  #Paths show how each coefficient for the predictors changes as λ
  # is varied.
 
  # In Lasso (L1 penalty), some paths may remain at zero for larger λ
  # values, indicating variable selection where less important 
  # predictors are excluded from the model.

  #Zero Crossings:
  
  # When a path is on the y-axis (coefficient is zero), it indicates 
  # that the variable is not contributing to the model for those λ values.
  # As λ decreases, more variables may become active (non-zero coefficients).

  #Interpreting the Paths:
  
  #Predictor importance can be inferred by observing which 
  # coefficients remain non-zero and their relative size as 
  # λ decreases.

  # Paths that diverge and grow larger as regularization decreases are significant contributors to the logistic model.

  # Vertical Lines and Cross-Validation:
  
  # If cross-validation results are plotted (using cv.glmnet), 
  # vertical lines might indicate the optimal λ
  # values such as lambda.min and lambda.1se.

  #Interpretation and Usage:
  
  #Feature Selection: Observe which predictors' paths quickly drop 
  #to zero as λ  increases, as these are less important in 
  #separating your classes.

  #Model Simplicity vs. Performance: Choosing a higher λ
  #value results in a simpler model with potential less skilled predictors included.

  
  # Plot the coefficients against the fraction of deviance explained
plot(g1, xvar = "dev", label = TRUE)

  #X-Axis - Fraction of Deviance Explained:
  
  #Represents how well the model, with a particular set of coefficients, 
  #explains the variability in the data. 
  #It's similar to the R-squared value in ordinary least squares 
  #regression but adapted for generalized linear models.
  #As you move to the right, more deviance is explained, 
  #indicating that the model is capturing more of the underlying 
  #pattern in the data.
  
  #Y-Axis - Coefficients:

  #Shows the values of the coefficients for the different predictors 
  #in the model.
  #As the fraction of deviance explained increases, 
  #more coefficients become non-zero, showing their contribution to 
  #the model.

  # Run cross-validation using classification of prediction, minimizing misclassification error
cv.g1 <- cv.glmnet(x, y, family = "binomial", type.measure = "class")
plot(cv.g1)

  #X-Axis - Log(lambda):
  
  #Represents the logarithm of the regularization parameter λ.
  # Larger λ values apply stronger regularization, 
  # which usually means simpler models with fewer active predictors 
  #(coefficients closer to zero).

  #Y-Axis - Mean Cross-Validated Deviance:
  
  #Typically shows the mean binomial deviance or another suitable error 
  # metric calculated during cross-validation.
  #Lower values indicate better model performance in terms of 
  # classification error on the validation dataset.

  # Error Bars:
  
  #Show the estimated variability (standard error) of the 
  #deviance across cross-validation folds, giving an indication of the 
  # robustness of model performance at each λ.

  # Narrow error bars suggest consistent performance across 
  # the cross-validation samples.

  #Performance Curve:
  
  #The plot curve demonstrates how classification error changes 
  #with different λ values.
  #Generally, as λ decreases (moving left on the plot), 
  #more features are included, potentially decreasing deviance up to a 
  #point.

  # Vertical Lines:
  
  #Lambda.min: Represents the value of λ that minimizes 
  # the cross-validated error. This is where the model performs 
  # best in terms of predictive accuracy during cross-validation.
  #Lambda.1se: Represents the largest λ
  # within one standard error of (\lambda_{\text{min}}`. 
  # Choosing this, you get a more regularized model that is slightly 
  # simpler, possibly enhancing generalizability with almost the 
  # same performance.
    
  # Interpretation and Usage:
    # Optimal Lambda Selection:
    # Lambda.min is ideal if maximizing accuracy is your primary concern, 
    # as it picks the model with the least classification error.
    # Lambda.1se offers a more conservative choice, 
    # yielding a simpler model with almost equally good performance, 
    # potentially improving generalization on unseen data.

  # Extract the lambda values that minimize misclassification error
lambda.min <- cv.g1$lambda.min
lambda.min
log(lambda.min)

  # Extract the lambda value that is one standard error above the minimum error
lambda.1se <- cv.g1$lambda.1se
lambda.1se
log(lambda.1se)

  # Extract all of the fitted models
fit_g1 <- cv.g1$glmnet.fit

  # Evaluate misclassification error using a threshold of 0.5
gps.train <- 
  gps.train %>% 
  mutate(y_class.min = as.vector(predict(fit_g1,
                                         s = lambda.min,
                                         newx = x,
                                         type = "class")[,1]))

  # Create a confusion matrix to compare true and predicted classes
table(gps.train$RESPONSE, gps.train$y_class.min)

  # Convert predictions to factors and recalculate misclassification error
gps.train$y_class.min <- as.factor(as.numeric(gps.train$y_class.min) - 1)
  
  # Calculate confusion matrix statistics
confusion.mat <- confusionMatrix(gps.train$y_class.min, gps.train$RESPONSE)
confusion.mat

#True Positives (TP): The number of correctly predicted positive class instances.
#True Negatives (TN): The number of correctly predicted negative class instances.
#False Positives (FP): The instances incorrectly predicted as the positive class (Type I error).
#False Negatives (FN): The instances incorrectly predicted as the negative class (Type II error).

# Accuracy: Overall correctness of the model's predictions.
# Accuracy=TP+TN/TP+TN+FP+FN
# Sensitivity (Recall or True Positive Rate): 
# Proportion of actual positives correctly identified.
# Sensitivity=TP/TP+FN
# Specificity (True Negative Rate): 
# Proportion of actual negatives correctly identified.
# Specificity=TN/TN+FP
# Precision (Positive Predictive Value): 
# Proportion of predicted positives that were actually positive.
# Kappa: Measures the agreement between observed and predicted classifications, adjusted for chance agreement.

  # Calculate the Misclassification Error (1 - accuracy)
1 - confusion.mat$overall[1]

  # Run cross-validation using deviance as the measure (default setting)
cv.g2 <- cv.glmnet(x, y, family = "binomial", type.measure = "deviance")
plot(cv.g2)

  # Extract lambda values for minimum deviance and one standard error above the minimum
(cv.g2$lambda.min)
log((cv.g2$lambda.min))
(cv.g2$lambda.1se)
log((cv.g2$lambda.1se))
#####################
#5. Now, cross validation with the holdout TEST data (gps.test created above)
#####################

# Now, back to the classification error cross-validated results (cv.g1)

  # Extract coefficients under the optimum lambda (lambda.min)
coef_g1 <- coef(fit_g1, s = lambda.min)

  # Print only those coefficients with non-zero estimates
data.frame(name = coef_g1@Dimnames[[1]][coef_g1@i + 1], coefficient = coef_g1@x)

  # Set up the test data for predictions
  # Create a model matrix for the test set with one-hot encoding for categorical variables
xfactors2 <- model.matrix(RESPONSE ~ GENDER + MARSTAT + NONNATIV + ETHNIC + HHTYPE 
                          + CHILDREN + PHONE + HASJOB + SOCALL + DISABALL + REGION, data = gps.test,
                          contrasts.arg = lapply(gps[, c(1, 2, 25, 33, 31, 29, 16, 24, 20, 21, 15)], contrasts, contrasts = FALSE))[,-1]

is.factor(gps$GENDER)
is.factor(gps$MARSTAT) 
is.factor(gps$NONNATIV) 
is.factor(gps$ETHNIC)
is.factor(gps$HHTYPE)
is.factor(gps$CHILDREN)
is.factor(gps$PHONE)
is.factor(gps$HASJOB) 
is.factor(gps$SOCALL) 
is.factor(gps$DISABALL)
is.factor(gps$REGION)

is.factor(gps$CHILDREN)
gps.test$CHILDREN<-as.factor(gps.test$CHILDREN)

xfactors2 <- model.matrix(RESPONSE ~ GENDER + MARSTAT + NONNATIV + ETHNIC + HHTYPE 
                          + CHILDREN + PHONE + HASJOB + SOCALL + DISABALL + REGION, data = gps.test,
                          contrasts.arg = lapply(gps[, c(1, 2, 25, 33, 31, 29, 16, 24, 20, 21, 15)], contrasts, contrasts = FALSE))[,-1]

  # Combine continuous variables with encoded factors
x2 <- as.matrix(cbind(gps.test$AGE13, gps.test$URBAN, gps.test$HOUSEVAL, gps.test$PNONNAT1, gps.test$HHSIZE, xfactors2))
  
  # Rename columns for clarity
colnames(x2)[1:5] <- c("AGE13", "URBAN", "HOUSEVAL", "PNONNAT1", "HHSIZE")

  # Extract response variable for the test set
y2 <- as.integer(gps.test[, c(36)])

  # Compute predictions for RESPONSE using the model with the optimum lambda
gps.test <- 
  gps.test %>% 
  mutate(y_pred.min = as.vector(predict(fit_g1,
                                        s = lambda.min,
                                        newx = x2,
                                        type = "response")[,1]))

  # Check the deviance on the test sample
deviance.lasso.min <- mean(-2 * ((y2 == 2) * log(gps.test$y_pred.min) + (y2 == 1) * log(1 - gps.test$y_pred.min)))
deviance.lasso.min

  # Evaluate misclassification on the test sample
gps.test <- 
  gps.test %>% 
  mutate(y_class.min = as.vector(predict(fit_g1,
                                         s = lambda.min,
                                         newx = x2,
                                         type = "class")[,1]))

  # Create a confusion matrix for the test data
table(gps.test$RESPONSE, gps.test$y_class.min)

  # Convert predictions to factors and calculate the confusion matrix statistics
gps.test$y_class.min <- as.factor(as.numeric(gps.test$y_class.min) - 1)
confusion.mat.test <- confusionMatrix(gps.test$y_class.min, gps.test$RESPONSE)
confusion.mat.test

  # Compute misclassification error (1 - accuracy)
1 - confusion.mat.test$overall[1]

  # Look at the range of values in the new class variable
ddply(gps.test, .(y_class.min), summarise, Value = max(y_pred.min))
ddply(gps.test, .(y_class.min), summarise, Value = min(y_pred.min))

  # Interpretation: the predicted class 0's range from .11 to .4999, the predicted class 1's range from .500 to .775

  # Compare the predictions from lambda.min and lambda.1se
gps.test <- 
  gps.test %>% 
  mutate(y_pred.1se = as.vector(predict(fit_g1,
                                        s = lambda.1se,
                                        newx = x2,
                                        type = "response")[,1]))

  # Calculate deviance for lambda = 1se
deviance.lasso.1se <- mean(-2 * ((y2 == 2) * log(gps.test$y_pred.1se) + (y2 == 1) * log(1 - gps.test$y_pred.1se)))
deviance.lasso.1se

  # Reshape data for plotting
long.dat <- stack(gps.test, select = c(y_pred.min, y_pred.1se))

  # Create a density plot comparing predictions at lambda.min and lambda.1se
ggplot(long.dat, aes(x = values, fill = ind)) + geom_density(alpha = .3)
