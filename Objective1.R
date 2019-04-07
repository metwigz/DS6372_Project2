setwd("C:/Users/Satish Mylapore/Documents/SMU/MSDS 6372/Project 2/NFL Dataset")

#### --------------- Libararies used ------------------
library(tidyverse)
library(caret)
library(glmnet)
library(ggplot2)
library(broom)
library(car)
library(corrplot)
library(pscl)
#library(pROC)
library(ROCR)
library(MASS)
library(survey)
library(selectiveInference)


#### ---------------SEED for reproducibility----------------
set.seed(123)
# using df.clean from Zack's clean data

#####------------------splitting the data into train and test------------------------------
# Data is plit to 80/20 % by GOOD values into Train and Test.
df.train.index <- createDataPartition(df.clean$GOOD, p = .8, list = FALSE)
df.train <- df.clean[df.train.index,]
df.test <- df.clean[-df.train.index,]

#equal percentage of data split for train and test set.
xtabs(~GOOD+GOOD, data = df.train)
xtabs(~GOOD+GOOD, data = df.test)

##------------------------------------------------From Zac-
# df.clean.f <- df.clean[df.clean$GOOD == 0,]
# df.clean.t <- df.clean[df.clean$GOOD == 1,]
# 
# fsize <- floor(0.5 * nrow(df.clean[df.clean$GOOD == 0,]))
# df.train.f.ind <- sample(seq_len(nrow(df.clean.f)), size = fsize)
# df.train.t.ind <- sample(seq_len(nrow(df.clean.t)), size = fsize)
# df.train <- rbind(df.clean.f[df.train.f.ind,], df.clean.t[df.train.t.ind,])
# df.test <- rbind(df.clean.f[-df.train.f.ind,], df.clean.t[-df.train.t.ind,])


# Train and test are equally split (randamizing at the GOOD feature level)
plot(df.train$GOOD)
plot(df.test$GOOD)

#####------------------Logistic regression diagnostics & Assumptions ---------------------------------

##------Building Model for Diagnostics-------
model <- glm(GOOD~., data = df.train, family = binomial)

#getting the predicted probablities of each observation.
probabilities <- predict(model, type = "response")

#assigning the pridiction classes at 50% probabilities to GOOD as 1's and 0's
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
#View(predicted.classes)

### ----- Linearity assumption-----

# Select only numeric predictors
#we will check the linear relationship between continuous predictor variables and the logit of the outcome. 
#we will be visually inspecting the scatter plot between each predictor and the logit values.

diagData <- df.train %>% select_if(is.numeric)
pred <- colnames(diagData)

diagData <- diagData %>% mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(diagData, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


### ----- Influential values-----

plot(model, which = 4, id.n = 3)

model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
View(model.data %>% top_n(4, .cooksd))

### ----- Influential values----------
#.rownames GOOD   togo distance homekick kickdiff timerem timeremqtr .fitted .se.fit .resid    .hat .sigma .cooksd .std.resid index
#<chr>     <fct> <dbl>    <int> <fct>       <int>   <int>      <dbl>   <dbl>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>      <dbl> <int>
#1 262       0        20       40 1             -14    2700        900    2.23   0.462  -2.16 0.0186   0.810  0.0258      -2.18   212
#2 776       0        10       30 0              -8     891        891    3.73   0.392  -2.74 0.00352  0.808  0.0211      -2.74   615
#3 834       0        16       51 0              24     865        865    1.27   0.511  -1.74 0.0448   0.811  0.0250      -1.78   661


###---------- Influential values from Zac's dataset-------------------
# A tibble: 4 x 16
#.rownames GOOD   togo distance homekick kickdiff timerem timeremqtr .fitted .se.fit .resid   .hat .sigma .cooksd .std.resid index
#<chr>     <fct> <dbl>    <int> <fct>       <int>   <int>      <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>      <dbl> <int>
#1 936       0         7       32 0             -17     900        900    1.37   0.747  -1.79 0.0902   1.03  0.0611      -1.87    40
#2 792       0         2       40 1              41     900        900    1.15   0.988  -1.69 0.178    1.03  0.119       -1.86    44
#3 480       1         2       52 1               7    1874         74   -2.41   0.651   2.23 0.0321   1.03  0.0544       2.27    84
#4 746       1         7       53 1              -9    1118        218   -2.62   0.640   2.32 0.0260   1.03  0.0537       2.35    91


# Visualizing the data with the standardized residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = GOOD), alpha = .5) +
  theme_bw()

# removing the influential data points from training dataset
#df.train <- df.train[-c(118,212,615,661),] # not required for Zac's dataset
# repeat the process to see if the changes on removing the influential points

### ----- Multicollinearity --------------

vif(model)
#As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity. 
#In our data there is no collinearity: all variables have a value of VIF well below 5.

#      togo   distance   homekick   kickdiff    timerem timeremqtr 
#1.084855   1.053335   1.057453   1.072351   1.038193   1.058535 


##Final check
# pairs(df.train[,c(1:5, 7)], col = (as.numeric(df.clean$GOOD)+1) )
# correlations <- cor(df.train %>% select_if(is.numeric))
# corrplot(correlations, method="circle")
# pairs(df.train, col=df.train$GOOD)


#### ---------------- Penalized Logistic Regression using LASSO Model regularization ----------------------------


# Additionnal data preparation
#training data
df.train.x <- model.matrix(GOOD~., data = df.train)[,-1] #remove the intercept data
df.train.y <- df.train$GOOD
#testing data
df.test.x <- model.matrix(GOOD~., data = df.test)[,-1] #remove the intercept data
df.test.y <- df.test$GOOD

#Identifying the best lambda value to peanilize the model for regularization using cross validation
cv.lasso <- cv.glmnet(df.train.x, df.train.y , alpha = 1, family = "binomial")
plot(cv.lasso)
bestlambda <- cv.lasso$lambda.1se

coef(cv.lasso, cv.lasso$lambda.1se)
coef(cv.lasso, cv.lasso$lambda.min)
# based on the below values using the best lambda (lambda.1se) minimizes the complexity of the model to have only one feature (distance)
# using next best lambda (lambda.min) removes two features (Kickdiff and timerem). 
# Hence we will be using lambda.min to avoid high biasing.

####---- results of coef ----
# > coef(cv.lasso, cv.lasso$lambda.1se)
# 7 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  3.23393183
# togo         .         
# distance    -0.03604887
# homekick1    .         
# kickdiff     .         
# timerem      .         
# timeremqtr   .         

##### This is the coeff that will be used for further model as the previous model will make it more biased on distance.

# > coef(cv.lasso, cv.lasso$lambda.min)
# 7 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  5.853178820
# togo         .          
# distance    -0.099944889
# homekick1   -0.161344892
# kickdiff     .          
# timerem      .          
# timeremqtr   0.000287501



##### -------- Computing final LASSO Model using min lambda ----------------

df.train.lasso.model <- glmnet(df.train.x, df.train.y , alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)

# Making prediction on test data
probabilities <- df.train.lasso.model %>% predict(newx = df.test.x)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
mean(predicted.classes == df.test.y)


####-------------CONFIDENCE INTERVAL of LASSO -----------------#########

df.train.lasso.model.ci <- glmnet(df.train.x, df.train.y ,alpha=1, family = "binomial", lambda = cv.lasso$lambda.min, standardize = FALSE)

# #lasso.ci.beta <- coef(df.train.lasso.model, s=df.train.lasso.model$)
# df.train.x <- as.matrix(sapply(df.train.x, as.numeric))
# 
# df.train.y <- as.matrix(sapply(df.train.y, as.numeric))
# nrow(df.train.x)
# nrow(df.train.y)
lamb <- cv.lasso$lambda.min

beta_hat  = coef(df.train.lasso.model.ci, s=(lamb/nrow(df.train.x)), exact=TRUE, x = df.train.x, y = df.train.y)

# apply(df.train.x, 2, as.numeric)
# sapply(df.train.x, as.numeric)
class(df.train.x) <- "numeric"
storage.mode(df.train.x) <- "numeric"

# apply(df.train.y, 2, as.numeric)
# sapply(df.train.y, as.numeric)
class(df.train.y) <- "numeric"
storage.mode(df.train.y) <- "numeric"

df.train.y <- ifelse(df.train.y == 2, 1, 0)

out  = fixedLassoInf(df.train.x,df.train.y,beta_hat,df.train.lasso.model.ci$lambda,family="binomial")
out

# Call:
#   fixedLassoInf(x = df.train.x, y = df.train.y, beta = beta_hat, 
#                 lambda = df.train.lasso.model.ci$lambda, family = "binomial")
# 
# Testing results at lambda = 0.007, with alpha = 0.100
# 
# Var   Coef Z-score P-value LowConfPt UpConfPt LowTailArea UpTailArea
# 1  0.010   0.386   0.699    -0.202    0.045       0.000      0.049
# 2 -0.109  -8.148   0.000     0.083    0.131       0.049      0.049
# 3 -0.308  -1.379   0.168    -0.244    0.676       0.049      0.048
# 4  0.003   0.266   0.791    -0.125    0.016       0.050      0.049
# 5  0.000  -0.284   0.776    -0.001    0.000       0.050      0.050
# 6  0.001   1.358   0.174     0.000    0.001       0.050      0.048
# 
# Note: coefficients shown are partial regression coefficients


###########-----------testing 1's and 0's individually-----############

## testing 1's and 0's individually

df.test.x.c <- model.matrix(GOOD~., data = df.test[df.test$GOOD == 0,] )[,-1] #remove the intercept data
df.test.y.c <- (df.test[df.test$GOOD == 0,])
df.test.y.c <- df.test.y.c$GOOD

xtabs(~GOOD+GOOD, data = df.test)
xtabs(~GOOD+GOOD, data = df.test)

# Making prediction on test data
probabilities.c <- df.train.lasso.model %>% predict(newx = df.test.x.c)
predicted.classes.c <- ifelse(probabilities.c > 0.4, 1, 0)

#Confidence Interval
#confint(df.train.lasso.model)

# Model accuracy

mean(predicted.classes.c == df.test.y.c)

#########----------------Stepwise/Forward Logistic Regression-----------------#####

# will be using df.train and df.test (calculated in the begining)
df.train.forward.model <- glm(GOOD ~., data = df.train, family = binomial) %>%
  stepAIC(trace = FALSE, direction = "forward")
df.train.stepwise.model <- glm(GOOD ~., data = df.train, family = binomial) %>%
  stepAIC(trace = FALSE)

####---- Stepwise----
# Stepwise feature selection removed all the features as non significant and added only distance as a significant feature with model AIC = 571.2
summary(df.train.stepwise.model)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  6.31102    0.57765  10.925   <2e-16 ***
#  distance    -0.11026    0.01311  -8.413   <2e-16 ***
#AIC: 571.2

# Make predictions
probabilities <- df.train.stepwise.model %>% predict(df.test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes==df.test$GOOD)


####---- Forward----
# Forward feature selection removed all the features as non significant and added only distance as a significant feature with model AIC = 577.62

summary(df.train.forward.model)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  6.221e+00  6.457e-01   9.635  < 2e-16 ***
#   togo         9.857e-03  2.551e-02   0.386    0.699    
# distance    -1.092e-01  1.341e-02  -8.148 3.69e-16 ***
#   homekick1   -3.076e-01  2.231e-01  -1.379    0.168    
# kickdiff     3.003e-03  1.130e-02   0.266    0.790    
# timerem     -3.207e-05  1.129e-04  -0.284    0.776    
# timeremqtr   5.629e-04  4.145e-04   1.358    0.174   

# Make predictions
probabilities <- df.train.forward.model %>% predict(df.test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes==df.test$GOOD)




############----------K-Fold Cross Validation-------------############

# ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
# 
# df.train.glm.model <- train(GOOD~., data = df.train, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 5)
# 
# df.test.prediction = predict(df.train.glm.model, newdata=df.test)
# confusionMatrix(data=df.test.prediction, df.test$GOOD)
# 
# mean(df.test.prediction == df.test$GOOD)


####------------ROC-----------------

# Considering the AUC values of all 3 models LASSO (AUC = 0.8299383), Stepwise (0.5720165), Forward (0.5667695), LASSO is a better 
# model compared to stepwise or Forward feature selections.

# For Lasso Model
prob.lasso <- predict(df.train.lasso.model, newx =df.test.x, type="response")
pred.lasso <- prediction(prob.lasso, df.test.y)
perf.lasso <- performance(pred.lasso, measure = "tpr", x.measure = "fpr")

auc <- performance(pred.lasso, measure = "auc")
auc <- auc@y.values[[1]]
plot(perf.lasso, main="LASSO")
abline(a=0, b= 1) 
text(x = .40, y = .6,paste("AUC = ", round(auc[[1]],3), sep = ""))
auc

# Forward
#prob.forward <- predict(df.train.forward.model, newx =df.test, type="response")
probabilities.forward <- df.train.forward.model %>% predict(df.test, type = "response")
pred.forward <- prediction(probabilities.forward, df.test.y)
perf.forward <- performance(pred.forward, measure = "tpr", x.measure = "fpr")
plot(perf.forward)

auc <- performance(pred.forward, measure = "auc")
auc <- auc@y.values[[1]]
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc[[1]],3), sep = ""))
auc

# Stepwise
#prob.forward <- predict(df.train.forward.model, newx =df.test, type="response")
probabilities.stepwise <- df.train.stepwise.model %>% predict(df.test, type = "response")
pred.stepwise <- prediction(probabilities.stepwise, df.test.y)
perf.stepwise <- performance(pred.stepwise, measure = "tpr", x.measure = "fpr")
plot(perf.stepwise)

auc <- performance(pred.stepwise, measure = "auc")
auc <- auc@y.values[[1]]
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc[[1]],3), sep = ""))
auc



######################### RANDOM FOREST ####################################


# we will use the same df.train and df.test

library(randomForest)

df.train.rf.model <- randomForest(GOOD~., data = df.train)

print(df.train.rf.model)

# Call:
#   randomForest(formula = GOOD ~ ., data = df.train) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 13.82%
# Confusion matrix:
#   0   1 class.error
# 0 14  98  0.87500000
# 1 17 703  0.02361111

df.test.rf.pred <- predict(df.train.rf.model, type = "prob", newdata = df.test)
rf.pred <- prediction(df.test.rf.pred[,2], df.test$GOOD)
rf.perf = performance(rf.pred, measure = "tpr", x.measure = "fpr")

auc <- performance(rf.pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

rf.important <- importance(df.train.rf.model)

varImpPlot(df.train.rf.model)

# looking at the importance plot distance, timeremqtr,timerem looks important, hence we wiil build model with these 3 predictors
########### ----------Updated-------- ##########
df.train.rf.model.updated <- randomForest(GOOD~distance+timeremqtr+timerem+kickdiff, data = df.train)

df.test.rf.pred.updated <- predict(df.train.rf.model.updated, type = "prob", newdata = df.test)
rf.pred.updated <- prediction(df.test.rf.pred.updated[,2], df.test$GOOD)
rf.perf.updated = performance(rf.pred.updated, measure = "tpr", x.measure = "fpr")

auc <- performance(rf.pred.updated, measure = "auc")
auc <- auc@y.values[[1]]
auc


#--

# Based on the plot, LASSO models provides a better prediction and fit.
plot(rf.perf, main="ROC Curve Comparison", col=1)
# Adding all plots
plot(perf.lasso, col=2, add=TRUE)
plot(perf.forward,col=3 , add=TRUE)
plot(perf.stepwise,col=4 ,add=TRUE)
#plot(rf.perf.updated,col=5 ,add=TRUE)
legend(0.6, 0.6, legend = c("RandomForest", "LASSO", "Forward", "Stepwise"), 1:4)
#legend(0.6, 0.6, legend = c("RandomForest", "LASSO", "Forward", "Stepwise","RF Updated"), 1:5)
abline(a=0, b=1 )


