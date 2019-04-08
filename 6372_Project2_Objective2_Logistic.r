library(caret)

#Dataframe to save the overall accuracy from the trials
df.results <- data.frame(percent=c(), cut = c(), acc = c())

for(i in seq(from=0.5, to=0.8, by=0.05) )
{
  set.seed(123)
  df.train.index <- createDataPartition(df.clean$GOOD, p = 0.5, list = FALSE)
  df.train <- df.clean[df.train.index,]
  df.test <- df.clean[-df.train.index,]
  
  # 5 fold cross validation
  train_control <- trainControl(method = "cv", number = 5)
  # Initial test with including all interaction and squared terms
  df.fit2 <- train(GOOD ~ togo + distance + homekick + kickdiff + timerem + timeremqtr + 
                     togo*distance + togo*homekick + togo*kickdiff + togo*timerem + togo*timeremqtr +
                     distance*homekick + distance*kickdiff + distance*timerem + distance*timeremqtr +
                     homekick*kickdiff + homekick*timerem + homekick*timeremqtr +
                     kickdiff*timerem + kickdiff*timeremqtr +
                     timerem*timeremqtr +
                     togo*togo + distance*distance + homekick*homekick + kickdiff*kickdiff +
                     timerem*timerem + timeremqtr*timeremqtr,
                   data = df.train, trControl = train_control, method = "glmStepAIC", family=binomial(), trace = FALSE)
  # print cv stepwise scores
  summary(df.fit2)
  
  #===Just looking at training set
  confusionMatrix(predict(df.fit2, newdata = df.test), df.test$GOOD)
  
  #Does the ROC curve plotting, gets the optimal cut point,
  #cuts the prediction to match, and prints out the confusion matrix
  df.pred <- predict(df.fit2, newdata = df.train, type = "prob")[,2]
  df.cut <- printcutroc(df.fit2, df.pred, df.train$GOOD)
  df.pred <- as.factor(ifelse(df.pred >= df.cut, 1, 0))
  confusionMatrix(df.pred, df.train$GOOD)
  
  #Saves the 1/0 split, cut point (from ROC curve), and accuracy 
  df.results <- rbind(df.results, data.frame(percent=i, cut = df.cut, acc = confusionMatrix(df.pred, df.train$GOOD)$overall['Accuracy']))
}

rownames(df.results) <- c()
#Print out the values of the percent, cut, and accuracy
df.results

#----BASED ON THESE TESTS (and a run on the test set), USING THE 50/50 SPLIT FOR GOOD 0/1's---------
set.seed(123)
df.train.index <- createDataPartition(df.clean$GOOD, p = 0.5, list = FALSE)
df.train <- df.clean[df.train.index,]
df.test <- df.clean[-df.train.index,]

# 5 fold cross validation
train_control <- trainControl(method = "cv", number = 5)
# Initial test with including all interaction terms
df.fit2 <- train(GOOD ~ togo + distance + homekick + kickdiff + timerem + timeremqtr + 
                   togo*distance + togo*homekick + togo*kickdiff + togo*timerem + togo*timeremqtr +
                   distance*homekick + distance*kickdiff + distance*timerem + distance*timeremqtr +
                   homekick*kickdiff + homekick*timerem + homekick*timeremqtr +
                   kickdiff*timerem + kickdiff*timeremqtr +
                   timerem*timeremqtr +
                   togo*togo + distance*distance + homekick*homekick + kickdiff*kickdiff +
                   timerem*timerem + timeremqtr*timeremqtr,
                 data = df.train, trControl = train_control, method = "glmStepAIC", family=binomial(), trace = FALSE)

#RIDGE REGRESSION
#train_control <- trainControl(method = "cv", number = 5)
#df.fit2 <- train(GOOD ~ togo + distance + homekick + kickdiff + timerem + timeremqtr + 
#                   togo*distance + togo*homekick + togo*kickdiff + togo*timerem + togo*timeremqtr +
#                   distance*homekick + distance*kickdiff + distance*timerem + distance*timeremqtr +
#                   homekick*kickdiff + homekick*timerem + homekick*timeremqtr +
#                   kickdiff*timerem + kickdiff*timeremqtr +
#                   timerem*timeremqtr +
#                   togo*togo + distance*distance + homekick*homekick + kickdiff*kickdiff +
#                   timerem*timerem + timeremqtr*timeremqtr,
#                 data = df.train, trControl = train_control, tuneLength = 10, method = "glmnet", family="binomial")

#coef(df.fit2$finalModel, df.fit2$bestTune$lambda)
#togo + homekick + distance + timerem + togo*homekick + distance*homekick + kickdiff*timerem

# print cv stepwise scores
summary(df.fit2)

#---------------------RESULTS OF THE STEPWISE RUN------------------------------
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           7.465e+00  8.543e-01   8.738  < 2e-16 ***
#  distance             -1.299e-01  1.854e-02  -7.006 2.45e-12 ***
#  `togo:homekick1`     -8.092e-02  3.103e-02  -2.608  0.00911 ** 
#  `togo:timeremqtr`     1.660e-04  8.136e-05   2.041  0.04127 *  
#  `kickdiff:timerem`   -2.008e-05  1.008e-05  -1.992  0.04641 *  
#  `timerem:timeremqtr` -4.923e-07  2.651e-07  -1.857  0.06335 .  

#---------------------MODEL USING ONLY THE ONES FROM STEPWISE-------------------
df.fit2 <- train(GOOD ~ togo + distance + homekick + kickdiff + timerem + timeremqtr +
                   togo*homekick + togo*timeremqtr + kickdiff*timerem,
                 data = df.train, trControl = train_control, method = "glm", family=binomial(), trace = FALSE)
#Ridge model:
#df.fit2 <- train(GOOD ~ togo + homekick + distance + timerem +
#                   togo*homekick + distance*homekick + kickdiff*timerem,
#                 data = df.train, trControl = train_control, method = "glm", family=binomial(), trace = FALSE)


#Confusion matrix before fixing the cut from the ROC curve
confusionMatrix(predict(df.fit2, newdata = df.test), df.test$GOOD)

#Does the ROC curve plotting, gets the optimal cut point,
#cuts the prediction to match, and prints out the confusion matrix
df.pred <- predict(df.fit2, newdata = df.train, type = "prob")[,2]
df.cut <- printcutroc(df.fit2, df.pred, df.train$GOOD)
df.pred <- as.factor(ifelse(df.pred >= df.cut, 1, 0))
confusionMatrix(df.pred, df.train$GOOD)

#RUNNING THE TEST SET
df.pred.test <- predict(df.fit2, newdata = df.test, type = "prob")[,2]
df.pred.test <- as.factor(ifelse(df.pred.test >= df.cut, 1, 0))
confusionMatrix(df.pred.test, df.test$GOOD)

