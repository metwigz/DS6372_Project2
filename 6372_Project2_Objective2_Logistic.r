library(caret)
library(car)

set.seed(123)
df.train.index <- createDataPartition(df.clean$GOOD, p = .8, list = FALSE)
df.train <- df.clean[df.train.index,]
df.test <- df.clean[-df.train.index,]

fit.full <- glm(GOOD ~togo + distance + homekick + kickdiff + timerem + timeremqtr + 
                  togo*distance + togo*homekick + togo*kickdiff + togo*timerem + togo*timeremqtr +
                  distance*homekick + distance*kickdiff + distance*timerem + distance*timeremqtr +
                  homekick*kickdiff + homekick*timerem + homekick*timeremqtr +
                  kickdiff*timerem + kickdiff*timeremqtr +
                  timerem*timeremqtr +
                  togo*togo + distance*distance + homekick*homekick + kickdiff*kickdiff +
                  timerem*timerem + timeremqtr*timeremqtr, data = df.train, family = binomial)
fit.min <- glm(GOOD ~1, data = df.train, family = binomial)
df.train.stepwise.model <- stepAIC(fit.min, direction = "both",
                                   scope=list(upper=fit.full, lower=fit.min), trace = FALSE)

#probabilities <- df.train.stepwise.model %>% predict(df.test, type = "response")
#predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
#mean(predicted.classes==df.test$GOOD)

# Does the ROC curve plotting, gets the optimal cut point,
# cuts the prediction to match, and prints out the confusion matrix
df.train.step.pred <- predict(df.train.stepwise.model, df.train, type = "response")
df.step.cut <- printcutroc(df.train.stepwise.model, df.train.step.pred, df.train$GOOD) #custom function from cleaning file
df.train.step.pred <- as.factor(ifelse(df.train.step.pred >= df.step.cut, 1, 0))
confusionMatrix(df.train.step.pred, df.train$GOOD) #confusion matrix of the training

# Making prediction and confusion matrix on test data using df.cut
df.test.step.pred <- predict(df.train.stepwise.model, df.test, type = "response")
#printcutroc(df.train.stepwise.model, df.test.step.pred, df.test$GOOD) #custom function from cleaning file
df.test.step.pred <- as.factor(ifelse(df.test.step.pred >= df.step.cut, 1, 0))
confusionMatrix(df.test.step.pred, df.test$GOOD) #confusion matrix of the test data

summary(df.train.stepwise.model)

vif(df.train.stepwise.model)

#----------------------------------------------------------------------------------
#-------------------------IGNORE EVERTHING BELOW THIS------------------------------
#----------------------------------------------------------------------------------
#Dataframe to save the overall accuracy from the trials
df.results <- data.frame(percent=c(), cut = c(), acc = c(), sens = c(), specif = c())

for(i in seq(from=0.5, to=0.8, by=0.05) )
{
  set.seed(123)
  df.clean.f <- df.clean[df.clean$GOOD == 0,]
  df.clean.t <- df.clean[df.clean$GOOD == 1,]
  
  fsize <- floor(i * nrow(df.clean[df.clean$GOOD == 0,]))
  df.train.f.ind <- sample(seq_len(nrow(df.clean.f)), size = fsize)
  df.train.t.ind <- sample(seq_len(nrow(df.clean.t)), size = fsize)
  df.train <- rbind(df.clean.f[df.train.f.ind,], df.clean.t[df.train.t.ind,])
  df.test <- rbind(df.clean.f[-df.train.f.ind,], df.clean.t[-df.train.t.ind,])
  
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
  confuzzle <- confusionMatrix(df.pred, df.train$GOOD)
  df.results <- rbind(df.results, data.frame(percent=i, cut = df.cut, acc = confuzzle$overall['Accuracy'], 
                                             sens = confuzzle$byClass['Sensitivity'], specif = confuzzle$byClass['Specificity']))
}

rownames(df.results) <- c()
#Print out the values of the percent, cut, and accuracy
df.results

#set.seed(123)
#Splitting the data into GOOD = 0,1
#df.clean.f <- df.clean[df.clean$GOOD == 0,]
#df.clean.t <- df.clean[df.clean$GOOD == 1,]

#Making the training set have 50/50 0's and 1's
#fsize <- floor(0.50 * nrow(df.clean[df.clean$GOOD == 0,]))
#df.train.f.ind <- sample(seq_len(nrow(df.clean.f)), size = fsize)
#df.train.t.ind <- sample(seq_len(nrow(df.clean.t)), size = fsize)
#df.train <- rbind(df.clean.f[df.train.f.ind,], df.clean.t[df.train.t.ind,])
#df.test <- rbind(df.clean.f[-df.train.f.ind,], df.clean.t[-df.train.t.ind,])

#-----------------------------------------------------------------------------------------
set.seed(123)
df.train.index <- createDataPartition(df.clean$GOOD, p = .8, list = FALSE)
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
confusionMatrix(predict(df.fit2, newdata = df.train), df.train$GOOD)

#Does the ROC curve plotting, gets the optimal cut point,
#cuts the prediction to match, and prints out the confusion matrix
df.pred <- predict(df.fit2, newdata = df.train, type = "prob")[,2]
df.cut <- printcutroc(df.fit2, df.pred, df.train$GOOD)
#df.pred <- as.factor(ifelse(df.pred >= 0.5, 1, 0))
df.pred <- as.factor(ifelse(df.pred >= df.cut, 1, 0))
confusionMatrix(df.pred, df.train$GOOD)

#RUNNING THE TEST SET
df.pred.test2 <- predict(df.fit2, newdata = df.test, type = "prob")[,2]
df.pred.test2 <- as.factor(ifelse(df.pred.test2 >= df.cut, 1, 0))
confusionMatrix(df.pred.test2, df.test$GOOD)

vif(glm(GOOD ~ togo + distance + kickdiff + togo*distance + togo*homekick + distance*kickdiff, data = df.train, family = binomial))

