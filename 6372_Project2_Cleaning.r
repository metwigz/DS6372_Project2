#Read in the data
df.orig <- read.csv("data/nfl2008_fga.csv")
names(df.orig)

#Number of rows
length(df.orig$GameDate)

#Gets the number of NA's in the dataset
missing <- colSums(is.na(df.orig))
missing

#Put the data into a new dataframe to clean
df.clean <- df.orig

#DET vs GB information (about the NA)
#https://www.pro-football-reference.com/boxscores/200812280gnb.htm
#NYG vs ARI information (about the NA)
#https://www.pro-football-reference.com/boxscores/200811230crd.htm

#By logic and the websites, down has to be 1 and togo has to be 10
df.clean$down[df.clean$HomeTeam == "ARI" & df.clean$AwayTeam == "NYG" & df.clean$GameDate == 20081123 & df.clean$timerem == 1811] <- 1
df.clean$togo[df.clean$HomeTeam == "ARI" & df.clean$AwayTeam == "NYG" & df.clean$GameDate == 20081123 & df.clean$timerem == 1811] <- 10
df.clean$down[df.clean$HomeTeam == "GB" & df.clean$AwayTeam == "DET" & df.clean$GameDate == 20081228 & df.clean$timerem == 1807] <- 1
df.clean$togo[df.clean$HomeTeam == "GB" & df.clean$AwayTeam == "DET" & df.clean$GameDate == 20081228 & df.clean$timerem == 1807] <- 10

#Gets the number of NA's in the dataset
colSums(is.na(df.clean))

#Make a factor: qtr, down, GOOD, homekick
df.clean$qtr <- as.factor(df.clean$qtr)
df.clean$down <- as.factor(df.clean$down)
df.clean$GOOD <- as.factor(df.clean$GOOD)
df.clean$homekick <- as.factor(df.clean$homekick)

#Remove Missed and Blocked because they are also part of the response
df.clean$Missed <- NULL
df.clean$Blocked <- NULL

#GameDate won't help for future predictions
#Name and Kicker won't help for future predictions
#season is always 2008
df.clean$GameDate <- NULL
df.clean$name <- NULL
df.clean$kicker <- NULL
df.clean$season <- NULL

#We don't need AwayTeam or HomeTeam
df.clean$HomeTeam <- NULL
df.clean$AwayTeam <- NULL

#We don't care who the kick team is, just
#if the team is at home or away (homekick)
df.clean$kickteam <- NULL
df.clean$def <- NULL

pairs(df.clean, gap=0)

#Outliers visible in ydline vs distance
plot(df.clean$ydline, df.clean$distance)
#ydline is wrong on these
df.clean[df.clean$ydline > 50,]
#This is no problem because distance is fine and we are removing ydline
#ydline and distance are equivelent
df.clean$ydline <- NULL

#----------RUN CONTINUOUS CORRELATION SECTION AND COME BACK HERE

#timerem is correlated with qtr,min,sec
#Creating a new column "Time Remaining in Quarter" (timeremqtr)
df.clean$timeremqtr <- (df.clean$timerem - (4 - as.numeric(df.clean$qtr))*15*60)
#Don't need min, sec: they are redundant
df.clean$min <- NULL
df.clean$sec <- NULL

#kickdiff is correlated with offscore and defscore
#We will just keep the difference in score
df.clean$offscore <- NULL
df.clean$defscore <- NULL

xtabs(~qtr + down, data = df.clean)       #PROBLEM HERE, several 0's
xtabs(~qtr + homekick, data = df.clean)   #PROBLEM HERE qtr 5, few values
xtabs(~qtr + GOOD, data = df.clean)       #PROBLEM HERE qtr 5, few values
xtabs(~down + homekick, data = df.clean)
xtabs(~down + GOOD, data = df.clean)
xtabs(~homekick + GOOD, data = df.clean)

#Removing qtr because it is problematic, it does
#not have enough of some values, see xtabs above
df.clean$qtr <- NULL

pairs(df.clean, gap=0)

#-----------CORRELATION OF CONTINUOUS---------------------------------------------
library(corrplot)
df.clean.numeric <- df.clean[, sapply(df.clean, is.numeric)]
#Correlations of all numeric variables
df.clean.allcor <- cor(df.clean.numeric, use="pairwise.complete.obs")
#The cutoff point for correlation, currently randomly assigned
corr_amt <- 0.3

#Finds rows [A,B,123], [B,A,123] and removes them (duplicates)
rmCorDup <- function(res)
{
  rmrow <- numeric()
  len <- length(res$Var1)
  for( i in c(1:(len-1)) )
  {
    num <- i+1
    for(j in c(num:len))
    {
      if( (res[i,1] == res[j,2]) & (res[i,2] == res[j,1]) )
      {
        rmrow <- c(rmrow, j)
      }
    }
  }
  res <- res[-rmrow,]
  res
}

#Gets a list of all correlations with higher than 'corr_amt' of correlation
df.clean.highcor <- rmCorDup(subset(as.data.frame(as.table(df.clean.allcor)), (abs(Freq) > corr_amt) & (abs(Freq) < 1)))
df.clean.highcor
#Vector of the names of the columns with high correlation
df.clean.highcor.names <- unique( c(as.vector(df.clean.highcor$Var1), as.vector(df.clean.highcor$Var2)) )

#Creates a matrix of high correlation for the graphic
df.clean.highcor.matrix <- df.clean.allcor[df.clean.highcor.names, df.clean.highcor.names]
#Creates the high correlation graphic
corrplot.mixed(df.clean.highcor.matrix, tl.col="black", tl.pos = "lt")

#------CORRELATION OF MIXED------------------------------------------------
library(ggplot2)

df.clean.num1 <- df.clean[, sapply(df.clean, is.numeric)]
df.clean.cat1 <- df.clean[, sapply(df.clean, is.factor)]

for(i in c(1:ncol(df.clean.num1)) )
{
  for(j in c(1:ncol(df.clean.cat1)) )
  {
    plot <- ggplot(NULL, aes(df.clean.cat1[,j], df.clean.num1[,i])) +
      geom_boxplot() +
      xlab(names(df.clean.cat1)[j]) +
      ylab(names(df.clean.num1)[i])
    print(plot)
  }
}

#Most questionable ones from visual inspection
#timeremqtr, down
plot <- ggplot(df.clean, aes(down, timeremqtr)) +
  geom_boxplot() +
  xlab("down") +
  ylab("timeremqtr")
print(plot)
#Kruskal-Wallis ANOVA
fit <- kruskal.test(timeremqtr~down, data = df.clean)
fit

#timerem, down
plot <- ggplot(df.clean, aes(down, timerem)) +
  geom_boxplot() +
  xlab("down") +
  ylab("timeremqtr")
print(plot)
#Kruskal-Wallis ANOVA
fit <- kruskal.test(timerem~down, data = df.clean)
fit

#down, togo
plot <- ggplot(df.clean, aes(down, togo)) +
  geom_boxplot() +
  xlab("down") +
  ylab("timeremqtr")
print(plot)
fit <- kruskal.test(togo~down, data = df.clean)
fit

#Each of these ANOVAs indicate that the values are highly correlated
#(all p-values <3.637e-07)

#Because each of these are highly associated with 'down' we remove down
df.clean$down <- NULL

#------CORRELATION OF CATEGORICAL------------------------------------------
#MANTEL-HAENSZEL?

#pairs plot with GOOD colored into it
pairs(df.clean[,c(1:5, 7)], col = (as.numeric(df.clean$GOOD)+1) )

#=============================OBJECTIVE 1=====================================

#------------------EQUAL % TRAINING SET-----------------------------
set.seed(123)
df.clean.f <- df.clean[df.clean$GOOD == 0,]
df.clean.t <- df.clean[df.clean$GOOD == 1,]

fsize <- floor(0.5 * nrow(df.clean[df.clean$GOOD == 0,]))
df.train.f.ind <- sample(seq_len(nrow(df.clean.f)), size = fsize)
df.train.t.ind <- sample(seq_len(nrow(df.clean.t)), size = fsize)
df.train <- rbind(df.clean.f[df.train.f.ind,], df.clean.t[df.train.t.ind,])
df.test <- rbind(df.clean.f[-df.train.f.ind,], df.clean.t[-df.train.t.ind,])

#NORMAL TRAINING/TEST SET
#smp_size <- floor(0.5 * nrow(df.clean))
#set.seed(123)
#train_ind <- sample(seq_len(nrow(df.clean)), size = smp_size)
#df.train <- df.clean[train_ind, ]
#df.test <- df.clean[-train_ind, ]

#-----Logistic Regression---------------------------------------------------
library(caret)

#JUST AN EXAMPLE
# 5 fold cross validation
train_control <- trainControl(method = "cv", number = 5)
#Stepwise with the cross validation
df.fit1 <- train(GOOD ~ ., data = df.train, trControl = train_control, method = "glmStepAIC", family=binomial())
summary(df.fit1)

#=============================OBJECTIVE 2======================================

# 5 fold cross validation
train_control <- trainControl(method = "cv", number = 5)
# Initial test with including all interaction terms
df.fit2 <- train(GOOD ~ .^2, data = df.train, trControl = train_control, method = "glmStepAIC", family=binomial())
# print cv stepwise scores
summary(df.fit2)

confusionMatrix(predict(df.fit2, newdata = df.test), df.test$GOOD)

#-----LDA after correlation/collinear fixed?--------------------------------
library(MASS)
df.clean.lda <- lda(GOOD~., data = df.clean)




