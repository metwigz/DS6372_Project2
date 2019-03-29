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

#Remove Missed and Blocked because they are also part of the response
df.clean <- df.clean[, c(1:21)]

#GameData won't help for future predictions
#Name and Kicker won't help for future predictions
#season is always 2008
df.clean <- df.clean[,c(2:10,12,14,16:19,21)]

pairs(df.clean, gap=0)

#TODO: 
#ydline and distance are equivelent
#Season is pointless to include
#timerem is correlated with qtr,min,sec
#homekick is redundant with kickteam/def, I would remove homekick and keep the other two

#Make a factor: qtr, down, GOOD
df.clean$qtr <- as.factor(df.clean$qtr)
df.clean$down <- as.factor(df.clean$down)
df.clean$GOOD <- as.factor(df.clean$GOOD)

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

#------CORRELATION OF CATEGORICAL------------------------------------------
#MANTEL-HAENSZEL?


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

#-----LDA after correlation/collinear fixed?--------------------------------
library(MASS)
df.clean.lda <- lda(GOOD~., data = df.clean)

#-----Logistic Regression---------------------------------------------------
#JUST AN EXAMPLE
df.fit1 <- glm(GOOD~., data = df.clean, family=binomial(link="logit"))

#EXAMPLE STEPWISE
df.step <- step(df.fit1)
summary(df.step)



