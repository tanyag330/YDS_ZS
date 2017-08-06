#set path

getwd()
setwd("/Users/home/Desktop/submission")

#install packages
install.packages("dplyr")
install.packages("randomForest")
install.packages("caret")
install.packages("Metrics")
install.packages("mice")

# load library
library('dplyr')
library('randomForest')
library('caret')
library('Metrics')
library('mice')
library(data.table)


## Reading the Training data into R

zs_train <- read.csv("Train.csv",header = T, stringsAsFactors = F)
zs_test <- read.csv("test.csv",header = T, stringsAsFactors = F)
zs_sample_sub <- read.csv("sample_submission.csv",header = T, stringsAsFactors = F)

head(train)
head(test)

str(train)
str(test)

#checking data
summary(train)
summary(test)

#writing queries
t<-train[!duplicated(train[,c('PID','Date')]),]
hospital <- left_join(test, t)

sum(is.na(t))
mice_mod <- mice(t[, ], method='rf')
mice_output <- complete(mice_mod)
t <- mice_output
sum(is.na(t))

t$PID<-(as.factor(t$PID))
t$Date<-as.factor(t$Date)

t_reserved<-t


outcomeName<-c('event1')
predictors <- c('PID','Date','Event')

s<-sample_sub

summary(s$event1)

#All possible combinations of PID , Date and Event.
x<-expand.grid(PID=levels(train$PID),Date=levels(train$Date),Event=levels(train$Event))



#############################################################################

# order data
train <- train[order(PID)]
test <- test[order(PID)]

# Predicting future events based on popular past events per patient -------
casted <- dcast(data = train, PID + Date ~ Event, sep = "1",  length, drop = TRUE , value.var = "Event")

# get top 10 events per row
submit <- colnames(casted)[-1][apply(casted[,-c('PID'),with=F],1, function(x)order(-x)[1:10])]

# create the submission file
upcoming_events <- as.data.table((matrix(submit,ncol = 10, byrow = T)))
colnames(upcoming_events) <- colnames(sample_sub)[-1]
upcoming_events <- cbind(PID = test$PID, upcoming_events)
fwrite(upcoming_events,"solution.csv")
#############################################################################