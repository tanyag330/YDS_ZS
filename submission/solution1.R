
getwd()
setwd("/Users/home/Desktop/submission")

# load library
library(data.table)


# load and check data ---------------------------------------------------------------

train <- fread("train.csv")
test <- fread("test.csv")
sample_sub <- fread("sample_submission.csv")

head(train)
head(test)

str(train)
str(test)

# order data
train <- train[order(PID)]
test <- test[order(PID)]

# Predicting future events based on popular past events per patient -------
train_dcast <- dcast(data = train, PID + Date ~ Event, sep = "1",  length, drop = TRUE , value.var = "Event")

# get top 10 events per row
random_submit <- colnames(train_dcast)[-1][apply(train_dcast[,-c('PID'),with=F],1, function(x)order(-x)[1:10])]

# create the submission file
random_mat <- as.data.table((matrix(random_submit,ncol = 10, byrow = T)))
colnames(random_mat) <- colnames(sample_sub)[-1]
random_mat <- cbind(PID = test$PID, random_mat)
fwrite(random_mat,"solution.csv")