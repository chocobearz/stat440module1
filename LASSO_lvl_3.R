# remove all variables from the workspace
rm(list = ls(all = TRUE))
setwd("/Users/jerem/Downloads/STAT 440/Module 1/stat440module1-master/data")
#load cleaned train and test data
train <- read.csv("level3.csv", sep=",")


test <- read.csv("test_level3.csv", sep=",")

#change the value from T/F to 0 and 1
train[,7:10] <- (train[,7:10])*1
test[,7:10] <- (test[,7:10])*1

#Library
library(mice) #for missing values
library(caret) #for dummyVars
library(glmnet) #for LASSO

#impute missing numerical values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
temp.num.train <- mice(train,m=5,maxit=50,meth='pmm',seed=500)
train.complete.num <- complete(temp.num.train,1)
temp.test <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
test.complete <- complete(temp.test,1)
apply(test.complete,2,pMiss)

#impute missing binary values (date) in train data
train.complete.num$confirmed = as.factor(train.complete.num$confirmed)
test.complete$confirmed = as.factor(test.complete$confirmed)

temp.date.train <- mice(train.complete.num,m=5,maxit=50,meth='logreg',seed=500)
train.complete.date<- complete(temp.date.train,1)
View(train.complete.date)
apply(train.complete.date,2,pMiss)

#impute missing factors values (clean_age) in train data
train.complete.date$clean_age = as.factor(train.complete.date$clean_age)
train.complete.date$clean_age = as.factor(train.complete.date$clean_age)

temp.age.train <- mice(train.complete.date,m=5,maxit=50,meth='polyreg',seed=500)
train.complete<- complete(temp.age.train,1)
View(train.complete)
apply(train.complete,2,pMiss)

#LASSO
y.1.cv <- train.complete$duration
x.1.cv <- data.matrix(train.complete[,c(-6,-1)])
y.2.cv <- test.complete$duration
x.2.cv <- data.matrix(test.complete[,c(-6,-1)])
cv.lasso.2 <- cv.glmnet(y=y.1.cv, x= x.1.cv, family="gaussian")
pred.las1.min <- predict(cv.lasso.2, newx=x.2.cv, s=cv.lasso.2$lambda.min)
pred.las1.1se <- predict(cv.lasso.2, newx=x.2.cv, s=cv.lasso.2$lambda.1se)

baseline = as.data.frame(cbind(test$X, pred.las1.min))
colnames(baseline)[1:2] <-c("Id","duration")
baseline
write.table(baseline, file = "/Users/jerem/Downloads/STAT 440/Module 1/baseline_lvl_3_lambda-min.txt", sep = ",",
            row.names = FALSE)
