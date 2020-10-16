# remove all variables from the workspace
rm(list = ls(all = TRUE))
setwd("/Users/jerem/Downloads/STAT 440/Module 1/stat440module1-master/data")
#load cleaned train and test data
train <- read.csv("level2.csv", sep=",")


test <- read.csv("test_level2.csv", sep=",")

#change the value from T/F to 0 and 1
train[,11:40] <- (train[,11:40])*1
test[,11:40] <- (test[,11:40])*1

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

#impute missing factors values (date) in train data
train.complete.num$confirmed = as.factor(train.complete.num$confirmed)
test.complete$confirmed = as.factor(test.complete$confirmed)

temp.fac.train <- mice(train.complete.num,m=1,maxit=50,meth='polyreg',seed=500)
train.complete<- complete(temp.fac.train,1)
apply(train.complete,2,pMiss)
View(train.complete)


#LASSO
y.1.cv <- train.complete$duration
x.1.cv <- data.matrix(train.complete[,c(-9,-1,-2)])
y.2.cv <- test.complete$duration
x.2.cv <- data.matrix(test.complete[,c(-9,-1,-2)])
cv.lasso.2 <- cv.glmnet(y=y.1.cv, x= x.1.cv, family="gaussian")
cv.lasso.2
coef(cv.lasso.2, s=cv.lasso.2$lambda.min)
coef(cv.lasso.2, s=cv.lasso.2$lambda.1se)
#Use lambda-min for prediction since lambda-1SE model is not complex enough
# Plot CV-MSPE
plot(cv.lasso.2, s=cv.lasso.2$lambda.min)
#Prediciton
pred.las1.min <- predict(cv.lasso.2, newx=x.2.cv, s=cv.lasso.2$lambda.min)

baseline = as.data.frame(cbind(test$X, pred.las1.min))
colnames(baseline)[1:2] <-c("Id","duration")
baseline
write.table(baseline, file = "/Users/jerem/Downloads/STAT 440/Module 1/baseline_lvl_2_lambda-min.txt", sep = ",",
            row.names = FALSE)
