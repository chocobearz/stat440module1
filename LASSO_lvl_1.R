# remove all variables from the workspace
rm(list = ls(all = TRUE))
setwd("/Users/jerem/Downloads/STAT 440/Module 1/stat440module1-master/data")
#load cleaned train and test data
train <- read.csv("level1.csv", sep=",")


test <- read.csv("test_level1.csv", sep=",")

#change the value from T/F to 0 and 1
train[,11:44] <- (train[,11:44])*1
test[,11:44] <- (test[,11:44])*1

#Library
library(mice) #for missing values
library(caret) #for dummyVars
library(glmnet) #for LASSO
library(MASS) # for ridge
library(pls)# for PLS

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
pred.las1.min <- predict(cv.lasso.2, newx=x.2.cv, s=cv.lasso.2$lambda.min)
pred.las1.1se <- predict(cv.lasso.2, newx=x.2.cv, s=cv.lasso.2$lambda.1se)

baseline = as.data.frame(cbind(test$X, pred.las1.min))
colnames(baseline)[1:2] <-c("Id","duration")
baseline
write.table(baseline, file = "/Users/jerem/Downloads/STAT 440/Module 1/baseline_lvl_1_lambda-min.txt", sep = ",",
            row.names = FALSE)

#Hybrid stepwise
n=nrow(train.complete)
initial.s.cv <- lm(data=train.complete[,c(-1,-2)], formula=duration ~ 1)
final.s.cv <- lm(data=train.complete, formula=duration~.)
step.s.cv <- step(object=initial.s.cv, scope=list(upper=final.s.cv), k = log(n))
pred.s.cv = predict(step.s.cv, newdata=test.complete[,c(-1,-2)])

baseline.step = as.data.frame(cbind(test$X, pred.s.cv))
colnames(baseline.step)[1:2] <-c("Id","duration")
baseline.step
write.table(baseline.step, file = "/Users/jerem/Downloads/STAT 440/Module 1/baseline_lvl_1_step.txt", sep = ",",
            row.names = FALSE)

#PLS
mod.pls.cv = plsr(duration~., data=train.complete[,c(-1,-2)], ncomp=5, validation="CV")
#a. Optimal number of components (reported in part c.)
mp.cv = mod.pls.cv$validation
Opt.Comps = which.min(sqrt(mp.cv$PRESS/nrow(train.complete)))
#b. Prediction
pred.pls.cv = predict(mod.pls.cv, ncomp=Opt.Comps, newdata=test.complete[,c(-1,-2)])

baseline.pls = as.data.frame(cbind(test$X, pred.pls.cv))
colnames(baseline.pls)[1:2] <-c("Id","duration")
baseline.pls
write.table(baseline.pls, file = "/Users/jerem/Downloads/STAT 440/Module 1/baseline_lvl_1_pls.txt", sep = ",",
            row.names = FALSE)