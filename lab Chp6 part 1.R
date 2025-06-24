
# Chapter 6 Lab 2a: Ridge Regression

library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

xtmp=model.matrix(Salary~.,Hitters)
xtmp[1:2,]

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
# check norm of regression coefficients
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
# obtain ridge regression coefficients for lamba=50
50%in%grid
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# estimate test error
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

#test mean squared error of fitted model when lambda=4
4%in%grid
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
#test mean squared error of the simple model
mean((mean(y[train])-y.test)^2)
# lambda very large forces regression coefficient estimates to be very small
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# test mean squared error of the least squares model
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)
# compare coefficients
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]
# cross-validation; k=10 by default
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# estimate mean squared test error for model based on the best lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# ridge regression based on the best lambda and fitted on full date set
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# inference on ridge regression

library(ISLR)
library(hdi)
library(dplyr)
xModel = model.matrix(Balance~.,data=Credit)
xModela = as.data.frame(xModel)
xsub = xModela %>% select(Income, Limit, Rating, StudentYes)
outRes = ridge.proj(xsub, Credit$Balance, family = "gaussian", standardize = TRUE,
                    multiplecorr.method = "none")

outRes$pval


