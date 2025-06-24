
# # Chapter 6 Lab 2b: The Lasso
library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# create training and test sets

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]


# test mean squared error of the least squares model
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)

# lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

# prediction based on best model
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# variable selection via lasso
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# inference for lasso regression based on full data set
library(hdi)
outRes = lasso.proj(x, y, family = "gaussian", standardize = TRUE,
                    multiplecorr.method = "none")
outRes$pval

which(outRes$pval < 0.05)
length(which(outRes$pval < 0.05))
outRes$pval[which(outRes$pval < 0.05)]

# inference for lasso regression based on training data set
outResA = lasso.proj(x[train,], y[train], family = "gaussian", standardize = TRUE,
                     multiplecorr.method = "none")
outResA$pval
which(outResA$pval < 0.05)
length(which(outResA$pval < 0.05))

outResA$pval[which(outResA$pval < 0.05)]

###### try lasso on Credit data set
# create model metrix
mod.mat = model.matrix(Balance~Income+Limit+Rating+Student,Credit)[,-1]
bal.credit = Credit$Balance

# run lasso
lasso.credit=glmnet(mod.mat,bal.credit,alpha=1,lambda=grid)
plot(lasso.mod)

# cv to get optimal lambda
set.seed(1)
cv.out.credit=cv.glmnet(mod.mat,bal.credit,alpha=1)
plot(cv.out.credit)
bestlam.credit=cv.out.credit$lambda.min

# estimated coefficients at optimal lambda
lasso.coef.credit=predict(lasso.credit,type="coefficients",s=bestlam.credit)
lasso.coef.credit

# testing on each null hypothesis
library(hdi)
outRes.credit = lasso.proj(mod.mat,bal.credit, family = "gaussian", standardize = TRUE,
                    multiplecorr.method = "none")
outRes.credit$pval



