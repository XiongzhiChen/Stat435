# Logistic Regression
# you may need to change this line when loading data
load("C:/MathRepo/stat435/PenalizedLogisticExample.RData")
dim(xMat)
length(y)
# 100 observations; 50 predictors
simData = data.frame(cbind(y,xMat))
glm.fits=glm(y~.,family=binomial,data=simData)
# we can also do; the results are the same
# glm.fits=glm(y~xMat,family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
# obtain predicted probabilities
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
# apply threshold 0.5 to obtain predicted class labels
glm.pred=rep(0,100)
glm.pred[glm.probs>.5]=1
table(glm.pred,y)

# To apply LASSO to logistic regression
library(glmnet)
load("C:/MathRepo/stat435/PenalizedLogisticExample.RData")
set.seed(1)
# 10-fold cross-validation to determine optimal lambda based on training set;
# 70% observations as training set
cvfit = cv.glmnet(xMat[1:70,], y[1:70], family = "binomial", type.measure = "class")
# The default in "cv.glmnet" is type.measure="deviance", applicable to generalized linear models; 
# type.measure="class" applies to binomial and multinomial logistic regression only, and gives misclassification error. 
# type.measure="auc" is for two-class logistic regression only, and gives area under the ROC curve 
plot(cvfit)
cvfit$lambda.min
# estimated coefficients at optimal lambda
coefEst = coef(cvfit, s = "lambda.min")
coefEst = as.vector(coefEst)
# show nonzero estimated coefficients
coefEst[coefEst!=0]
# obtain predicted probabilities on test data
ProbPrd = predict(cvfit, newx = xMat[71:100,], s = "lambda.min", type = "response")
as.vector(ProbPrd)
# apply threshold 0.5 to obtain predicted class labels
PredClass = as.numeric(ProbPrd >0.5)
table(PredClass,y[71:100])
## try predicted class labels directly
ClassPrd = predict(cvfit, newx = xMat[71:100,], s = "lambda.min", type = "class")
table(as.vector(ClassPrd),y[71:100])
# inference for lasso logistic regression
library(hdi)
outRes = lasso.proj(xMat, y, family = "binomial", standardize = TRUE,
                    multiplecorr.method = "none")

outRes$pval

# additional information for LASSO logistic regression at
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

# for ridge logistic regression, we can use
cv.ridge.logistic = cv.glmnet(xMat[1:70,], y[1:70], alpha=0, family = "binomial", type.measure = "class")
cv.ridge.logistic$lambda.min
plot(cv.ridge.logistic)
out.ridge.Res = ridge.proj(xMat, y, family = "binomial", standardize = TRUE,
                    multiplecorr.method = "none")
