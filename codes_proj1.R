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

# inference for lasso regression
library(ISLR)
library(hdi)
library(dplyr)
xModel = model.matrix(Balance~.,data=Credit)
xModela = as.data.frame(xModel)
xsub = xModela %>% select(Income, Limit, Rating, StudentYes)
outRes = lasso.proj(xsub, Credit$Balance, family = "gaussian", standardize = TRUE,
                    multiplecorr.method = "none")
outRes$pval
