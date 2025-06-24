adData = read.csv("Advertising.csv",header = T,na.strings = " ")
Fit1 = lm(sales~TV,data=adData)
library(broom)
tidy(Fit1)
#library(ggplot2)
#ggplot(adData, aes(TV, sales)) +geom_point() + theme_bw() + geom_smooth(method = "lm", se = FALSE)


AD1 = read.table("Auto.data",header = T,na.strings = "?")
Fit2 = lm(mpg~horsepower,data=AD1)
library(broom)
tidy(Fit2)


# Six plots (selectable by which) are currently available: a plot of residuals against fitted values, a Scale-Location plot of sqrt(| residuals |) against fitted values, a Normal Q-Q plot, a plot of Cook's distances versus row labels, a plot of residuals against leverages, and a plot of Cook's distances against leverage/(1-leverage). By default, the first three and 5 are provided.
# which = c(1,2,3,5)
plot(Fit1,which = c(1))


plot(Fit2,which = c(1))


## Heterogeneous error variances

library(ggplot2)
ggplot(Fit1, aes(Fit1$fitted.values,Fit1$residuals)) + geom_point() + geom_smooth(method='loess')+theme_bw()+
  xlab("Fitted values")+ylab("Residuals")



library(ggplot2)
ggplot(Fit2, aes(Fit2$fitted.values,Fit2$residuals)) + geom_point() + geom_smooth(method='loess')+theme_bw()+
  xlab("Fitted values")+ylab("Residuals")


# outliers

plot(Fit1,which = c(3))

plot(Fit1$fitted.values,rstudent(Fit1),ylab="Studentized residuals",xlab="Fitted values")


plot(Fit2,which = c(3))


plot(Fit2$fitted.values,rstudent(Fit2),ylab="Studentized residuals",xlab="Fitted values")


## High-leverage points


plot(Fit1,which = c(5))


plot(Fit2,which = c(5))


## Q-Q Plot


qqnorm(Fit1$residuals); qqline(Fit1$residuals, col = 2, lwd=2)

## Test on Normality

qqnorm(Fit2$residuals); qqline(Fit2$residuals, col = 2, lwd=2)

## Kolmogorov-Smirnov Test

ks.test(Fit1$residuals, "pnorm",mean(Fit1$residuals),sd(Fit1$residuals))

ks.test(Fit2$residuals, "pnorm", mean(Fit2$residuals),sd(Fit2$residuals))

## Correlation of error terms

set.seed(3.14)
# generate equally correlated random errors
x0 = rnorm(1); n =1000
errvecA = rnorm(n); r=0.3
errvec = sqrt(1-r)*errvecA + sqrt(r)*x0
# generate observations
xvec = rnorm(n)
yvec = 1+2*xvec+errvec
fitA = lm(yvec~xvec)
par(mfrow=c(1,2))
plot(1:n,fitA$residuals)
plot(fitA$fitted.values,fitA$residuals)



