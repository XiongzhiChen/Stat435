library(ISLR)
library(boot)

dim(Portfolio)
plot(Portfolio$X,Portfolio$Y)

# The Bootstrap

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  estAlpha = (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
  return(estAlpha)
}
alpha.fn(Portfolio,1:100) # alpha hat

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) # replace=TRUE
boot(Portfolio,alpha.fn,R=1000)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) 
tmp = boot(Portfolio,alpha.fn,R=1000)
  
# extra and work on bootstrap estimates
hist(tmp$t)
mean(tmp$t)
sd(tmp$t)
mean(tmp$t)-tmp$t0

# quantile function
quantile(tmp$t,probs=c(0.025,0.975), names = FALSE)

# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index) {
   LMObj = lm(mpg~horsepower,data=data,subset=index)
   EstCoef = coef(LMObj)
  return(EstCoef)
}

boot.fn(Auto,1:392)  

set.seed(-3.14)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

## quadratic model
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
tmp1 = boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

