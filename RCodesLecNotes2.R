adData = read.csv("Advertising.csv",header = T,na.strings = " ")
library(ggplot2)
p1=ggplot(adData)+geom_point(aes(TV,sales))+theme_bw()
p2=ggplot(adData)+geom_point(aes(newspaper,sales))+theme_bw()
p3=ggplot(adData)+geom_point(aes(radio,sales))+theme_bw()
library(gridExtra)
grid.arrange(p1,p2,p3,nrow=1)


creditData = read.csv("Credit.csv",header=T,na.strings = " ")
library(ggplot2)
p4=ggplot(creditData)+geom_boxplot(aes(Gender,Balance))+theme_bw()
p5=ggplot(creditData)+geom_boxplot(aes(Married,Balance))+theme_bw()
library(gridExtra)
grid.arrange(p4,p5,nrow=1)


library(ggplot2)
p6=ggplot(creditData)+geom_point(aes(Age,Balance))+theme_bw()
p7=ggplot(creditData)+geom_point(aes(Income,Balance))+facet_grid(.~Gender)+theme_bw()
library(gridExtra)
grid.arrange(p6,p7,nrow=1)


# an estimated model

itp=5; slp=0.05
yFit = itp+slp*adData$TV
p1a=ggplot(adData, aes(TV, sales)) +   geom_point() + theme_bw() + 
  geom_abline(intercept=5,slope=slp,size=1.5,color="blue") +
  geom_segment(aes(xend = TV, yend = yFit), alpha = .2)   # alpha to fade lines
p1a


# optimal model

ModFit <- lm(sales~TV, data = adData)
adData$fitted <- fitted(ModFit)   # Save the predicted values
p2a=ggplot(adData, aes(TV, sales)) +   geom_point() + theme_bw() + geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = TV, yend = fitted), alpha = .2)   # alpha to fade lines
p2a

library(gridExtra)
grid.arrange(p1a,p2a,nrow=1)


