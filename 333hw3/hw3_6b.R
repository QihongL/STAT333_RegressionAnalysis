setwd("/Users/Qihong/Dropbox/Current Courses/STAT333/333hw3")
rm(list = ls())

mydata1 = read.csv('Data1_large.csv');
mydata2 = read.csv('Data2_large.csv');

# fit the model 
lm1.fit = lm(Y ~ X, data = mydata1)
lm2.fit = lm(Y ~ X, data = mydata2)

# visualization 
xmin = min(mydata1$X, mydata2$X)
xmax = max(mydata1$X, mydata2$X)
ymin = min(mydata1$Y, mydata2$Y)
ymax = max(mydata1$Y, mydata2$Y)
par(mfrow=c(1,2))
# 1st plot 
plot(mydata1$X, mydata1$Y, pch = 20, xlim=c(xmin, xmax), ylim = c(ymin, ymax))
abline(lm1.fit)
nd <- data.frame(x=seq(xmin,xmax,length=500))
p_conf1 <- predict(lm1.fit,interval="confidence",newdata=mydata1)
p_pred1 <- predict(lm1.fit,interval="prediction",newdata=mydata1)
matlines(mydata1$X,p_conf1[,c("lwr","upr")],col=4,lty=2,type="l")
matlines(mydata1$X,p_pred1[,c("lwr","upr")],col=2,lty=2,type="l")

# 2nd plot 
plot(mydata2$X, mydata2$Y, pch = 20, xlim=c(xmin, xmax), ylim = c(ymin, ymax))
abline(lm2.fit)
nd <- data.frame(x=seq(xmin,xmax,length=500))
p_conf2 <- predict(lm2.fit,interval="confidence",newdata=mydata2)
p_pred2 <- predict(lm2.fit,interval="prediction",newdata=mydata2)
matlines(mydata2$X,p_conf2[,c("lwr","upr")],col=4,lty=2,type="l")
matlines(mydata2$X,p_pred2[,c("lwr","upr")],col=2,lty=2,type="l")


# compare stats 
summary(lm1.fit)
summary(lm2.fit)

anova(lm1.fit)
anova(lm2.fit)


# compare confidence intervals 
Xnew = data.frame(X = 15)
predict(lm1.fit, Xnew, interval = "confidence", level = 0.95, se.fit = T)
predict(lm2.fit, Xnew, interval = "confidence", level = 0.95, se.fit = T)
# compare prediction intervals 
predict(lm1.fit, Xnew, interval = "prediction", level = 0.95, se.fit = T)
predict(lm2.fit, Xnew, interval = "prediction", level = 0.95, se.fit = T)


anova1 = as.data.frame(anova(lm1.fit))
anova2 = as.data.frame(anova(lm2.fit))
# 
n = 160
Xh = 15
SSxx1 = sum(mydata1$X^2) - sum(mydata1$X)^2/n
MSE1 = anova1$"Mean Sq"[2]
SE_CI1 = sqrt(MSE1 * ( (1/n) + (Xh - mean(mydata1$X))/SSxx1 ))
SE_Pr1 = sqrt(MSE1 * ( 1 + (1/n) + (Xh - mean(mydata1$X))/SSxx1 ))
SE_CI1
SE_Pr1

SSxx2 = sum(mydata2$X^2) - sum(mydata2$X)^2/n
MSE2 = anova2$"Mean Sq"[2]
SE_CI2 = sqrt(MSE2 * ( (1/n) + (Xh - mean(mydata2$X))/SSxx2 ))
SE_Pr2 = sqrt(MSE2 * ( 1 + (1/n) + (Xh - mean(mydata2$X))/SSxx2 ))
SE_CI2
SE_Pr2


