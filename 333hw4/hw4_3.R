# HW4 - Question 3 - textbook 3.3 
setwd('/Users/Qihong/Dropbox/Current Courses/STAT333/333hw4')
library(car)
library(plyr)
rm(list = ls())
par(mfrow=c(1,1))

mydata = read.table("CH01PR19.txt", header = F)
colnames(mydata)[1] = "GPA"
colnames(mydata)[2] = "ACT"

# fit the regression model
lm.fit = lm(GPA~ACT, data = mydata)
# plot the regressione line 
plot(mydata$ACT, mydata$GPA, pch = 20)
# stripchart(mydata$ACT)
abline(lm.fit)

# a) visualization 
boxplot(mydata$ACT)
title("ACT scores")


# b) Dot residual plot 
plot(mydata$ACT, lm.fit$residuals, pch = 20, 
     main=expression('Residual against X'[i]))
abline(mean(lm.fit$residuals), 0)
plot(lm.fit$residuals, pch = 20,  main=expression('Residual dot plot'))
abline(mean(lm.fit$residuals), 0)


# c) Residual plot again fitted values
plot(lm.fit$fitted.values, lm.fit$residuals, pch = 20, 
     main=expression('Residual against predicted values of Y'))
abline(mean(lm.fit$residuals), 0)
# d) 
# 1) Normal probability plot 
residualStandard = rstandard(lm.fit)
qqnorm(residualStandard)
qqline(residualStandard)

# 2) r between ordered residual and expected values 
n = length(lm.fit$residuals)
sorttedResiduals = sort(lm.fit$residuals)
# check if I can replicate the qq plot -- No? Why? 
MSE = sum((lm.fit$residuals - mean(lm.fit$residuals))^2) / (n - 2)
myResidualStandard = (sorttedResiduals - mean(lm.fit$residuals)) / sqrt(MSE)
plot(myResidualStandard)

# E[kth smallest residual]
indices = 1:n
zValue = qnorm((indices - 0.375) / (n + 0.25))
expectedRes = sqrt(MSE) * zValue 
#plot(expectedRes, main = 'expected residual under normality')
cor(expectedRes, sort(lm.fit$residuals))


# 4) Brown-Forsythe test
X1 = mydata$ACT[which(mydata$ACT< 26)]
X2 = mydata$ACT[which(mydata$ACT>= 26)]
residual1 = lm.fit$residuals[which(mydata$ACT< 26)]
residual2 = lm.fit$residuals[which(mydata$ACT>= 26)]
n1 = length(X1)
n2 = length(X2)
deviation1 = abs(residual1 - median(residual1))
deviation2 = abs(residual2 - median(residual2))
pooledVar = (sum(deviation1^2) + sum(deviation2^2)) / (n1 + n2 - 2)
tBF = (mean(deviation1) - mean(deviation2)) / (sqrt(pooledVar) * sqrt((1/n1) + (1/n2)))
tBF
qt(0.99, n1+n2-2)



# levene test 
resids = residuals(lm.fit)
fits = fitted.values(lm.fit)
mydata$group = as.factor(recode(mydata$ACT, "-Inf:25 = 'A'; else = 'B'"))
mydata = as.data.frame(cbind(mydata,fits,resids))
mydata = arrange(mydata,desc(-mydata$ACT))
leveneTest(resids ~ group, data = mydata)
qf(0.99,1,118)




mydata = read.table("CH03PR03.txt", header = F)
colnames(mydata)[1] = "GPA"
colnames(mydata)[2] = "ACT"
colnames(mydata)[3] = "IQ"
colnames(mydata)[4] = "percentile"

head(mydata)
par(mfrow = c(1,2))
lm.fit2 = lm(mydata$GPA ~ mydata$ACT + mydata$IQ)
plot(lm.fit2$residuals ~ mydata$IQ, pch = 20, main = "Residuals against IQ \n Model: GPA ~ ACT & IQ")
abline(mean(lm.fit$residuals), 0)
lm.fit3 = lm(mydata$GPA ~ mydata$ACT + mydata$percentile)
plot(lm.fit3$residuals ~ mydata$percentile, pch = 20, main = "Residuals against percentile\n Model: GPA ~ ACT & percentile")
abline(mean(lm.fit$residuals), 0)
