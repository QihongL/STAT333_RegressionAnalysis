# HW9 - 2 & 3 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw9_code')
rm(list = ls());
# library(plyr);library(ggplot2);library(car)
library(perturb)
mydata = read.table('CH10PR13.txt', col.names = c('Y','X1','X2','X3'))
mydataCopy = mydata


# scatter
plot(mydata, pch = 16)
# fit the model
lm.fit = lm(Y ~ X1 + X2 + X3, data = mydata)

# multicoli
vif(lm.fit)
colldiag(lm.fit)



##################
# ridge regression
##################
n = dim(mydata)[1]
# mydata = as.data.frame(scale(mydata / sqrt(n - 1)))
k = seq(1,10, 0.01)
lridge = lm.ridge(Y ~ X1 + X2 + X3, lambda = k, data = mydata)

coefs1 = print(lridge);

# trace plot
matplot(lridge$lambda, t(lridge$coef/coefs1$scales), type = 'l',
        lty = 1, xlab = expression(lamda), ylab = expression(hat(beta)))
legend('topright', names(mydata)[-1], lty = 1, col = 1:5, bty = 'n', cex = .7)

select(lridge)

k = 5.21
lridge = lm.ridge(Y ~ X1 + X2 + X3, lambda = k, data = mydata)
lridge
lm.fit



####################
# detransform 
####################
sy = sd(mydataCopy$Y)
sx1 = sd(mydataCopy$X1)
sx2 = sd(mydataCopy$X2)
sx3 = sd(mydataCopy$X3)

# get all betas
coefficients = as.data.frame(lridge$coef)
# get individual beta value
beta1Trans = coefficients[1,]
beta2Trans = coefficients[2,]
beta3Trans = coefficients[3,]

# detransform 
beta1 = beta1Trans * (sy / sx1);beta1
beta2 = beta2Trans * (sy / sx2);beta2
beta3 = beta3Trans * (sy / sx3);beta3


