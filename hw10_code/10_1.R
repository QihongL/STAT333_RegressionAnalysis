# HW10 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw10_code')
rm(list = ls());
library(car);library(perturb)
mydata = read.table('CH06PR18.txt', header = F, col.names = c('rentalRate_Y','age_X1','expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)

####################
# ANALYSIS
####################
# scatter plots
plot(mydata, pch = 16)

# linear model
lm.fit = lm(rentalRate_Y ~ age_X1 + expenses_X2 + vacancy_X3 + sqrFootage_X4, data = mydata)
n = dim(mydata)[1]
p = dim(mydata)[2] + 1

# hw 10.1 - added variable plots
avPlots(lm.fit, pch = 16)

# hw 10.2 - outlier identification 

# a) studentized deleted residuals
press = rstudent(lm.fit)
press
tcrit = qt(1 - 0.5 * (0.01 / (2 * length(mydata$rentalRate_Y))), n - p - 1 )
tcrit
plot(press ~ lm.fit$fitted.values, ylim = c(-5,5), pch = 16, 
     xlab = 'Fitted values', ylab = 'studentized deleted residuals')
abline(h = 0)
abline(h = tcrit, lty = 2)
abline(h = - tcrit, lty = 2)


# b)
Hii = hatvalues(lm.fit)
Hii
par(mfrow = c(1,1))
plot(Hii, rsstnd, pch = 16)

par(mfrow = c(2,2))
plot(lm.fit, pch = 16)

# d) 
influence.measures(lm.fit)

