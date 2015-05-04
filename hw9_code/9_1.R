# HW9 - 1 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw9_code')
rm(list = ls());
library(plyr);library(car);library(perturb)
mydata = read.table('CH06PR18.txt', header = F, col.names = c('rentalRate_Y','age_X1','expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)

# scatter and cor
plot(mydata, pch = 16)
cor(mydata)

lm.fit = lm(rentalRate_Y ~ age_X1 + expenses_X2 + vacancy_X3 + sqrFootage_X4, data = mydata)
vif(lm.fit)
colldiag(lm.fit)
