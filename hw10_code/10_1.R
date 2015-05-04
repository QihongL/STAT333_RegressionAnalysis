# HW10 - 1 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw10_code')
rm(list = ls());
library(car);library(perturb)
mydata = read.table('CH06PR18.txt', header = F, col.names = c('rentalRate_Y','age_X1','expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)

