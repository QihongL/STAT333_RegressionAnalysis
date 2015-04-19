# HW8 - 1 - Commercial properties - From Prof.Packard 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw8_code')
library(car)
rm(list = ls())
mydata = read.table('CH06PR18.txt', header = F, 
            col.names = c('rentalRate_Y','age_X1',
            'expenses_X2','vacancy_X3','sqrFootage_X4'))

n = dim(mydata)[1]  # = 81
p = 4

# fit the model 
lm.fit_1234 = lm(mydata$rentalRate_Y ~ mydata$age_X1 + mydata$expenses_X2
                 + mydata$vacancy_X3 + mydata$sqrFootage_X4)

# 8.1.1 construct test matrix 
C = rbind (c(0, 0, 0, 0, 1), c(0, 1, -1, 0, 0), c(0, 0, 1, -1, 0))
d = c(0, 0, 0)

# 8.1.2 hypothesis test
linearHypothesis(lm.fit_1234, hypothesis.matrix = C, rhs = d)

qf(0.95,81,77)

# 8.1.3 W = X1 + X2 + X3
W = mydata$age_X1 + mydata$expenses_X2 +mydata$vacancy_X3
lm.fit_w4 = lm(mydata$rentalRate_Y ~ W)
summary(lm.fit_w4)

# general linear test 
anova(lm.fit_w4, lm.fit_1234)

qf(0.95, 3, 77)
