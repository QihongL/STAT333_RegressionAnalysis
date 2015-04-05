# HW7.345 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333hw7_code')
rm(list = ls())

mydata = read.table('CH06PR18.txt', header = F, 
                    col.names = c('rentalRate_Y','age_X1',
                                  'expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)


################################################
# Q3 - textbook 7.7 - modeling order 4123
################################################
lm.fit_4123 = lm(mydata$rentalRate_Y ~ mydata$sqrFootage_X4
                + mydata$age_X1 + mydata$expenses_X2 + mydata$vacancy_X3)

lm.fit_1243 = lm(mydata$rentalRate_Y ~ 
                 + mydata$age_X1 + mydata$expenses_X2 + mydata$sqrFootage_X4 + mydata$vacancy_X3)

# partial T test 
summary(lm.fit_4123)
# partial F
# Anova(lm.fit_4123, type = 'III')
# sequential ss with order: 4123
anova(lm.fit_4123)

anova(lm.fit_1243)



################################################
# Q4 - textbook 7.8 - test whether b2 = b3 = 0 given b1, b4
################################################

lm.fit_1234 = lm(mydata$rentalRate_Y ~ mydata$age_X1 + mydata$expenses_X2
                 + mydata$vacancy_X3 + mydata$sqrFootage_X4)
# construct test matrix 
C = rbind (c(0, 0, 1, 0, 0), c(0, 0, 0, 1, 0))
d = c(0,0)
# test
linearHypothesis(lm.fit_1234, hypothesis.matrix = C, rhs = d)
n = dim(mydata)[1]  # = 81
qf(0.99, n-3, n -5) # critical value

################################################
# Q5 - textbook 7.10 - test whether b1 = -1. b2 = 0
################################################
# construct test matrix 
C = rbind (c(0, 1, 0, 0, 0), c(0, 0, 1, 0, 0))
d = c(-0.1,0.4)
# test
linearHypothesis(lm.fit_1234, hypothesis.matrix = C, rhs = d)
qf(0.99, n-3, n -5) # critical value

