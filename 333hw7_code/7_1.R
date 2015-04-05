# HW7 - 1 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333hw7_code')
rm(list = ls())
library(plyr)
library(ggplot2)
library(car)
mydata = read.table('CH06PR18.txt', header = F, 
            col.names = c('rentalRate_Y','age_X1',
                          'expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)

# a) stem plot for all regressors
stem(mydata$age_X1)
stem(mydata$expenses_X2)
stem(mydata$vacancy_X3)
stem(mydata$sqrFootage_X4)

# b) scatter plot matrix & r matrix 
# pairs(mydata, pch = 16, main = 'Scatter Plot Matrix')
# cor(mydata)
ggpairs(mydata, title="scatter plot matrix and the correlation matrix")


# c) fit regression model, state the function 
lm.fit = lm(mydata$rentalRate_Y ~ mydata$age_X1
            + mydata$expenses_X2 + mydata$vacancy_X3 + mydata$sqrFootage_X4)

summary(lm.fit)
anova(lm.fit)

# d) box plot for residuals 
boxplot(lm.fit$residuals, main = 'Residuals')

# e) residuals plot 
# residual against Y
par(mfrow = c(1,2))
plot(lm.fit$residuals ~ lm.fit$fitted.values, pch = 20, 
     main = 'Residuals against fitted values')
abline(mean(lm.fit$residuals), 0)

qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
# residual against Xs
par(mfrow = c(2,2))
plot(lm.fit$residuals ~ mydata$age_X1, pch = 20)
abline(mean(lm.fit$residuals), 0)
plot(lm.fit$residuals ~ mydata$expenses_X2, pch = 20)
abline(mean(lm.fit$residuals), 0)
plot(lm.fit$residuals ~ mydata$vacancy_X3, pch = 20)
abline(mean(lm.fit$residuals), 0)
plot(lm.fit$residuals ~ mydata$sqrFootage_X4, pch = 20)
abline(mean(lm.fit$residuals), 0)
mtext("Residuals against all predictors", side = 3, line = -2, outer = TRUE)

# residual against each 2factors interaction term
# all possible 2-way combinations from 4 choices 
combn(4,2)
# start plotting... 
par(mfrow = c(3,2))
interactionX1_X2 = mydata$age_X1 * mydata$expenses_X2
plot(lm.fit$residuals ~ interactionX1_X2 , pch = 20)
abline(mean(lm.fit$residuals), 0)
interactionX1_X3 = mydata$age_X1 * mydata$vacancy_X3
plot(lm.fit$residuals ~ interactionX1_X3 , pch = 20)
abline(mean(lm.fit$residuals), 0)
interactionX1_X4 = mydata$age_X1 * mydata$sqrFootage_X4
plot(lm.fit$residuals ~ interactionX1_X4 , pch = 20)
abline(mean(lm.fit$residuals), 0)
interactionX2_X3 = mydata$expenses_X2 * mydata$vacancy_X3
plot(lm.fit$residuals ~ interactionX2_X3 , pch = 20)
abline(mean(lm.fit$residuals), 0)
interactionX2_X4 = mydata$expenses_X2 * mydata$sqrFootage_X4
plot(lm.fit$residuals ~ interactionX2_X4 , pch = 20)
abline(mean(lm.fit$residuals), 0)
interactionX3_X4 = mydata$vacancy_X3 * mydata$sqrFootage_X4
plot(lm.fit$residuals ~ interactionX3_X4 , pch = 20)
abline(mean(lm.fit$residuals), 0)
mtext("Residuals against all interaction terms", side = 3, line = -2, outer = TRUE)







# f) lack of fit test 
# might be impossible since some levels of X2,X3,X4 have no replications

# g) levene test
# add resid and fitted to the data frame
resid = lm.fit$residuals
fitted = lm.fit$fitted.values
mydata = cbind(mydata, fitted, resid)
# rank the data w.p. to fitted values
mydata = arrange(mydata, desc(mydata$fitted))
group = c(rep('A',41), rep('B', 40))
mydata = cbind(mydata,group)

leveneTest(mydata$resid ~ mydata$group)
qf(0.95,1,79)


