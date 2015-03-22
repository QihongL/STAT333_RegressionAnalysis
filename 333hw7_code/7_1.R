# HW7 - 1 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333hw7_code')
rm(list = ls())
library(plyr)

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
pairs(mydata, pch = 16, main = 'Scatter Plot Matrix')
cor(mydata)


# c) fit regression model, state the function 
lm.fit = lm(mydata$rentalRate_Y ~ mydata$age_X1
            + mydata$expenses_X2 + mydata$vacancy_X3 + mydata$sqrFootage_X4)

summary(lm.fit)
anova(lm.fit)

# d) box plot for residuals 
boxplot(lm.fit$residuals, main = 'Residuals')

# e) residuals plot 
# residual against Y
plot(lm.fit$residuals ~ mydata$age_X1, pch = 20)
abline(mean(lm.fit$residuals), 0)

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
mtext("Residuals Against Each Predictors", outer = TRUE, cex = 1.5)

# residual against each 2factors interaction term
combn(4,2)


# f) lack of fit test 
# might be impossible since some levels of X2,X3,X4 have no replications

# g) levene test

# add resid and fitted to the data frame
resid = lm.fit$residuals
fitted = lm.fit$fitted.values
mydata = cbind(mydata, fitted, resid)
# rank the data w.p. to fitted values
mydata = arrange(mydata, desc(mydata$fitted))
group = c(rep('A',40), rep('B', 41))
mydata = cbind(mydata,group)

leveneTest(mydata$resid ~ mydata$group)
