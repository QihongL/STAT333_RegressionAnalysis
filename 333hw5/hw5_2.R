# HW5 - Question 2 - GPA data
setwd('/Users/Qihong/Dropbox/Current Courses/STAT333/333hw5')
rm(list = ls())
mydata = read.table('CH01PR19.txt', header = F)
colnames(mydata)[1] = "GPA"
colnames(mydata)[2] = "ACT"
par(mfrow = c(1,1))

# fit the regression model
lm.fit = lm(GPA~ACT, data = mydata)
# plot the regressione line 
# plot(mydata$ACT, mydata$GPA, pch = 20, main = expression('Regression function'))
# # stripchart(mydata$ACT)
# abline(lm.fit)

# a) Bonferroni 95% joint confidence interval for b0 b1
confint(lm.fit, level = 0.975)
# b) plot the confidence ellipses for b0, b1, at 90%, 95%, 99%

ce1 = confidenceEllipse(lm.fit, col = 'black', levels = 0.99, 
                  main = expression('90%, 95%, 99% confidence ellipse'))
ce2 = confidenceEllipse(lm.fit, col = 'blue', levels = 0.95, add = T)
ce3 = confidenceEllipse(lm.fit, col = 'red',levels = 0.90, add = T)

