# HW5 - Question 2 - GPA data
setwd('/Users/Qihong/Dropbox/Current Courses/STAT333/333hw5')
rm(list = ls())
mydata = read.table('CH01PR19.txt', header = F)
colnames(mydata)[1] = "GPA"
colnames(mydata)[2] = "ACT"
par(mfrow = c(2,2))

# fit the regression model
lm.fit = lm(GPA~ACT, data = mydata)
# plot the regressione line 
plot(mydata$ACT, mydata$GPA, pch = 20, main = expression('Regression function'))
abline(lm.fit)

# regression thourough origin
lm.fit_o = lm(GPA~ACT - 1 , data = mydata)
plot(mydata$ACT, mydata$GPA, pch = 20, main = expression('Regression through origin'))
abline(lm.fit_o)


plot(lm.fit$residuals ~ lm.fit$fitted.values, pch = 20, main = expression('Residuals'))
abline(mean(lm.fit$residuals), 0)
plot(lm.fit_o$residuals ~ lm.fit_o$fitted.values, pch = 20, 
     main = expression('Residuals\nregression through origin'))
abline(mean(lm.fit_o$residuals), 0)


# C) lack of fit test for regression through origin model 

lm.fit_f = lm(GPA~factor(ACT) -1, data = mydata)
anova(lm.fit_f)
anova(lm.fit_o, lm.fit_f, test = 'F')

# F test 
alpha = 0.005
c = dim(table(mydata$ACT)) - 2
df_r = dim(mydata)[1] - c
qf(0.995, c, df_r)
