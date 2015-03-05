# HW4 - Question 4 - textbook 3.15 
setwd('/Users/Qihong/Dropbox/Current Courses/STAT333/333hw4')
rm(list = ls())
par(mfrow = c(1,1))
mydata = read.table('CH03PR15.csv', header = F)
colnames(mydata)[1] = "Concentration_Y"
colnames(mydata)[2] = "Time_X"

# a) fit the model 
lm.fit = lm(mydata$Concentration_Y ~ mydata$Time_X) 
plot(mydata$Concentration_Y ~ mydata$Time_X, pch = 20)
title("Linear regression fit")
abline(lm.fit)

#) residual against X
plot(lm.fit$residuals ~ Time_X, data = mydata, pch = 20)
title('residual against X')
abline(mean(lm.fit$residuals), 0)

# b) F test of lack of fit, a = .025 
lm.fit2 = lm(mydata$Concentration_Y ~ factor(mydata$Time_X))
anova(lm.fit2)
anova(lm.fit, lm.fit2, test = 'F')
qf(0.975, 3, 10)
          
# c) what regression function is more appropriate? 

