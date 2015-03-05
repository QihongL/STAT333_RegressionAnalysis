# HW5 - Question 1 - textbook 3.18 
setwd('/Users/Qihong/Dropbox/Current Courses/STAT333/333hw5')
rm(list = ls())
library('car')
mydata = read.table('CH03PR18.txt', header = F)
colnames(mydata)[1] = "ProductionTimeHours_Y"
colnames(mydata)[2] = "ProductionLotSize_X"
head(mydata)

################################################
# compare raw regression and transformation 
################################################
par(mfrow = c(3,2))
# plot the data
plot(ProductionTimeHours_Y~ProductionLotSize_X, data = mydata, pch = 20,
     main =expression('Regression function: Y ~ X'))
# fit the model
lm.fit1 = lm(ProductionTimeHours_Y~ProductionLotSize_X, data = mydata)
abline(lm.fit1)

# plot the transformed data, transformation X' = sqrt(X)
Xtransformed = sqrt(mydata$ProductionLotSize_X)
plot(ProductionTimeHours_Y~Xtransformed, data = mydata, pch = 20, 
     main =expression('Regression function: Y ~ '~sqrt(X)))
# fit the model
lm.fit2 = lm(ProductionTimeHours_Y~Xtransformed, data = mydata)
abline(lm.fit2)



# residual against fitted values
plot(lm.fit1$residuals ~ lm.fit1$fitted.values, pch = 20,
     main = expression('Residual against fitted values: Y ~ X'))
abline(mean(lm.fit1$residuals), 0)
plot(lm.fit2$residuals ~ lm.fit2$fitted.values, pch = 20,
     main = expression('Residual against fitted values: Y ~ '~sqrt(X)))
abline(mean(lm.fit2$residuals), 0 )


# normal probability plot
residualStandard1 = rstandard(lm.fit1)
qqnorm(residualStandard1, main = expression('Normal Probability plot: Y ~ X'))
qqline(residualStandard1)

residualStandard2 = rstandard(lm.fit2)
qqnorm(residualStandard2, main = expression('Normal Probability plot : Y ~ '~sqrt(X)))
qqline(residualStandard2)

mtext("Compare raw and transformed fit", side = 3, line = -1.5, outer = TRUE)



dev.off()
################################################
# Box-Cox Procedure
################################################
par(mfrow = c(2,2))
bc_out = boxCox(lm.fit1, lambda = seq(0, 5, 0.01),
                plotit = T, eps = 1/50, 
                xlab = expression(lambda), y = "log-likelihood",)
bc_out = as.data.frame(bc_out)
colnames(bc_out) = c('lambda', 'log_l')
MLE = bc_out$lambda[which.max(bc_out$log_l)]; MLE

Ytransformed = mydata$ProductionTimeHours_Y ^ (1.5)
plot(Ytransformed~ProductionLotSize_X, data = mydata,pch = 20,
     main = expression('Regression function'))
lm.fit3 = lm(Ytransformed~mydata$ProductionLotSize_X)
abline(lm.fit3)

# residuals against fitted values
plot(lm.fit3$residuals ~ lm.fit3$fitted.values, pch = 20,
     main = expression('Residual against fitted values'))
abline(mean(lm.fit3$residuals), 0)

# QQ plot: Box-Cox
residualStandard3 = rstandard(lm.fit3)
qqnorm(residualStandard3, main = expression('Normal Probability plot'))
qqline(residualStandard3)

mtext("Box-Cox procedure", side = 3, line = -1.5, outer = TRUE)



################################################
# compare 3 regression fits
################################################

summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
anova(lm.fit1)
anova(lm.fit2)
anova(lm.fit3)
