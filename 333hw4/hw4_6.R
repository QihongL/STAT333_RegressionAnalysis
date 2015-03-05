# Statistics 333 hw 4 - question 6 
rm(list = ls())
# The data have the interesting property that when a simple linear regression 
# model is fit each regression gives nearly identical parameter estimates
# and summary statistics, despite the fact that the data 
# themselves are quite different. 
mydata = anscombe

# fit SLR 
lm1.fit = lm(mydata$y1 ~ mydata$x1)
lm2.fit = lm(mydata$y2 ~ mydata$x2)
lm3.fit = lm(mydata$y3 ~ mydata$x3)
lm4.fit = lm(mydata$y4 ~ mydata$x4)

# some stats
# The regression funtions are highly similar!
summary(lm1.fit)
summary(lm2.fit)
summary(lm3.fit)
summary(lm4.fit)
# as well as the ANOVA results! WTF! 
anova(lm1.fit)
anova(lm2.fit)
anova(lm3.fit)
anova(lm4.fit)

# visualization: regression functions 
par(mfrow=c(2,2))
plot(mydata$y1 ~ mydata$x1, pch = 20);
plot(mydata$y2 ~ mydata$x2, pch = 20);
plot(mydata$y3 ~ mydata$x3, pch = 20);
plot(mydata$y4 ~ mydata$x4, pch = 20);
mtext('Comparing scatter plots', outer=T, line= -2)
dev.off()


par(mfrow=c(2,2))
plot(mydata$y1 ~ mydata$x1, pch = 20); abline(lm1.fit)
plot(mydata$y2 ~ mydata$x2, pch = 20); abline(lm2.fit)
plot(mydata$y3 ~ mydata$x3, pch = 20); abline(lm3.fit)
plot(mydata$y4 ~ mydata$x4, pch = 20); abline(lm4.fit)
mtext('Comparing regression plots', outer=T, line= -2)
dev.off()

# visualization: regression functions 
par(mfrow=c(2,2))
plot(lm1.fit$residuals ~ mydata$x1, pch = 20); abline(mean(lm1.fit$residuals), 0 )
plot(lm2.fit$residuals ~ mydata$x2, pch = 20); abline(mean(lm2.fit$residuals), 0 )
plot(lm3.fit$residuals ~ mydata$x3, pch = 20); abline(mean(lm3.fit$residuals), 0 )
plot(lm4.fit$residuals ~ mydata$x4, pch = 20); abline(mean(lm4.fit$residuals), 0 )
mtext('Comparing residual plots', outer=T, line= -2)





