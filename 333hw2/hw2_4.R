setwd("/Users/Qihong/Dropbox/Current Courses/STAT333/hw2")
rm(list = ls())

# read the data 
mydata = read.table("CH01PR19.txt", header = F)
colnames(mydata)[1] = "GPA"
colnames(mydata)[2] = "ACT"

# visualize the regression line 
lm.fit = lm(GPA ~ ACT, data = mydata)
plot(mydata$ACT, mydata$GPA, pch = 20)
abline(lm.fit)
summary(lm.fit)

# point estimate for ACT = 30
X = 30
estimate = lm.fit$coefficients[1] + 30 * lm.fit$coefficients[2]
estimate

# hw2_5 
# 5.1 residual sum to zero
sum(lm.fit$residuals)

# estimation of population variance 
pop.variance =sum ((lm.fit$residuals)^2) / lm.fit$df.residual
pop.variance
pop.std = sqrt(pop.variance)
pop.std

# print the ANOVA table
anova(lm.fit)


