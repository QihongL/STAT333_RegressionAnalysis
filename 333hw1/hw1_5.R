rm(list = ls())
setwd("/Users/Qihong/Courses/STAT333/hw1")
data1 = read.csv("Yield_Data.csv")
data2 = read.csv("Yield_Data2.csv")

# summary for mean and variance
mean1 = tapply(data1$Yield, data1$Treatment, mean)
mean1
var1 = tapply(data1$Yield, data1$Treatment, var)
var1

# boxplot(data1$Yield~data1$Treatment, main = "Yield data1")
# boxplot(data2$Yield~data2$Treatment, main = "Yield data2")

# compute the ratio between largest and smallest variance
ratio = max(var1) / min(var1)
ratio
# given this ratio, do you think we can assume the variance is equal? 
# my answer: no 

# ANOVA table
anova(lm(data1$Yield ~ data1$Treatment))



##### 2nd data set
# summary for mean and variance
tapply(data2$Yield, data2$Treatment, mean)
tapply(data2$Yield, data2$Treatment, var)
# ratio of largest and smallest variance
max(tapply(data2$Yield, data2$Treatment, var)) / min(tapply(data2$Yield, data2$Treatment, var))
# ANOVA table
anova(lm(data2$Yield ~ data2$Treatment))

