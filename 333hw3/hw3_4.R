setwd("/Users/Qihong/Dropbox/Current Courses/STAT333/333hw3")
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

#####################BEGIN HW3##########################################

#################################
######### TEXTBOOK 2.13 #########
#################################
# obtain a 95 percent interval estimate of the mean freshman GPA 
# for students whose ACT test score is 28. 
# Interpret your confidence interval.


# a) 
newdata = data.frame(ACT = 28)
predict(lm.fit, newdata, interval = "confidence", level = 0.95)

# b)
predict(lm.fit, newdata, interval = "prediction", level = 0.95)

# c) yes, b is wider than a, because ... 

# d) 
# Confidence band
W = sqrt(2 * qf(.95,2,118))
W



#################################
######### TEXTBOOK 2.23 #########
#################################

# a) compute it by hand! 
anova(lm.fit)
sum((mydata$GPA - mean(mydata$GPA))^2)

# b)
# MSR = 3.5878, MSE = 0.3383
# if there are the same... 

# c) F test beta1 = 0

# d) 


# e) get r, attach the right sign
cor(mydata$GPA,mydata$ACT)

# f) explain r and r square

