# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333projectCode')
rm(list = ls())
library(GGally)
library(car)
library(plyr)
mydata = read.csv('databases/OpenAir_example_data_long.csv')
dim(mydata)
# eliminate row with NA 
mydata = mydata[complete.cases(mydata), ]

# temp: trim the dimentionality of the input space
# mydata = data.frame(mydata[1:300,2:10])

# glance at the data
summary(mydata)
# ggpairs(mydata, pch = 20)
plot(mydata, pch = 16)

# fit everything (with all interaction)
lm.fit = lm (mydata$pm10 ~ mydata$nox * mydata$no2 * mydata$so2 * mydata$co)
summary(lm.fit)
anova(lm.fit)
Anova(lm.fit, type = 'III')
# plot(lm.fit, pch = 16)


# lm.fit_noInt = lm (mydata$pm10 ~ mydata$ws + mydata$wd + mydata$nox + 
#                  mydata$no2 + mydata$o3 + mydata$so2 + mydata$co)
# 
# 
# # fitting individual predictor
# lm.fit_nox = lm (mydata$pm10 ~ mydata$nox)
# summary(lm.fit_nox)
# plot(mydata$pm10 ~ mydata$nox, pch = 20)
# abline(lm.fit_nox)
# plot(lm.fit_nox)

