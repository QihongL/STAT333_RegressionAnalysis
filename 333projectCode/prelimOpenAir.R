# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333projectCode')
rm(list = ls())
mydata = read.csv('OpenAir_example_data_long.csv')
# verify the dimensionality of the input space
dim(mydata)
# temp: reduce the dim 
mydata = data.frame(mydata[1:100,2:9])

summary(mydata)


pairs(mydata, pch = 20)


lm.fit = lm (mydata$pm10 ~ mydata$no2 * mydata$o3 * mydata$so2 * mydata$co)
summary(lm.fit)
readline("Press <return> to see the linear model") 
plot(lm.fit)
