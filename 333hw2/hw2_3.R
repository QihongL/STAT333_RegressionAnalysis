setwd("/Users/Qihong/Dropbox/Current Courses/STAT333/hw2")
rm(list = ls())
mydata = read.csv("Woodpecker Data.csv")

# mean centering
Temperature = mydata$Temperature - mean(mydata$Temperature)
Cavity_Depth = mydata$Cavity_Depth - mean(mydata$Cavity_Depth)
# fit the model
lm.fit = lm(Cavity_Depth ~ Temperature)
# scatter plot 
plot(Temperature, Cavity_Depth, pch = 20)
# add the regression line
abline(lm.fit)
# report the statistics
summary(lm.fit)
