# HW8 - 4 - The SENIC data set  (Textbook 9.27, part a, b, p.381) 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw8_code')
rm(list = ls())
library(car)
library(leaps)
source('All_reg.R')
source('press.R')
mydata = read.table('APPENC01.txt', header = F, 
                    col.names = c('ID','lenStay','age','infectRisk','culturingRatio', 'xrayRatio',
                                  'numBed', 'affiliation', 'region' ,'dailyCensus', 'numNurses', 'facilities' ))
mydata = data.frame(mydata)
################
# PREPROCESSING
################
# eliminate categorical regressors
mydata$affiliation = NULL
mydata$region = NULL
mydata$ID = NULL

# select 57th to 113th observations
testSet = mydata[1:56,]
mydata = mydata[57:113,]
# count sample size 
nTrain = dim(mydata)[1]
nTest = dim(testSet)[1]

# log transform
LOG_STAY_train = log(mydata$lenStay)
LOG_STAY_test = log(testSet$lenStay)
mydata$lenStay = NULL

################
# compare model on the training and test set
################

# fit the model
lm.fit_train = lm(LOG_STAY_train~ age+ culturingRatio+ xrayRatio+ dailyCensus, data = mydata)
lm.fit_test = lm(LOG_STAY_test~ age+ culturingRatio+ xrayRatio+ dailyCensus, data = testSet)

summary(lm.fit_train)
summary(lm.fit_test)


################
# cross validation 
################
out = summary(lm.fit_train)
beta = t(coef(out)[,1])

temp = as.data.frame(cbind( rep(1,nTest), 
    testSet$age, testSet$culturingRatio, testSet$xrayRatio, testSet$dailyCensus))

Ypred = beta %*% t(temp)
sum( (Ypred - LOG_STAY_test)^2 ) / nTest

