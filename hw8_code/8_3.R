# HW8 - 3 - The SENIC data set  (Textbook 9.25, part b, p.381) 
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
n = dim(mydata)[1]
# log transform
lenStay = mydata$lenStay
LOG_STAY = log(lenStay)
mydata$lenStay = NULL

################
# MODEL SELECTION
################
plot(mydata, pch = 20)
Out_all <- All_reg(LOG_STAY ~., data=mydata, nbest=5, nvmax=7)

# plot Mallow's Cp against p 
par(mfcol = c(2,2))
plot(Out_all$Cp ~ Out_all$P, pch = 20, main ='Mallow Cp', xlab = 'number of parameters')
abline(a = 0, b = 1)
plot(Out_all$RSQ_A ~ Out_all$P, pch = 20, main ='Adjusted R square', xlab = 'number of parameters')
plot(Out_all$BIC ~ Out_all$P, pch = 20, main ='BIC', xlab = 'number of parameters')

# select among models with 5 variables 
Out_all[16:25,]

################
# FITTING THE MODELS
################

# 4 predictors (including the intercept)
lm.fit_11 = lm(LOG_STAY~ age+ xrayRatio+ dailyCensus, data = mydata)
lm.fit_12 = lm(LOG_STAY~ xrayRatio+ dailyCensus+ numNurses, data = mydata)
lm.fit_13 = lm(LOG_STAY~ xrayRatio+ numBed+ dailyCensus, data = mydata)
lm.fit_14 = lm(LOG_STAY~ age+ xrayRatio+ numBed, data = mydata)
lm.fit_15 = lm(LOG_STAY~ infectRisk+ xrayRatio+ dailyCensus, data = mydata)
# 5 predictors (including the intercept)
lm.fit_16 = lm(LOG_STAY~ age+ xrayRatio+ dailyCensus+ numNurses, data = mydata)
lm.fit_17 = lm(LOG_STAY~ age+ xrayRatio+ numBed+ dailyCensus, data = mydata)
lm.fit_18 = lm(LOG_STAY~ age+ culturingRatio+ xrayRatio+ dailyCensus, data = mydata)
lm.fit_19 = lm(LOG_STAY~ age+ infectRisk+ xrayRatio+ dailyCensus, data = mydata)
lm.fit_20 = lm(LOG_STAY~ age+ xrayRatio+ dailyCensus+ facilities, data = mydata)
# 6 predictors (including the intercept)
lm.fit_21 = lm(LOG_STAY~ age+ culturingRatio+ xrayRatio+ dailyCensus+ numNurses, data = mydata)
lm.fit_22 = lm(LOG_STAY~ age+ infectRisk+ xrayRatio+ dailyCensus+ numNurses, data = mydata)
lm.fit_23 = lm(LOG_STAY~ age+ xrayRatio+ numBed+ dailyCensus+ numNurses, data = mydata)
lm.fit_24 = lm(LOG_STAY~ age+ xrayRatio+ dailyCensus+ numNurses+ facilities, data = mydata)
lm.fit_25 = lm(LOG_STAY~ age+ infectRisk+ xrayRatio+ numBed+ dailyCensus, data = mydata)
# 7 predictors (including the intercept)
lm.fit_26 = lm(LOG_STAY~ age+ infectRisk+ xrayRatio+ numBed+ dailyCensus+ numNurses, data = mydata)
lm.fit_27 = lm(LOG_STAY~ age+ infectRisk+ culturingRatio+ xrayRatio+ dailyCensus+ numNurses, data = mydata)
lm.fit_28 = lm(LOG_STAY~ age+ culturingRatio+ xrayRatio+ dailyCensus+ numNurses+ facilities, data = mydata)
lm.fit_29 = lm(LOG_STAY~ age+ culturingRatio+ xrayRatio+ numBed+ dailyCensus+ numNurses, data = mydata)
lm.fit_30 = lm(LOG_STAY~ age+ infectRisk+ xrayRatio+ dailyCensus+ numNurses+ facilities, data = mydata)


# 4 predictors (including the intercept)
press(lm.fit_11)
press(lm.fit_12)
press(lm.fit_13)
press(lm.fit_14)
press(lm.fit_15)
# 5 predictors (including the intercept)
press(lm.fit_16)
press(lm.fit_17)
press(lm.fit_18)
press(lm.fit_19)
press(lm.fit_20)
# 6 predictors (including the intercept)
press(lm.fit_21)
press(lm.fit_22)
press(lm.fit_23)
press(lm.fit_24)
press(lm.fit_25)
# 7 predictors (including the intercept)
press(lm.fit_26)
press(lm.fit_27)
press(lm.fit_28)
press(lm.fit_29)
press(lm.fit_30)



################
# STEPWISE REGRESSION MODELS
################

null.lm <- lm(LOG_STAY ~1, data=mydata)
full.lm <- lm(LOG_STAY ~., data=mydata)
step(null.lm, scope=list(lowr=null.lm, upper=full.lm), direction="forward")





