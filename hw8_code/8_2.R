# HW8 - 2 - Commercial properties - from textbook 7.19
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw8_code')
library(car)
rm(list = ls())
mydata = read.table('CH06PR18.txt', header = F, 
            col.names = c('rentalRate_Y','age_X1',
            'expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)
# sample size 
n = dim(mydata)[1]


################
# TRANSFORMATION
################
# Unit Normal Scaling (UNS)
mydataUNS = as.data.frame(scale(mydata))
# Unit Length Scaling (ULS), a.k.a correlational transformation 
mydataULS = as.data.frame(scale(mydata / sqrt(n - 1)))

# how to do UNS by hand ?
# columnMeans = colMeans(mydata)
# columnStds = apply(mydata, 2, sd)
# mydataUNS = (mydata - columnMeans) / columnStds


# fit model w/o standardization 
lm.fit_1234 = lm(mydata$rentalRate_Y ~ mydata$age_X1 + mydata$expenses_X2 
                 + mydata$vacancy_X3 + mydata$sqrFootage_X4)
# fit model w/ UNS
lm.fit_UNS_1234 = lm(mydataUNS$rentalRate_Y ~ mydataUNS$age_X1 + mydataUNS$expenses_X2 
                     + mydataUNS$vacancy_X3 + mydataUNS$sqrFootage_X4)
# fit model w/ ULS
lm.fit_ULS_1234 = lm(mydataULS$rentalRate_Y ~ mydataULS$age_X1 + mydataULS$expenses_X2 
                     + mydataULS$vacancy_X3 + mydataULS$sqrFootage_X4)

# check the results 
summary(lm.fit_1234)
summary(lm.fit_UNS_1234)
summary(lm.fit_ULS_1234)


####################
# DETRANSFORMATION
####################
# We can see that the transformed the model has the same MSE, R, t value and so on
# Although the estimated coefficients are different, we can obtain the "original" 
# coefficient in the following way: 

# 1st of all, we need sd for Y and all Xs
allsd = apply(mydata, 2, sd)
Sy = allsd[1]
Sx1 = allsd[2]
Sx2 = allsd[3]
Sx3 = allsd[4]
Sx4 = allsd[5]
# get all beta values for the transformed model 
temp = summary(lm.fit_UNS_1234)
allBetaTrans = temp$coefficients[,1]
beta1Trans = allBetaTrans[2]
beta2Trans = allBetaTrans[3]
beta3Trans = allBetaTrans[4]
beta4Trans = allBetaTrans[5]

# then bi = (Sy / Sxi) * bi_transformed
beta1 = beta1Trans * (Sy / Sx1);beta1
beta2 = beta2Trans * (Sy / Sx2);beta2
beta3 = beta3Trans * (Sy / Sx3);beta3
beta4 = beta4Trans * (Sy / Sx4);beta4



