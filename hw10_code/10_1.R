# HW10 - Commercial properties
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/hw10_code')
rm(list = ls());
library(car);library(perturb)
mydata = read.table('CH06PR18.txt', header = F, col.names = c('rentalRate_Y','age_X1','expenses_X2','vacancy_X3','sqrFootage_X4'))
mydata = data.frame(mydata)

# constants
n = dim(mydata)[1]
p = 4 + 1

########################################
# hw 10.1 # ADDED VARIABLE PLOTS
########################################

# scatter plots
plot(mydata, pch = 16)
# linear model
lm.fit = lm(rentalRate_Y ~ age_X1 + expenses_X2 + vacancy_X3 + sqrFootage_X4, data = mydata)

# added variable plots
avPlots(lm.fit, pch = 16)


########################################
# hw 10.2 # OUTLIER DETECTION 
########################################

# a) studentized deleted residuals (PRESS)
press = rstudent(lm.fit)
press

# bonferroni criterion 
tcrit = qt(1 - (0.01 / (2 * n)), n - p - 1 ); tcrit

# plot the bonferroni criterion 
par(mfcol = c(1,1))
plot(press ~ lm.fit$fitted.values, ylim = c(-4.5,4.5), pch = 16, 
     xlab = 'Fitted values', ylab = 'studentized deleted residuals')
abline(h = 0)
abline(h = tcrit, lty = 2)
abline(h = - tcrit, lty = 2)


# b) visualization of "hii > 2p/n" criterion 

# obtain the hat leverage values
Hii = hatvalues(lm.fit);Hii
# identify outliers
2 * p / n
which(Hii > 2 * p / n)
# plot them
par(mfrow = c(1,1))
plot(Hii, press, pch = ' ')
text(Hii, press, labels = as.character(1:n))
points (Hii[Hii > 2 * p / n], press[Hii > 2 * p / n], cex = 2.5, col = 'red')
abline(v = 2 * p / n, lty = 2)





# c) hidden extrapolation of a new observation 
xnew = c(1, 10, 12, 0.05, 350000)
X = model.matrix(lm.fit)
# compute hat_new
xnew %*% solve(t(X) %*% X) %*% xnew 
# compare with average hat value
p/n




# plot the data and mark high leverage values
par(mfrow = c(2,2))
plot(mydata$rentalRate_Y ~ mydata$age_X1, pch = ' ')
text(mydata$age_X1, mydata$rentalRate_Y, labels = as.character(1:n))
points (mydata$age_X1[Hii > 2 * p / n], mydata$rentalRate_Y[Hii > 2 * p / n], cex = 2.5, col = 'red')

plot(mydata$rentalRate_Y ~ mydata$expenses_X2, pch = ' ')
text(mydata$expenses_X2, mydata$rentalRate_Y, labels = as.character(1:n))
points (mydata$expenses_X2[Hii > 2 * p / n], mydata$rentalRate_Y[Hii > 2 * p / n], cex = 2.5, col = 'red')

plot(mydata$rentalRate_Y ~ mydata$vacancy_X3, pch = ' ')
text(mydata$vacancy_X3, mydata$rentalRate_Y, labels = as.character(1:n))
points (mydata$vacancy_X3[Hii > 2 * p / n], mydata$rentalRate_Y[Hii > 2 * p / n], cex = 2.5, col = 'red')

plot(mydata$rentalRate_Y ~ mydata$sqrFootage_X4, pch = ' ')
text(mydata$sqrFootage_X4, mydata$rentalRate_Y, labels = as.character(1:n))
points (mydata$sqrFootage_X4[Hii > 2 * p / n], mydata$rentalRate_Y[Hii > 2 * p / n], cex = 2.5, col = 'red')



# 

# d) DEFITS, cook's dist, DFBATES
indices = sort(c(61, 8, 3, 53, 6, 62 ))
influenceStat = influence.measures(lm.fit)
influenceStat
influenceStat$infmat[indices,]

# dffits threshold
2 * sqrt(p / n)
# dfbetas threshold
2 / sqrt(n)
# hat value threshold
2*p/n


# f) compute the cook's distance
par(mfcol = c(1,1))
# plot(lm.fit, 4, pch = ' ')
plot(1:n, cooks.distance(lm.fit), pch = ' ', main = 'cooks distance', xlab = 'indices')
text(1:n, cooks.distance(lm.fit), labels = as.character(1:n))
# points (Hii[Hii > 2 * p / n], press[Hii > 2 * p / n], cex = 2.5, col = 'red')



# plot the model 
par(mfrow = c(2,2))
plot(lm.fit, pch = 16)
