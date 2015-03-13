# HW6 - 1,3,4 - Consumer finance
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333hw6')
rm(list = ls())
mydata = read.table('CH05PR05.txt', header = F)
colnames(mydata)[1] = "Y"; # the number of competing loan companies in each city
colnames(mydata)[2] = "X"; # perThousand of company 

# convert the data into vector form
X = cbind(rep(1, length(mydata$X)), c(mydata$X))
Y = c(mydata$Y)

# play with the data
lm.fit = lm(mydata$Y ~ mydata$X)
plot(mydata$Y ~ mydata$X, pch = 20, main = 'SLR regression function')
abline(lm.fit)


# Question 1 - textbook 5.5

# a) Y'Y
t(Y) %*% Y

# b) X'X
t(X) %*% X

# C) X'Y
t(X) %*% Y


# Question 3, textbook 5.13
# invert X'X
solve(t(X) %*% X)


# Question 4, textbook 5.24 
# a) 
# 1) obtain beta vector
beta = solve(t(X) %*% X) %*% (t(X) %*% Y); beta

# 2) obtain residuals 
Yfitted = X %*% beta
residuals = Y - Yfitted;residuals

H = X %*% solve((t(X) %*% X)) %*% t(X);

# 3) SSR
J = matrix(rep(rep(1,6),6),6,6)
SSR = t(Y) %*%  (H - J / 6) %*% Y;SSR

# 4) SSE
SSE = t(Y) %*%  (diag(6) - H) %*% Y;SSE

# 5) variance and covariance matrix for betas
MSE = SSE / 4
beta_vcov = MSE * solve(t(X) %*% X); beta_vcov

# 6) point estimate of Y when X = 4
Xhat = c(1, 4)
Yhat = Xhat %*% beta; Yhat

# 7) variance of a new prediction, where Xhat is 4
Yhat_var = MSE * (1 + t(Xhat) %*% solve(t(X) %*% X) %*% Xhat);Yhat_var

# b) variance of beta1 and beta0, and their covariance
beta_vcov
covariance_of_b0_and_b1 = beta_vcov[1,2];covariance_of_b0_and_b1
variance_b0 = beta_vcov[1,1];variance_b0
variance_b1 = beta_vcov[2,2];variance_b1


# c) the hat matrix 
H = X %*% solve((t(X) %*% X)) %*% t(X);H

# d) variance-covariance matrix for residuals
MSE * (diag(dim(H)[1]) - H)


