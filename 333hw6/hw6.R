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
beta = solve(t(X) %*% X) %*% (t(X) %*% Y)

# 2) obtain residuals 
Yfitted = X %*% beta
residuals = Y - Yfitted

# 3) SSR
SSR = sum((Yfitted - mean(Y))^2)

# 4) SSE
SSE = sum(residuals^2)

# 5) variance and covariance matrix for betas
MSE = SSE / 4
beta_vcov = MSE * var(solve(t(X) %*% X))

# 6) point estimate of Y when X = 4
Xhat = c(1, 4)
Yhat = Xhat %*% beta

# 7) variance of a new prediction, where Xhat is 4
Yhat_var = MSE * (1 + t(Xhat) %*% solve(t(X) %*% X) %*% Xhat)

# b) variance of beta1 and beta0, and their covariance
cov_b0_b1 = beta_vcov[1,2]
var_b0 = beta_vcov[1,1]
var_b1 = beta_vcov[2,2]


# c) the hat matrix 
H = X %*% solve((t(X) %*% X)) %*% t(X)

# d) variance-covariance matrix for residuals
MSE * (diag(dim(H)[1]) - H)


