
##################
# 4 eigen decompose 
##################

X = matrix(c(1,1,1,3,4,-2,2,5,1),3,3)
XTX = t(X) %*% X
results = eigen(XTX)
results$vectors
diag(results$values)
results$vectors %*% diag(results$values) %*% solve(results$vectors)


