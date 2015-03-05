# Stat 333 Sp 2015 HW 3 Problem 6
# Generating Data1.csv 

# small sample sizes
# n <- c(20,20,20,20,20,20,20,20)

# large sample sizes
n <- rep(2000,8)

means <- c(100, 90, 80, 70, 60, 50, 40, 30)
std = 4.5 
y1 <- rnorm(n[1],mean=means[1], sd=std)
y2 <- rnorm(n[2],mean=means[2], sd=std)
y3 <- rnorm(n[3],mean=means[3], sd=std)
y4 <- rnorm(n[4],mean=means[4], sd=std)
y5 <- rnorm(n[5],mean=means[5], sd=std)
y6 <- rnorm(n[6],mean=means[6], sd=std)
y7 <- rnorm(n[7],mean=means[7], sd=std)
y8 <- rnorm(n[8],mean=means[8], sd=std)

Y <- round(c(y1,y2,y3,y4,y5,y6,y7,y8),2)
X <- c(rep(5, n[1]), rep(7, n[2]), rep(9, n[3]), rep(11, n[4]),rep(13, n[5]), rep(15, n[6]), rep(17, n[7]), rep(19, n[8]))
data1 <-data.frame(X, Y)


# Generating Data2.csv
n <- c(20,20,20,20,20,20,20,20)
means <- c(80, 75, 70, 65, 60, 55, 50, 45)
std = 4.5 
y1 <- rnorm(n[1],mean=means[1], sd=std)
y2 <- rnorm(n[2],mean=means[2], sd=std)
y3 <- rnorm(n[3],mean=means[3], sd=std)
y4 <- rnorm(n[4],mean=means[4], sd=std)
y5 <- rnorm(n[5],mean=means[5], sd=std)
y6 <- rnorm(n[6],mean=means[6], sd=std)
y7 <- rnorm(n[7],mean=means[7], sd=std)
y8 <- rnorm(n[8],mean=means[8], sd=std)


Y <- round(c(y1,y2,y3,y4,y5,y6,y7,y8),2)
X <- c(rep(9, n[1]), rep(10, n[2]), rep(11, n[3]), rep(12, n[4]),rep(13, n[5]), rep(14, n[6]), rep(15, n[7]), rep(16, n[8]))
data2 <-data.frame(X, Y)


write.table(data1, "Data1_large.csv", sep=",", row.names = F)
write.table(data2, "Data2_large.csv", sep=",", row.names = F)
