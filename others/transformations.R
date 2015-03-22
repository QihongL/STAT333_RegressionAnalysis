rm(list = ls())

# simulate transformations

x = 1:100
y = x + rnorm(100)

plot(x,y)

par(mfrow = c(2,2))
plot(x, log(y), main = 'raw')
plot(sqrt(x), log(y), main = 'x^2')


plot(x^2, y, main = 'raw')
plot(sqrt(x), y, main = 'x^2')



# plot(x, y^3, main = 'raw')
# plot(x,log(y^2, 3))