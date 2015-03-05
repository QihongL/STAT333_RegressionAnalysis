rm(list = ls())
library(plyr)
setwd("/Users/Qihong/Courses/STAT333/hw1")


fisher = c(4.7, 5.3, 5.9, 4.8, 5.1, 6.2, 6.1, 6.1, 5.3, 6.1, 4.9)
pearson = c(6.3, 5.7, 5.8, 4.9, 6.9, 6.8, 7.2, 6.9, 6.8, 7.3)

length(fisher)
length(pearson)
type <- c(rep("fisher",length(fisher)), rep("person", length(pearson))) 
data = data.frame(c(fisher,pearson), type)
names(data)[1] = 'weight'
names(data)[2] = 'orchard'

boxplot(data$weight ~ data$orchard, 
        boxwex = 0.5, 
        main = "Weight of apples from \n two local apple orchards")

result = t.test(fisher, pearson, alternative = "less", var.equal = T,
       conf.level = 0.95)

result
