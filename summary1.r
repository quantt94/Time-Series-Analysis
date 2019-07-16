library(datasets)
# Quarterly Earnings per Johnson & Johnson Share
JJ <- log(JohnsonJohnson)
plot(JJ)
mean(JJ)
sd(JJ)
quantile(JJ,c(0.05,0.95))
summary(JJ)
hist(JJ,col="blue")