library(stats)
stock<-read.table("sp500.txt", header=TRUE)
list(names(stock))
stock<- ts(stock, frequency=1, start=c(1871))
print(stock)
print(stock[,1])
stock
plot.ts(stock[,2],col="red")

pd <- log(stock[,1]/stock[,2])
pe <- log(stock[,1]/stock[,3])
plot.ts(pd,col="red")
summary(pd)
var(pd)
t.test(pe,mu=2.5)
t.test(pe,mu=2.5)
t.test(pe,alternative=c("greater"),mu=2.5)
t.test(pe,alternative=c("greater"),mu=2.6)

