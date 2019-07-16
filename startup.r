#An example of generating growth rates and lagged variables

library(lmtest)
library(car)
library(sandwich)
library(dyn)

d <- read.table(file="usoutput.dat",header=TRUE)

# turn data into time-series
d <- ts(d,frequency=12,start=c(1980,1))
# ip           p            cd           tb           mr           cap
d[1:5,]

# convert to percent growth rates
g <- 100.*diff( log(d[,1]),lag=1,differences=1) -  100.*diff( log(d[,2]),lag=1,differences=1)

#plot g
plot.ts(g,main="g",col="blue")

#put dataset tegether and generate lags. New dataset is dts
dts <- ts.intersect(g=g,lg=lag(g,1),lcd=lag(d[,3],1),ltb=lag(d[,4],1),lmr=lag(d[,5],1),lcap=lag(d[,6],1))

summary(dts)

d[1:5,]
dts[1:5,]


sd(g) #stdev of g

# get stdev of each column of the dataset dts
apply(dts, 2, sd)

# regression of g on lcap
ols1 <- lm( g ~ lcap,data=dts)
summary(ols1)

