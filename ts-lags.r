#Generating lags, 2 Different methods.

library(dyn)

d <- read.table(file='apt.dat',header=TRUE)

names(d)

# 1. First approach
# turn data into time-series data
d <- ts(d)
names(d)

# add lags to your dataset
dts <- ts.intersect(R=d[,2],lR=lag(d[,2],1),f1=d[,3],f2=d[,4],f3=d[,5],lf1=lag(d[,3],1),lf2=lag(d[,4],1),lf3=lag(d[,5],1))

# Note missing values NA
dts[1:10,]

# Exclude missing observations (R should do automatically)
ols1 <- lm( R ~ lR+lf1+lf2+lf3,data=dts, na.action=na.omit)
summary(ols1)

# 2. Second approach, Use dyn library
# turn data into time-series data
d <- ts(d)

# use dyn$lm() that understands lag operator
ols2 <- dyn$lm(R ~ lag(R,1)+lag(f1,1)+lag(f2,1)+lag(f3,1),data=d)
summary(ols2)
