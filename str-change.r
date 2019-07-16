#example of structural break estimation and Chow test

library(car)
library(lmtest)
library(sandwich)
library(strucchange)

FF=read.table(file='apt.dat',header=TRUE)

summary(FF)

#get number of observations
nobs = nrow(FF)
# set break point for structural break test
tau = floor( 0.6*nobs )

#model without breaks
ols1 = lm(R ~ f1+f2+f3,data=FF)
summary(ols1)




# construct 0,1 dummy variable
d <- vector(mode="numeric", length=nobs)
d[1:nobs] =0
d[tau:nobs]=1
print(d)

#add the dummy variables to our dataset
FF$di <-  d
FF$df1 <- d * FF$f1
FF$df2 <- d * FF$f2
FF$df3 <- d * FF$f3

ols2 = lm(R ~ f1+f2+f3 + di+df1+df2+df3 ,data=FF)
summary(ols2)

# test is dummies are significant
linearHypothesis(ols2,c("di=0","df1=0","df2=0","df3=0"))
linearHypothesis(ols2,c("di=0","df1=0","df2=0","df3=0"),white.adjust=TRUE)

# Chow test for structural change (identical F stat )
# useful for a quick test. Does not do White cov correction
sctest(R ~ f1+f2+f3,data=FF,type="Chow",point=tau-1)

