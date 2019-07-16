# Example of ARCH estimation using garch() command along with LR test

library(stats)
library(tseries)

# sp500 daily return from 19260102 -- 20061229 

d <- read.table(file="sp500-arch.dat",header=TRUE)

#scale return by 100.
# use data from 1990-2006
y <- 100*ts(d[17233:21518,2])
plot.ts(y)
summary(y)
var(y)

# regression with an intercept
ols <- lm(y~1)
lgl_ols = logLik(ols)

arch1 <- garch(y,order=c(0,1))
summary(arch1)
print(arch1$n.used)
lgl_arch1 = -arch1$n.used*0.5*log(2.*pi) -arch1$n.likeli
print(lgl_arch1)


arch4 <- garch(y,order=c(0,4))
summary(arch4)
print(arch4$n.used)
lgl_arch4 = -arch4$n.used*0.5*log(2.*pi) -arch4$n.likeli
print(lgl_arch4)


print(lgl_ols)
print(lgl_arch1)
print(lgl_arch4)


lr_test = 2*(lgl_arch4 - lgl_arch1)
q = 3 # 3 restrictions
pvalue= 1.0 - pchisq( lr_test, q)
print(lr_test)
print(pvalue)


plot.ts(arch4$fitted.values[,1])


