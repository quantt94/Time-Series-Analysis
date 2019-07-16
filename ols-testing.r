# Example of hypothesis test and diagnostic tests
# data is stock return and 3 stock portfolio factors

library(lmtest)
library(car)
library(sandwich)


FF=read.table(file='apt.dat',header=TRUE)

summary(FF)

ols1 = lm(R ~ f1+f2+f3,data=FF)
summary(ols1)
vcov(ols1)

# linear hypothesis tests
coeftest(ols1)

# F-tests
# test f1=0,f3=0
linearHypothesis(ols1,c("f1=0","f3=0"))

# test f1 - f2=0, f3=0.05 
linearHypothesis(ols1,c("f1-f2=0","f3=0.05"))





# Breush-Godfrey Test for autocorrelations with 2 lags of residuals
bgtest(ols1,order=2)

# test for heteroskedasticity, Breush-Pagan Test
ncvTest(ols1) # test for u2 as function of fitted values
ncvTest(ols1, ~ f1+f2+f3) # test for u2 as separate function of regressors
# test for u2 as separate function of regressors with cross-products
ncvTest(ols1, ~f1+f2+f3+f1*f2+f2*f3+f1*f3) 

# Goldfeld-Quandt test for heteroskedasticity, split sample in two and test for equal vars
gqtest(ols1,point=0.5)

# ARCH(2) test for heteroskedasticity 
u2 = ts(ols1$residuals)^2
d = ts.union(u2,u2l=lag(u2,-1),u2l2=lag(u2,-2))
arch = lm(u2~ u2l+ u2l2, data=d)
summary(arch)

# compute White heteroskedastic consistent standard errors
coeftest(ols1,vcov=hccm)# robust se

# redo F -test above using White Robust se.
linearHypothesis(ols1,c("f1-f2=0","f3=0"),white.adjust=TRUE)

