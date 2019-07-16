library(zoo)
library(lmtest)
library(nlme)
library(dyn)

# an example of correcting for autocorrelated errors in the linear
# regression model. The follwoing model is jointly estimated
# Mkt.RF_t = b0 + HML_t*b1 + RF_t*b2 + u_t
# u_t = Phi*u_{t-1} + e_t, e_t ~ N(0,1)

FF=read.table(file='FF-Factors-m.dat',header=TRUE)

ols1 = lm(Mkt.RF ~RF,data=FF) # Missing SMB variable results in AC
summary(ols1)
lgl1=logLik(ols1)

plot(ols1$residuals[1:100],col="blue",type="l")



# Breush-Godfrey Test for autocorrelations with 2 lags of residuals
bgtest(ols1,order=1)

# correct the model for AC and jointly estimate AC coefficient
ac_model = gls(Mkt.RF ~RF,data=FF,correlation=corARMA(p=1),method="ML")
summary(ac_model)
lgl2=logLik(ac_model)

LR=2*(lgl2-lgl1)
pvalue=pchisq(LR,df=1,lower.tail = FALSE)
print(LR)
print(pvalue)

# Alternative fix to AC
# add a lagged dependent variable
# convert data to time-series form to make lags easy to add.
FF <- ts(FF)
# OLS for time series data
ols2 = dyn$lm(Mkt.RF ~RF + lag(Mkt.RF,1),data=FF)
summary(ols2)
lgl3=logLik(ols2)
print(lgl3)
