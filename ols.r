# simple 2 variable linear regression model

cy <- read.table(file='cons.dat',header=TRUE)
names(cy)
summary(cy)
list(cy)
# to call variables by name use attach
#attach(cy)
# lm() does OLS
ols<-lm(C~Y,data=cy)
# print results
summary(ols)
plot(ols$residuals,type='l')
print(ols$coefficients)
plot(cy$Y,ols$fitted,type='l')
confint(ols,level=0.95)

new <- data.frame(Y = seq(900, 1100, 20))
list(new)
predict(ols, new, interval = "prediction",level=0.95)


new <- data.frame(Y = seq(0, 1500, 50))
pred = predict(ols, new, interval = "prediction",level=0.95)
# plot the forecast and prediction intervals
matplot(new$Y,pred,type="l")

