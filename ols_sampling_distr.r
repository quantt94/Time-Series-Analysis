library(sm)
library(car)

N=500
T=100

x<-rnorm(T, mean=0, sd=1)

#y_r<-matrix(0,T,1)
b<-matrix(0,500,2)
t_test<-matrix(0,500,1)

# we use the same X is all samples

one=1 
xm=as.matrix(cbind(one,x))


for (r in 1:N) {
  
  u = rnorm(T, mean=0, sd=1)        
  
  y = 1.0+ 5.0*x + u
  
  beta_hat=solve(t(xm)%*%xm)%*%t(xm)%*%y
  e = y - xm%*%beta_hat
  sigma2 = (t(e)%*%e)/(T-2)
  var = drop(sigma2)*solve(t(xm)%*%xm)
  t_test[r]=(beta_hat[2] - 5.0)/sqrt(var[2,2])
  
  b[r,]=beta_hat
  
}

#beta_hat should be normal
qqPlot(b[,1],distr="norm")
qqPlot(b[,2],distr="norm")

#summary(b)
mean(b[,1])
sd(b[,1])
mean(b[,2])
sd(b[,2])
hist(b[,2],plot=TRUE,col="blue",main="Sampling Distribution of beta_hat")

mean(t_test)
sd(t_test[,1])
hist(t_test,plot=TRUE,col="blue",main="Distribution of t-test")

