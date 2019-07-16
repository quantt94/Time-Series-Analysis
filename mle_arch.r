library(stats)

# ARCH(1) minus log-likelihood
ARCH.1 <- function(theta,x)
{
logl = 0;
e    <<- 0;                          # variable made global
sigma2 <<- 0;
npar <<- 3;
n    <<- nrow(x);
mu     = theta[1];
omega  = theta[2];
alpha  = theta[3];
uncond = omega/(1-alpha);            # unconditional volality

i = 1;
while (i <= n){
#  e[i] <<- x[i,1] -mu;
  e[i] <<- x[i] -mu;
  if (i <= 1){
    sigma2[i] <<- uncond;
  }else{                                  
    sigma2[i] <<- omega + alpha*(e[i-1]^2); 
  }
  if(sigma2[i] <= 0.0){
    return(10.0^200);                   #penalize non-positive variances
  }
  
  logl = logl -.5*log(2*pi) -.5*log(sigma2[i]) -.5*(e[i]^2)/sigma2[i];
  i = i + 1;
}

return(-logl);    #because the computer do the minimum --> return the negative to get the maximum
}

# MAIN PROGRAM


FF=read.table(file='apt.dat',header=TRUE)

y      = as.matrix(FF$R)/10.;

sd(y[,1])
var(y[,1])

print("NOBS=");
print(nrow(y));


result_u <- nlm(ARCH.1, c(.001,.001,.001), y, hessian=T, steptol=.0000001, stepmax=100,
              iterlim=100,print.level=2,
              fscale=500,typsize=c(1.0,.1,.1));

print(result_u);

lgl_u = -result_u$minimum

#Get hessian
invhess <- solve( result_u$hessian );

print("Estimates Standard Error")
print( cbind(result_u$estimate,sqrt( diag(invhess) )) );


par(mfrow=c(2,1))         # multifigure setup: 2 rows, 1 cols
ts.plot(y,main="Returns");
ts.plot(sigma2,main="Conditional Variance");

readline("Press <return to continue>") 

# compute lgl for restructed model ARCH(0) with alpha=0.0
ARCH.0 <- function(theta,x) #omega equal to 0, basic model 
{
  phi=c(0,0,0)
  phi[1]=theta[1]
  phi[2]=theta[2]
  phi[3]=0.0
  ARCH.1(phi,x)
}
result_r <- nlm(ARCH.0, c(.01,.1), y, hessian=T, steptol=.0000001, stepmax=100,
              iterlim=100,print.level=2,
              fscale=500,typsize=c(1.0,.1));

print(result_r);

lgl_r = -result_r$minimum
print(lgl_u)
print(lgl_r)

LR_test = 2*(lgl_u - lgl_r)
pvalue = 1.0 - pchisq(LR_test,1)
print(LR_test)
print(pvalue) 
