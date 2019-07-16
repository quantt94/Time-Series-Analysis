# unit roots

library(tseries)
#par.ini <- par(no.readonly=TRUE) 

# simulate some iid data (stationary)
x <- rnorm(1000) 
plot.ts(x) #to make sure it is stationary
adf.test(x,alternative=c("stationary")) #pvalue = 0.01 -> stationary 

# make a unit root
y <- diffinv(x) + 5
plot.ts(y)
adf.test(y,alternative=c("stationary"),k=2) 




#14 macroeconomic time series: 
#cpi, ip, gnp.nom, vel, emp, 
#int.rate, nom.wages, gnp.def, 
#money.stock, gnp.real, stock.prices, 
#gnp.capita, real.wages, and 
#unemp and the joint series NelPlo

data(NelPlo)

# test CPI
adf.test(cpi,alternative=c("stationary"),k=2) 
log_cpi <- log(cpi)
adf.test(log_cpi,alternative=c("stationary"),k=2)
infl <- diff(log_cpi,lag=1,difference=1)
adf.test(infl,alternative=c("stationary"),k=2)
plot.ts(cpi)
plot.ts(log_cpi)
plot.ts(infl)

# test gnp.real
plot.ts(gnp.real) #important: you MUST generate the plot so you can see if it is stationary or not
log_g <- log(gnp.real)
adf.test(log_g,alternative=c("stationary"),k=2) #k is p in the model
growth_g <- diff(log_g,lag=1,difference=1)
adf.test(growth_g,alternative=c("stationary"),k=2)
plot.ts(growth_g)


