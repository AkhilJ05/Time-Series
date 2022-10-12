library(forecast)
library(tseries)

#BOX- JENKINS Methodology


### Model specification   out of moving average/ARIMA/box jenkins

df = read.csv("C:\\Users\\John\\Desktop\\Sem 3 notes\\Econ\\TRENT.NS.csv")
head(df)
summary(df)

attach(df)


auto.arima(Adj.Close)
# 0,1,0 shows lags are irrelevant
acf(diff(Adj.Close))

pacf(diff(Adj.Close))
# to be sure lags are irrelevant

# AIC = Akaike Information Criteria
# model with lower AIC is chosen as the model to test on

######## step 2   Parameter estimation


aa<- arima(Adj.Close,order = c(0,1,0))
aa

# checking if parametrers are iid
tsdiag(aa)

pacf(residuals(aa))

########## step 3  testing significance

# box test looks at residuals are iid, h0: they are iid
Box.test(residuals(aa),lag=10)

# SINCE P-value IS MORE THAN 0.05 WE CONCLUDE residuals are iid

######### step 4 prediction
#we predict stock prices

p1<- predict(aa,n.ahead = 10)
p1


p1$se  # is standard error of each var

####### step 5 viz

plot(Adj.Close,type = 'l',xlim=c(10,268))
lines(p1$pred,col='green')

#range of prediction i.e pred +1.96*se
lines(p1$pred+(1.96*p1$se),col='red')
lines(p1$pred-(1.96*p1$se),col='red')
