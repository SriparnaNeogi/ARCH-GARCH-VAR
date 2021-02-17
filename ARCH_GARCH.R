library(fGarch)

## Swiss Pension fund Index -
x = as.timeSeries(data(LPP2005REC))
View(x$LPP40)
# garchFit -
# Fit LPP40 Bechmark:
fit = garchFit(data= x$LPP40, trace = FALSE)
fit
fitted = fitted(fit)
head(fitted)

##ARCH - BOOKDOWN
install.packages("devtools")  # if not already installed
library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)

#####Data Import
data("byd", package="PoEdata")
View(byd)
rTS <- ts(byd$r)
plot.ts(rTS)
hist(rTS, main="", breaks=20, freq=FALSE, col="blue")
#### Is ARCH needed?
library(FinTS)
bydArchTest <- ArchTest(byd, lags=1, demean=TRUE)
bydArchTest
#### IF GARCH (0,1) THEN ARCH (1)
#A GARCH model subsumes ARCH models, 
#where a GARCH(0, q) is equivalent to an ARCH(q) model.
library(tseries)
byd.arch <- garch(rTS,c(0,1))
byd.arch
sbydarch <- summary(byd.arch)
sbydarch
hhat <- ts(2*byd.arch$fitted.values[-1,1]^2)
plot.ts(hhat)


####### GARCH(1,1) + arma(1,1)
library(fGarch)
m2=garchFit(formula=~arma(1,1)+garch(1,1),data=byd,trace=F,cond.dist="std")
summary(m2)
predict(m2,5)$meanForecast

########3 Identify order of ARCH
arima212 = arima(byd,order=c(2,1,1))
res.arima = arima212$res
arch08 = garch(res.arima,order=c(0,8),trace=F)
aic8 =AIC(arch08)
arch07 = garch(res.arima,order=c(0,7),trace=F)
aic7 =AIC(arch07)
arch06 = garch(res.arima,order=c(0,6),trace=F)
aic6 =AIC(arch06)
arch05 = garch(res.arima,order=c(0,5),trace=F)
aic5=AIC(arch05)
arch04 = garch(res.arima,order=c(0,4),trace=F)
aic4=AIC(arch04)
arch03 = garch(res.arima,order=c(0,3),trace=F)
aic3=AIC(arch03)
arch02 = garch(res.arima,order=c(0,2),trace=F)
aic2=AIC(arch02)
arch01 = garch(res.arima,order=c(0,1),trace=F)
aic1=AIC(arch01)

aic_all = rbind(aic1,aic2,aic3,aic4,aic5,aic6,aic7,aic8)
min(aic_all)


library(rugarch)               # load the "rugarch" package

spec = ugarchspec(variance.model = list(garchOrder = c(0,1))) # specify the model
fit = ugarchfit(data = byd, spec = spec)                        # fit the model
fcst = ugarchforecast(fitORspec = fit, n.ahead = 30)           # generate the forecast
print(fcst)                    # forecast object
print(fcst@forecast$seriesFor) # point forecast
print(fcst@forecast$sigmaFor)  # forecast of the conditional variance