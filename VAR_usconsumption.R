library(vars)
library(fpp)
library(ggplot2)
data(usconsumption)
plot(usconsumption)

View(usconsumption)
VARselect(usconsumption[,1:2], lag.max=8,
          type="const")[["selection"]]

var1 =  VAR(usconsumption[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

forecast(var1) %>%
  autoplot() + xlab("Year")

View(forecast(var1))