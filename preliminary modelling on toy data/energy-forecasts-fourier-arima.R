stop("dont have electricity data but can get from work computer")
#__________________________________________________________

# background ----
#__________________________________________________________

# hyndman says daily data and data with long seasonal periods (e.g. 365 for daily) is difficult to forecast with models other than fourier + ARIMA:
# - for data < 1yr, just set freq=7 and go straight ARIMA
# - for many yrs data, can use a TBATS model taht allows specification of periodicity of 7d and 365.25d to capture both weekly and seasal effects, BUT DOESNT ALLOW COVARIATES
# --> easiest way with covariates is regression model with fourier terms and other covariate, plus arima errors. Only issue is that have to assume that seasonal patterns don't change YOY, which is usually resonalbe unless there is decades of data
# https://robjhyndman.com/hyndsight/dailydata/
# https://otexts.com/fpp2/dhr.html
# https://robjhyndman.com/hyndsight/longseasonality/

#__________________________________________________________

# pull data from separate analysis - elec PRICE not demand ----
#__________________________________________________________

rm(list=ls())
library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)

# from https://www.emi.ea.govt.nz/Wholesale/Reports/W_P_C?DateFrom=20110101&DateTo=20201231&RegionType=ZONE&TimeScale=DAY&WeightedBy=DEMAND&_rsdr=L10Y&_si=v|3. Note you can get simple average, demand-wgted av, or anothe rav but all v similar, so doesn't matter. This is d-w av.
# - note also diff regions show v similar trends, so don't need to analyse separately.
ee <- read_csv('electricity spot prices.csv', skip=11) %>%
  # only look at Lower South Is, corresponding to Oamaru weather station
  filter(Region=='Lower South Island' ) %>%
  # convert to weekly
  mutate(date = substr(`Period start`, 1, 10)) %>%
  mutate(date = as_date(date, format='%d/%m/%Y')) %>%
  as_tibble()
ee <- ee[,6:7]
colnames(ee) <- c('price', 'date')
as.data.frame(ee[!complete.cases(ee),])
nrow(ee) # many yrs

#+++++++++++++++++++
# example straight from top hyndman link. NOTE: use AIC to pick good K value for fourier terms.
#+++++++++++++++++++

y <- ts(ee$price, frequency=7)
z <- fourier(ts(ee$price, frequency=365.25), K=5)
zforward <- fourier(ts(ee$price, frequency=365.25), K=5, h=100) # if you wanna forecast forward

# visualise what fourier really is
data.frame(z, index=1:length(z)) %>%
  gather(key, value, -index) %>%
  filter(index<1000) %>%  #charts too busy otherwise
  ggplot(aes(index, value, colour=key)) + geom_line() + facet_wrap(~key)
plot(z)
length(z)/length(ee$price) 
# ==> adds on cyclic elements - and are different length to orig data. There are ten elements for K=5, 1 for sin and 1 for cos component
fit <- auto.arima(y, xreg=z, seasonal=FALSE)
fc <- forecast(fit, xreg=zforward, h=100)
plot(fc)
AIC(fit, auto.arima(y, seasonal=FALSE)) # straigh arima better in this case!
