#__________________________________________________________

# awesome plotting package for ts data validation - especially changepoints and breakpoitns analysis!

# Examples from http://www.sthda.com/english/wiki/ggfortify-extension-to-ggplot2-to-handle-some-popular-packages-r-software-and-data-visualization#plotting-time-series
#__________________________________________________________

rm(list=ls())
library(ggfortify) #nice time series plotting
library(fpp2) # datasets for 'Forecasting Principles and Practice book'

#packages used by ggfortify for breakpoints/stationarity changes
library(changepoint)
library(strucchange)

#view ts data
data(elecdaily)
head(elecdaily)
str(elecdaily) # time series list

# ggfortify plots
autoplot(elecdaily)
demand <- elecdaily[,1]
str(demand)
autoplot(cpt.meanvar(demand))
autoplot(breakpoints(demand ~1))

