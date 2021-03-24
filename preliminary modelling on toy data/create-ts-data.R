#__________________________________________________________

# turn std data into a ts object same as the 'elecdaily' dataset used by Hyndman ----
#__________________________________________________________

library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)
library(ggfortify) #nice time series plotting
library(fpp2) # datasets for 'Forecasting Principles and Practice book'

# view data used by Hyndman
data(elecdaily)
ee <- elecdaily
head(ee)
str(ee)

# turn into what i'd normally work with - a dataframe - and then turn back into same as Hyndman str
eej <- as_tibble(ee)
eej <- ts(eej, frequency=7, start=c(1,4)) # , end=c(2,2)) # Note you CANT SPECIFY END - DOES AUTOMATICALLY
identical(ee, eej)
head(ee)
rm(ee, eej)
