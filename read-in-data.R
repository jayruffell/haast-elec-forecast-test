#________________________________________________________________

# Read in data ----
#________________________________________________________________

cat('\nNOTE times are getting reported in UTC, but data is presumably NZ. Just FYI...\n')

rm(list=ls())
library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)
library(ggfortify) #nice time series plotting
library(fpp2) # datasets for 'Forecasting Principles and Practice book'
library(GGally)
library(mgcv)

# Read in data and add new variables
dd <- read_csv('combineddata.csv') %>%
  mutate(hour=hour(time),
         day=weekdays(time)) %>%
  mutate(day=ifelse(day %in% c('Monday', 'Saturday', 'Sunday', 'Friday'), day, 'Other')) %>%
  mutate(week=week(time)) %>%
  mutate(xmasbreak=ifelse(week %in% c(51, 52, 1), 1, 0)) %>%
  mutate(daysSinceStart = 1:nrow(.))
