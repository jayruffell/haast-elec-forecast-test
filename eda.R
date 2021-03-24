cat('\nNOTE times are getting reported in UTC, but data is presumably NZ. Just FYI...\n')

rm(list=ls())
library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)
library(ggfortify) #nice time series plotting
library(fpp2) # datasets for 'Forecasting Principles and Practice book'
library(GGally)

# Read in data
dd <- read_csv('combineddata.csv')

#__________________________________________________________________________________________________________________________________

# eda --- 
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# pairs plot
#++++++++++++++

# Function to return points and geom_smooth
# allow for the method to be changed
ggpairsFun <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha=0.1) + 
    geom_smooth(method=method, ...)
  p
}
# Plot and save
graphics.off()
png("pairsPlot.png", height = 7, width = 10, units='in', res=300)
pairs <- dd %>%
  sample_n(1000) %>%
  select(-winddir_wgn) %>%
  ggpairs(lower = list(continuous = ggpairsFun))
print(pairs)
dev.off()

# RESULTS: 
# - Temperatures are all highly correlated, so can just use one. Picking Auckland because this is Probably the centre of North Island power consumption, So best for effects of temperature on consumption... Probably not the best choice for effects of temperature on power generation though. 
# - Wind doesn't seem to be related to load, so can probably ignore.Wind is slightly correlated with temperature. 
# - Clear non-linear relationship between load and temperature, which I will examine in a separate plot.

#++++++++++++++
# temp vs load
#++++++++++++++

# Basic plot
dd %>%
  sample_n(20000) %>%
  ggplot(aes(temp_ak, load)) + geom_point(alpha=0.1) + 
  ggsave('akTempVsLoad.png', height=6, width=10)
# ==> Very interesting non-linear relationship:
# - use being high at low temperatures, dropping rapidly when the temperature Increases above around 15° (heating no longer required), and then increasing when temperature is above say 20° (aircon).
# - Clearly three different sets of lines though, are these weekend versus weekday? Or seasonality? Or public holiday?
# - vertical stripes are because temperatures are getting rounded to the nearest degree

# Factor in dow
dd %>%
  sample_n(20000) %>%
  mutate(day=weekdays(time)) %>%
  ggplot(aes(temp_ak, load, colour=day)) + geom_point(alpha=0.1) +
  geom_smooth(se=F) + 
  ggsave('akTempVsLoad_dow.png', height=6, width=10)
# ==> Day of week is definitely part of it, With all trends shifting straight down when going weekdays down to Monday down to Saturday down to Sunday. Monday trend also perhaps slightly more variable? Public holidays? See here https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/

# Factor in time of day
dd %>%
  sample_n(20000) %>%
  mutate(day=weekdays(time)) %>%
  mutate(hour_num=hour(time)) %>%
  mutate(hour=as.character(hour_num)) %>%
  mutate(hour=fct_reorder(hour, hour_num)) %>%
  ggplot(aes(temp_ak, load, colour=hour)) + geom_point(alpha=0.1) +
  geom_smooth(se=F) + 
  ggsave('akTempVsLoad_hour.png', height=6, width=10)
# time of day closer look - cos hard to see what is what. should be clear spikes
dd %>%
  sample_n(20000) %>%
  mutate(day=weekdays(time)) %>%
  mutate(hour=hour(time)) %>%
  ggplot(aes(hour, load, colour=day)) + geom_point(alpha=0.1) +
  geom_smooth(se=F) + 
  ggsave('akHourVsLoad.png', height=6, width=10)
# ==> Time of day is missing piece of puzzle:
# - All show lower user in Late-night/early hrs, With peaks 7-10am and 4-8pm (EK's peak times)
# - Saturday and Sunday are both different, being lower than weekdays with a later morning peak (people getting up later). 
# - Monday morning and Friday evening are similar to weekdays: Coming out of weekend and heading into weekend respectively.

# putting dow and hour of day together - interactions
dd %>%
  sample_n(20000) %>%
  mutate(day=weekdays(time)) %>%
  mutate(hour_num=hour(time)) %>%
  mutate(hour=as.character(hour_num)) %>%
  mutate(hour=fct_reorder(hour, hour_num)) %>%
  ggplot(aes(temp_ak, load, colour=hour)) + geom_point(alpha=0.1) +
  geom_smooth(se=F) + facet_wrap(~day) + 
  ggsave('akTempVsLoad_hourTimesDay.png', height=6, width=10)
# ==> hard to tell if any interactions - let the model figure that out

# seasonality & long term trends
dd %>%
  sample_n(20000) %>%
  mutate(week_num=week(time)) %>%
  mutate(week=as.character(week_num)) %>%
  mutate(week=fct_reorder(week, week_num)) %>%
  ggplot(aes(week, load)) + geom_boxplot() + 
  geom_smooth(aes(week_num, load)) +
  ggsave('loadByWeek.png', height=6, width=10)
# ==> clear peak in winter months, and big drops 1st week of jan and last two weeks of december. Note Loess smooth couldn't handle this - lows in dec/jan are pulling estimates for adjacent weeks down. 
# ==> need extra dummy var to cover.
dd %>%
  sample_n(5000) %>%
  ggplot(aes(time, load)) + geom_point(alpha=0.1) +
  geom_smooth(se=F) + 
  ggsave('Load over time.png', height=6, width=10)
#==> very slight decrease over time

gc()
  
  
  
  
  
  
  
  







gc()