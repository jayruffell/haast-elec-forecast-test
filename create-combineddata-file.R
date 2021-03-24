cat('\nNOTE times are getting reported in UTC, but data is presumably NZ. Just FYI...\n')

#++++++++++++++
# notes
#++++++++++++++

# - removed palmy; too many NAs and AK-WGN trends prob captures. Plus PN only important for wind farm? and wind data only for WGN anyway.
# - intepolating 0.15% missing vals for load and ~4% missing vals for weather based on linear na.interp from forecast package (plots looked good)

rm(list=ls())
library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)
library(ggfortify) #nice time series plotting
library(fpp2) # datasets for 'Forecasting Principles and Practice book'


# Read in Climate data and check missing vals etc.
cc <- read_csv('climate_actuals.csv', col_names =F)
colnames(cc) <- c('var', 'time','station','value')
cc %>%
  group_by(station) %>%
  tally()
cc[!complete.cases(cc),] # ==> no missing vals

# Read in elec load and check missing vals etc.
ee <- read_csv('demandNI.csv', col_types = list(col_datetime(), col_character())) %>%
  transmute(time=Date, load=as.numeric(Load))
# report missing vals
ee[!complete.cases(ee),] %>%
  pull(time)
125/nrow(ee)*100 # .14% missing
# ==> missing vals up to sep 23 2010 - use algo that can deal with these, or start training after this period, or interpolate

# spread climate data to be one col per station-var combination
cc <- cc %>%
  mutate(station2=ifelse(grepl('Auckland', station), 'ak',
                        ifelse(grepl('Christ', station), 'ch',
                               ifelse(grepl('Palm', station), 'pn', 'wgn'))),
         var2=ifelse(grepl('temp', var), 'temp',
                    ifelse(grepl('wind_h', var), 'wind', 'winddir'))) %>%
  transmute(time, var=paste0(var2, '_', station2), value) %>%
  # occasionally have double-recordings for a station-time combo - take av 
  group_by(var, time) %>%
  summarise(value=mean(value)) %>%
  ungroup() %>%
  spread(key=var, value=value)
head(cc, 20)
tail(cc, 20)

# Snip both series down to min and max values, create a full time series based on these dates, then join other data
minTime <- max(c(min(cc$time), min(ee$time))) # 3pm 2008-07-09
maxTime <- min(c(max(cc$time), max(ee$time))) # 2pm 2018-07-09
alltimesdf <- data.frame(time=seq.POSIXt(from=minTime, to=maxTime, by='hours'))
dd <- alltimesdf %>%
  left_join(ee, by='time') %>%
  left_join(cc, by='time')

# record missing vals info
dd <- select(dd, -temp_pn) # removing; too many NAs and AK-WGN prob captures.
# summary(dd)
135/nrow(dd)*100 # missing load - 0.2% --> interpolate
3600/nrow(dd)*100 # missing climate - 4% --> interpolate

# interpolate missing vals
interp_fun <- function(x) {
  myts <- ts(x)
  return(na.interp(myts))
  }
# # ---------------------------------------
# # TMP check interp works
# plotdf <- dd %>%
#   mutate(temp_ak_i=interp_fun(temp_ak),
#          interped=ifelse(is.na(temp_ak), 'y', 'n')) %>%
#   select(temp_ak_i, time, interped) %>%
#   tail(100)
# ggplot(plotdf, aes(time, temp_ak_i, colour=interped)) + geom_point()
# # ---------------------------------------
names(dd)
dd_interpd <- dd %>%
  mutate(load=interp_fun(load),
         temp_ak=interp_fun(temp_ak),
         temp_ch=interp_fun(temp_ch),
         temp_wgn=interp_fun(temp_wgn),
         wind_wgn=interp_fun(wind_wgn),
         winddir_wgn=interp_fun(winddir_wgn),
         )
summary(dd_interpd)           

# write to file
write.csv(dd_interpd, 'combineddata.csv', row.names=F)

gc()