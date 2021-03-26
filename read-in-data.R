#________________________________________________________________

# Read in data ----
#________________________________________________________________

cat('\nNOTE times are getting reported in UTC, but data is presumably NZ. Just FYI...\n')

# params: filter dates for 'mdf', used for initial model training
startdate <- '2013-01-01'
enddate <- '2018-02-14'

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
    # hour - numeric and char
  mutate(
    hour=hour(time),  
    hourchar=as.character(hour),
    # day - numeric, char, and tues-thurs grouped (based on EDA)
    daychar=weekdays(time),
    day=
      ifelse(grepl('Mon', daychar), 1, 
             ifelse(grepl('Tu', daychar), 2, 
                    ifelse(grepl('We', daychar), 3, 
                           ifelse(grepl('Th', daychar), 4, 
                                  ifelse(grepl('Fr', daychar), 5, 
                                         ifelse(grepl('Sa', daychar), 6, 
                                                7)))))),
    daygrouped=ifelse(daychar %in% c('Monday', 'Saturday', 'Sunday', 'Friday'),
                      daychar, 'Other'),
    # week of year and days since start
    week=week(time),
    daysSinceStart = 1:nrow(.),
    # xmas break
    xmasbreak=ifelse(week %in% c(52, 1), 1, 0))

# create public hols dataframe and join to main df
holsdf <- data.frame(date=c('2013-12-25',
                            '2014-12-25',
                            '2015-12-25',
                            '2016-12-25',
                            '2017-12-25',
                            '2018-12-25',
                            '2013-12-26',
                            '2014-12-26',
                            '2015-12-26',
                            '2016-12-26',
                            '2017-12-26',
                            '2018-12-26',
                            '2013-12-29',
                            '2014-12-29',
                            '2015-12-29',
                            '2016-12-29',
                            '2017-12-29',
                            '2018-12-29',
                            '2013-01-01',
                            '2014-01-01',
                            '2015-01-01',
                            '2016-01-01',
                            '2017-01-01',
                            '2018-01-01',
                            '2013-01-02',
                            '2014-01-02',
                            '2015-01-02',
                            '2016-01-02',
                            '2017-01-02',
                            '2018-01-02'))
holsdf$date <- as.Date(holsdf$date)
holsdf$hol <- 1
dd <- dd %>%
  mutate(date=as.Date(time)) %>%
  left_join(holsdf, by='date') %>%
  select(-date)
dd$hol[is.na(dd$hol)] <- 0

#________________________________________________________________

# create dataframe for testing models on - reduced amount of data ----
#________________________________________________________________

mdf <- dd %>%
  filter(time>=startdate & time<=enddate)

#________________________________________________________________

# Helpef functions ----
#________________________________________________________________

# define function for reporting how long gams take
gam_report <- function(starttime,  # call to Sys.time() right before running
                       mygam){
  cat(paste0('Run in ', 
             round(as.numeric(difftime(Sys.time(),start, units='mins')), 1),
             ' mins with Rsquare of ', 
             round(summary(mygam)$r.sq, 3), '\n'))
}

# function for resids plot - currently only std resids
resids_plot <- function(mydf, mymod, mymod_char){
  # extract preds
  predictors_incInts <- attr(mymod$terms , "term.labels")
  predictors <- predictors_incInts[!grepl(':', predictors_incInts)]
  # plot
  myplot <- mdf %>%
    mutate(resids=resid(mymod)) %>%
    sample_n(1000) %>%
    select(resids, predictors) %>%
    gather(key, value, -resids) %>%
    ggplot(aes(value, resids, colour=key)) + geom_point(alpha=0.1) + geom_smooth(se=F) +
    facet_wrap(~key, scales='free') + theme(legend.position = 'none') 
  print(myplot)
  myplot + 
    ggsave(paste0('resids_', mymod_char, '.png'), height=6, width=10)
}

# function for fitted vs predicted values for specified time ranges
fitted_vals_summer_winter_plot <- function(mydf, mymod, mymod_char){
  myplot <- mydf %>%
    mutate(fitted=predict(mymod)) %>%
    mutate(period=ifelse(time>='2017-06-07' & time<='2017-07-14', 'june2017',
                         ifelse(time>='2017-12-07' & time<='2018-02-14', 'dec2017', ifelse(time>='2014-06-07' & time<='2014-07-14', 'june2014',ifelse(time>='2014-12-07' & time<='2015-02-14', 'dec2014', ifelse(time>='2015-06-07' & time<='2015-07-14', 'june2015',ifelse(time>='2015-12-07' & time<='2016-02-14', 'dec2015','other'))))))) %>%
    filter(grepl('june|dec', period)) %>%
    select(fitted, load, time, period) %>%
    gather(key, value, -time, -period) %>%
    ggplot(aes(time, value, colour=key)) + geom_line() + 
    facet_wrap(~period, scales='free', ncol=1)
  print(myplot)
  myplot +
    ggsave(paste0('predictedVsFitted_', mymod_char, '.png'), height=6, width=10)
}

# call to see mem usage
sort(sapply(mget(ls()),object.size))

gc()                            
                            
                     

