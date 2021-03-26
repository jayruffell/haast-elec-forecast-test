#________________________________________________________________

# Read in data ----
#________________________________________________________________

# ***** NOTE all vars should be numeric, even if character vsersions of numbers (e..g "1" instead of 1). Otherwise resid plots mess up x-axis cos of factor ordering.

cat('\nNOTE times are getting reported in UTC, but data is presumably NZ. Just FYI...\n')

# params: filter dates for 'mdf', used for initial model training
startdate <- '2013-07-07'
enddate <- '2017-07-07'

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
    daychar=as.character(day), # see note at top
    daygrouped=ifelse(daychar %in% c('1', '5', '6', '7'), daychar, '0'),
    # week of year and days since start
    week=week(time),
    weekchar=as.character(week),
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

#________________________________________________________________

# define function for reporting how long gams take
#________________________________________________________________

gam_report <- function(starttime,  # call to Sys.time() right before running
                       mygam){
  cat(paste0('Run in ', 
             round(as.numeric(difftime(Sys.time(),start, units='mins')), 1),
             ' mins with Rsquare of ', 
             round(summary(mygam)$r.sq, 3), '\n'))
}

#________________________________________________________________

# function for resids plot - currently only std resids
#________________________________________________________________

resids_plot <- function(mydf, mymod, mymod_char){
  # extract preds and rmove interaction  terms
  predictors_incInts <- attr(mymod$terms , "term.labels")
  predictors <- predictors_incInts[!grepl(':', predictors_incInts) &
                                     !grepl('poly', predictors_incInts)] 
  # remove polynomial terms
  polypreds <- predictors[grepl('poly', predictors)]
  polypreds <- c(polypreds, 'poly(temp_ak, 2)')
  polypreds_updated <- gsub('poly\\(', "", polypreds)
  polypreds_raw <- unique(gsub('\\, [1-9]\\)', "", polypreds_updated))
  predictors <- c(polypreds_raw, predictors[!grepl('poly', predictors)])
  
  # plot
  myplot <- mdf %>%
    mutate(resids=resid(mymod)) %>%
    sample_n(1000) %>%
    select(resids, predictors) %>%
    gather(key, value, -resids) %>%
    # coerce predictors to have numeric values, for plotting
    mutate(value=as.numeric(value)) %>%
    ggplot(aes(value, resids, colour=key)) + geom_point(alpha=0.1) + geom_smooth(se=F) +
    facet_wrap(~key, scales='free') + theme(legend.position = 'none') 
  print(myplot)
  myplot + 
    ggsave(paste0('resids_', mymod_char, '.png'), height=6, width=10)
}

#________________________________________________________________

# function for fitted vs predicted values for specified time ranges
#________________________________________________________________

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

#________________________________________________________________

# function for fitted vs predicted values on all test data
#________________________________________________________________

fitted_vals_plot <- function(testdata, mymod, mymod_char){
  myplot <- testdata %>%
    mutate(fitted=predict(mymod, newdata=testdata)) %>%
    select(fitted, load, time) %>%
    gather(key, value, -time) %>%
    ggplot(aes(time, value, colour=key)) + geom_line()
  print(myplot)
  myplot +
    ggsave(paste0('predictedVsFitted_testdata_', mymod_char, '.png'), height=6, width=10)
}

#________________________________________________________________

# function for mape
#________________________________________________________________

report_mape <- function(testdata, mymod){
  predsdf <- testdata %>%
    mutate(fitted=predict(mymod, newdata=testdata))
  return(100 * mean(abs((predsdf$load - predsdf$fitted)/predsdf$load)))
}

# call to see mem usage
sort(sapply(mget(ls()),object.size))

#________________________________________________________________

# Function to Fit a polynomial model for temperature on load, where the best polynomial order (up to maxorder) is chosen as that which has the fewest number of parameters while Having dAIC < 2. 
#________________________________________________________________

# Poly terms must be numbered sequentially and specified without an order, thusly: "poly1(myvar) + poly2(mynextvar)"
# EXAMPLE: 
# test <- fit_best_polynomial_model(form=
#                             'load ~ poly1(temp_ak) + 
#                             daychar + hourchar + 
#                             poly2(temp_ak):daychar + 
#                             poly3(temp_ak):hourchar +
#                             daychar:hourchar + 
#                           hol + daysSinceStart + weekchar',
#                           data=mdf, maxorder=3)
fit_best_polynomial_model <- function(form, data, maxorder){
  
  #++++++++++++++
  # define formulas and fit models
  #++++++++++++++
  
  # dynamically create grid of orders to use
  expandGridList <- list()
  allPolyTerms <- str_extract_all(form, 'poly[0-9]\\(.*\\)')[[1]]
  for(p in 1:length(allPolyTerms)) expandGridList[[p]] <- 1:maxorder
  names(expandGridList) <- allPolyTerms
  polyGrid <-expand.grid(expandGridList)
  polyGrid <- mutate(polyGrid, aic=NA, numparams=NA)
  
  # use AIC to find best model - from linear to maxordr for main and interaction effects
  cat(paste('\nfitting', nrow(polyGrid), 'polymodels:\n'))
  start <- Sys.time()
  for(i in 1:nrow(polyGrid)){
    # dynamically swap out poly names in formula for real names and orders
    orders_i <- polyGrid[i,]
    form_i <- form
    for(name_p in allPolyTerms) {
      order_p_i <- orders_i[, name_p] # the order for the name for this iteration
      truename <- paste0('poly(', 
                         substr(name_p, 7, nchar(name_p)-1), 
                         ',', order_p_i, ')')
      form_i=gsub(name_p, truename, form_i, fixed=T)  
    }
    form_updated <- as.formula(form_i)

    # fit model and save results
    mod <- lm(form_updated, data=data)
    polyGrid$aic[i] <- AIC(mod)
    polyGrid$numparams[i] <- length(mod$coefficients)
    cat(i)
  }
  cat(paste0('\nRun in ', 
             round(as.numeric(difftime(Sys.time(),start, units='mins')), 1),
             ' mins\n'))
  
  #++++++++++++++
  # refit based on best - or smallest params within delta2 anyway
  #++++++++++++++
  
  # find best
  bestAIC <- polyGrid %>%
    filter(aic==min(aic)) %>%
    select(aic) %>%
    distinct() %>% # in case multiple identical AIC
    pull()
  candidates <- polyGrid %>%
    mutate(deltaAIC=aic-bestAIC) %>%
    filter(deltaAIC <=2)
  finalparams <- distinct(filter(candidates, deltaAIC==min(deltaAIC)))
  # some checks to ensure one set of params only
  if(nrow(finalparams)>1){
    finalparams <- filter(finalparams, numparams==min(numparams))
  } 
  if(nrow(finalparams)>1) finalparams <- head(finalparams, 1)
  
  # refit
  # convert character formula to proper formula and add in poly order
  for(name_p in allPolyTerms) {
    order_p_i <- finalparams[, name_p] # the order for the name for this iteration
    truename <- paste0('poly(', 
                       substr(name_p, 7, nchar(name_p)-1), 
                       ',', order_p_i, ')')
    form=gsub(name_p, truename, form_i, fixed=T)  
  }
  form_updated <- as.formula(form)
  
  mr_final <- lm(form_updated, data=data)
  cat('best params:')
  print(finalparams)
  return(mr_final)
}

gc()                            
                            
                     

