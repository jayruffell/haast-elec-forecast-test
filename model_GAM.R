#________________________________________________________________

# Model requirements based on EDA:
#________________________________________________________________

# - long-term trend effect which can probably be linear
# - nonlinear hour effect
# - Non-linear temperature effect
# - non-linear day of week effect - At least separating out Monday to Friday from remaining days. (and public hols?)
# - Time of year effect, with either a dummy variable for Christmas/New Year or a flexible enough model to account for this. Aside from this temperature alone is probably enough
# - interactions:
# -- Between temperature and day of week
# -- between temperature and hour of day
# -- Possibly others, including higher order interactions

#________________________________________________________________

# GAM with tensor (most flexible) interactions on 3yr data. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# from here https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
# nb also see choose.k documentation (mgcv package) for important model checks for whether model is over/underfitted
# also here https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package for type of spline to use & choice of k
# -- Thin plate splines tend to have better RMSE performance than the other three you mention but are more computationally expensive to set up. Unless you have a reason to use the P or B spline bases, use thin plate splines unless you have a lot of data and if you have a lot of data consider the cubic spline option.
# -- The choice of k is important and the default is arbitrary and something you want to check (see gam.check()), but the critical observation is that you want to set k to be large enough to contain the envisioned dimensionality of the underlying function you are trying to recover from the data. In practice, one tends to fit with a modest k given the data set size and then use gam.check() on the resulting model to check if k was large enough. If it wasn't, increase k and refit. Rinse and repeat...

# read in data
source('read-in-data.r')

# Code in read-in-data grouped tues-thurs together - split these out and make integers for smooth
dd <- dd %>%
  mutate(daychar=weekdays(time)) %>%
  mutate(
    day=
      ifelse(grepl('Mon', daychar), 1, 
             ifelse(grepl('Tu', daychar), 2, 
                    ifelse(grepl('We', daychar), 3, 
                           ifelse(grepl('Th', daychar), 4, 
                                  ifelse(grepl('Fr', daychar), 5, 
                                         ifelse(grepl('Sa', daychar), 6, 
                                                7))))))) %>%
  select(-daychar)
str(dd)                                  

# Define data to model: five years worth up to 2018. NO higher order interactions based on day*hour*temp plot. Also make hour character to allow complex shapes
mdf <- dd %>%
  filter(time>='2017-01-01' & time<='2018-01-31')

#++++++++++++++
# prelim gam - like first one from above link. 
#++++++++++++++

# 'cr' and 'ps' are cubic- and p-splines, the ones chosen in the link. k should be set to maximum no. of unique values (and knots are then chosen automatically I believe)
# from above stackoverflow link: 
gam_prelim <- gam(load ~ s(hour, bs='cr', k = 24) +
               s(day, bs='cr', k = 7) +
               daysSinceStart + xmasbreak +
               s(temp_ak, bs='cr', k=200) # max 684 uniques values - but see above for how to check this. Note I'm probably not using a good value of k!
               ,
             data = mdf,
             family = gaussian)
summary(gam_prelim)$r.sq

# check if k vals are ok - # see printout, it tells you what to do :)
gam.check(gam_prelim) 

# plot 
layout(matrix(1:4, nrow = 1))
plot(gam_prelim, shade = TRUE)
graphics.off()

#++++++++++++++
# tensor-based gams 
#++++++++++++++

gam_te <- gam(load ~ 
                # hour-day interaction
                te(hour, day, 
                   k=c(24, 7),
                   bs=c('cr','cr')) +
                # hour-temp interaction
                te(hour, temp_ak, 
                   k=c(24, 100),
                   bs=c('cr','cr')) +
                # temp-day interaction
                te(temp_ak, day, 
                   k=c(100, 7),
                   bs=c('cr','cr')) +
                    daysSinceStart + xmasbreak
                  ,
                  data = mdf,
                  family = gaussian)
summary(gam_prelim)$r.sq

# Define data to model: five years worth up to 2018. NO higher order interactions based on day*hour*temp plot. Also make day integer so it can be modelled smoothly
mdf <- dd %>%
  mutate(dayint=as.integer(day)) %>%
  filter(time>='2017-01-01' & time<='2018-01-31')

# ---------------------------------------------------------------
# Define data to model: five years worth up to 2018. NO higher order interactions based on day*hour*temp plot. Also make hour character to allow complex shapes
mdf <- dd %>%
  mutate(hour=as.character(hour)) %>%
  filter(time>='2017-01-01' & time<='2018-01-31')
mMR_int <- lm(load ~ temp_ak + day + hour + 
                temp_ak:day + day:hour + temp_ak:hour +
                xmasbreak + daysSinceStart, data=mdf)
summary(mMR_int) # lots of highly sig values
AIC(mMR_int)
gc()

# See some fitted vals for both a summer and a winter month
mymod <- mMR_int
mdf %>%
  mutate(fitted=predict(mymod)) %>%
  mutate(period=ifelse(time>='2017-06-14' & time<='2017-07-07', 'june',
                       ifelse(time>='2017-12-14' & time<='2018-01-07', 'dec', 'other'))) %>%
  filter(period %in% c('june','dec')) %>%
  select(fitted, load, time, period) %>%
  gather(key, value, -time, -period) %>%
  ggplot(aes(time, value, colour=key)) + geom_line() + 
  facet_wrap(~period, scales='free', ncol=1)  +
  ggsave(paste0('predictedVsFitted_MR_int.png'), height=6, width=10)
# ==> june not bad, dec terrible!

# resids plot
mdf %>%
  mutate(resids=resid(mymod)) %>%
  sample_n(1000) %>%
  select(resids, load, temp_ak, day, hour, xmasbreak, daysSinceStart) %>%
  gather(key, value, -resids) %>%
  ggplot(aes(value, resids, colour=key)) + geom_point(alpha=0.1) + geom_smooth(se=F) +
  facet_wrap(~key, scales='free') + theme(legend.position = 'none') + 
  ggsave(paste0('resids_MR_int.png'), height=6, width=10)
# ==> clear patterning in reids for temp and days since start (which is a proxy for temp) - clearly need to specify temp as a nonlinear function. (was hoping - wishfully - that MR model would be ok)

str(dd)




# regression with ARIMA errors: same predictors tho






















gc()