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

# Initial model: multiple regression with interactions on 3yr data, and linear temp effect. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# read in data
source('read-in-data.r')

# Define data to model: five years worth up to 2018. NO higher order interactions based on day*hour*temp plot. Also make hour character to allow complex shapes
mdf <- dd %>%
  mutate(hour=as.character(hour)) %>%
  filter(time>='2013-01-01' & time<='2018-01-31')
mMR_int <- lm(load ~ temp_ak + day + hour + 
                temp_ak:day + day:hour + temp_ak:hour +
                xmasbreak + daysSinceStart, data=mdf)
summary(mMR_int) # lots of highly sig values
AIC(mMR_int)

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

gc()