#________________________________________________________________

# Initial model: multiple regression with interactions on 3yr data, and linear temp effect. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# read in data
source('read-in-data.r')

#++++++++++++++
# fit model
#++++++++++++++

# use AIC to find best model - from linear to maxordr for main and interaction effects
maxorder <- 4
polyGrid = expand_grid(main=1:maxorder, tempday=1:maxorder,
                           temphour=1:maxorder) %>%
  mutate(aic=NA, numparams=NA)
cat(paste('\nfitting', nrow(polyGrid), 'polymodels:\n'))
start <- Sys.time()
for(i in 1:nrow(resultsdf)){
  mod <- lm(load ~ poly(temp_ak, polyGrid$main[i]) + 
              daychar + hourchar + 
              poly(temp_ak, polyGrid$tempday[i]):daychar + 
              daychar:hourchar + 
              poly(temp_ak, polyGrid$temphour[i]):hourchar +
              hol + daysSinceStart,
            data=mdf)
  polyGrid$aic[i] <- AIC(mod)
  polyGrid$numparams[i] <- length(mod$coefficients)
  cat(i)
}
cat(paste0('\nRun in ', 
           round(as.numeric(difftime(Sys.time(),start, units='mins')), 1),
           ' mins\n'))

# refit based on best - or smallest params within delta2 anyway
bestAIC <- polyGrid %>%
  filter(aic==min(aic)) %>%
  pull(aic)
candidates <- polyGrid %>%
  mutate(deltaAIC=aic-bestAIC) %>%
  filter(deltaAIC <=2)
finalparams <- distinct(filter(candidates, deltaAIC==min(deltaAIC)))

mr_int_poly <- lm(load ~ poly(temp_ak, finalparams$main) + 
                    daychar + hourchar + 
                    poly(temp_ak, finalparams$tempday):daychar + 
                    daychar:hourchar + 
                    poly(temp_ak, finalparams$temphour):hourchar +
                    hol + daysSinceStart,
                  data=mdf)
summary(mr_int_poly) # lots of highly sig values
AIC(mr_int_poly)

#++++++++++++++
# model validation
#++++++++++++++

fitted_vals_summer_winter_plot(mydf = mdf, mymod= mr_int_poly, mymod_char='mr_int_poly')
resids_plot(mydf = mdf, mymod= mr_int_poly, mymod_char='mr_int_poly')

gc()