#________________________________________________________________

# Initial model: multiple regression with interactions on 3yr data, and linear temp effect. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# read in data
source('read-in-data.r')

#++++++++++++++
# fit model
#++++++++++++++

mMR_int <- lm(load ~ temp_ak + daychar + hourchar + 
                temp_ak:daychar + daychar:hourchar + temp_ak:hourchar +
                hol + daysSinceStart, data=mdf)
summary(mMR_int) # lots of highly sig values
AIC(mMR_int)

#++++++++++++++
# model validation
#++++++++++++++

fitted_vals_summer_winter_plot(mydf = mdf, mymod= mMR_int, mymod_char='mr_int')
resids_plot(mydf = mdf, mymod= mMR_int, mymod_char='mr_int')

gc()