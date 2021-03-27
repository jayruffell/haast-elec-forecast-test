#________________________________________________________________

# Initial model: multiple regression with interactions on 3yr data, and linear temp effect. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# read in data
source('read-in-data-and-helper-functions.r')

#++++++++++++++
# fit model
#++++++++++++++

mr_int <- lm(load ~ temp_ak + daychar + hourchar + 
                temp_ak:daychar + daychar:hourchar + temp_ak:hourchar +
                hol + daysSinceStart, data=mdf)
summary(mr_int) # lots of highly sig values
AIC(mr_int)

#++++++++++++++
# model validation
#++++++++++++++

resids_plot(mydf = mdf, mymod= mr_int, mymod_char='mr_int')

gc()