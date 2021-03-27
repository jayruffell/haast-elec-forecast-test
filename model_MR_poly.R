#________________________________________________________________

# Initial model: multiple regression with interactions on 3yr data, and linear temp effect. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# read in data
source('read-in-data-and-helper-functions.r')

#++++++++++++++
# fit model
#++++++++++++++

# use AIC to find best model - from linear to maxordr for main and interaction effects
mr_int_poly <- fit_best_polynomial_model(form=
                            'load ~ poly1(temp_ak) +
                            daychar + hourchar +
                            poly2(temp_ak):daychar +
                            poly3(temp_ak):hourchar +
                            daychar:hourchar +
                          hol + daysSinceStart + weekchar + xmasbreak',
                          data=mdf, maxorder=3)

#++++++++++++++
# PLaceholder: do auto.arima() on resids, and then refit with this corr structure
#++++++++++++++

#++++++++++++++
# model validation
#++++++++++++++

fitted_vals_summer_winter_plot(mydf = mdf, mymod= mr_int_poly, mymod_char='mr_int_poly')
fitted_vals_plot(newdata=mdf, mr_int_poly, 'mr_int_poly', panels=5,
                 mystartdate='2016-07-01', myenddate='2017-07-01')
resids_plot(mydf = mdf, mymod= mr_int_poly, mymod_char='mr_int_poly')

gc()