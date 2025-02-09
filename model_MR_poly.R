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
                            daysSinceStart + weekchar + hol',
                          data=mdf, maxorder=3)

#++++++++++++++
# try separeate model with higher order polys. Gonna do on test data sep cos AIC may overfit
#++++++++++++++

mr_int_poly_4 <- fit_best_polynomial_model(form=
                                           'load ~ poly1(temp_ak) +
                            daychar + hourchar +
                            poly2(temp_ak):daychar +
                            poly3(temp_ak):hourchar +
                            daychar:hourchar +
                            daysSinceStart + weekchar + hol',
                                         data=mdf, maxorder=4)
#++++++++++++++
# model validation
#++++++++++++++

resids_plot(mydf = mdf, mymod= mr_int_poly, mymod_char='mr_int_poly')
resids_plot(mydf = mdf, mymod= mr_int_poly_4, mymod_char='mr_int_poly_4')

gc()