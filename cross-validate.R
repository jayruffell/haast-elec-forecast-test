
#________________________________________________________________

# loop thru each model and report MAPE and plot fitted vs predicted for test data
#________________________________________________________________

# specify params
models <- list(gam_te, gam_by, mr_int, mr_int_poly)
plotnames <- list('gam_te_testdf', 'gam_by_testdf', 'mr_int_testdf',
                  'mr_int_poly_testdf')

# data check then loop
if(length(models) !=length(plotnames)) stop('check models and plotnames')
for(i in 1:length(models)){
  cat(paste0('MAPE for ', plotnames[[i]], ': ', 
               report_mape(testdf, models[[i]]), '\n'))
  fitted_vals_plot(newdata=testdf, models[[i]], plotnames[[i]], panels=5)
}

#________________________________________________________________

# for winning model, check sensitivity to climate extremes ----
#________________________________________________________________

mape_by_temperature(testdata=testdf, mymod=mr_int_poly, 'm_int_poly')

gc()