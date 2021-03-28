
#________________________________________________________________

# loop thru each candidate model and report MAPE and plot fitted vs predicted for *** test data ***
#________________________________________________________________

# specify params
models <- list(gam_te, gam_by, mr_int, mr_int_poly, mr_int_poly_4, 
               m_int_poly_xmasInteraction)
plotnames <- list('gam_te_testdf', 'gam_by_testdf', 'mr_int_testdf',
                  'mr_int_poly_testdf', 'mr_int_poly4_testdf',
                  'mr_int_poly_xmasInteraction')

# data check then loop
if(length(models) !=length(plotnames)) stop('check models and plotnames')
for(i in 1:length(models)){
  cat(paste0('MAPE for ', plotnames[[i]], ': ', 
               report_mape(testdf, models[[i]]), '\n'))
  fitted_vals_plot(newdata=testdf, models[[i]], plotnames[[i]], panels=5)
}

#________________________________________________________________

# retrain, report mape and plot fitted vals *** for holdout data ****
#________________________________________________________________

retraindf <- filter(dd, time>=(as.Date(startdate)+365) & 
                      time<=(as.Date(enddate)+365))
m_int_poly_xmasInteraction_retrained <- 
  fit_best_polynomial_model(form=
                              'load ~ 
       poly1(temp_ak) + daychar + hourchar +
       daysSinceStart + weekchar + xmasbreak +
       poly2(temp_ak):daychar + poly3(temp_ak):hourchar + 
       poly4(temp_ak):xmasbreak +
       daychar:hourchar + daychar:xmasbreak + hourchar:xmasbreak', 
                            data=retraindf, maxorder=3)

report_mape(holdoutdf, m_int_poly_xmasInteraction_retrained)
fitted_vals_plot(newdata=holdoutdf, m_int_poly_xmasInteraction_retrained,
                 'm_int_poly_xmasInteraction_retrained', panels=5)

#________________________________________________________________

# for winning model, check sensitivity to climate extremes ----
#________________________________________________________________

mape_by_temperature(testdata=holdoutdf, 
                    m_int_poly_xmasInteraction_retrained,
                    'm_int_poly_xmasInteraction_retrained')

gc()