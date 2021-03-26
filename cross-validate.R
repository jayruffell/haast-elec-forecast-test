# specify test data dates
period <- 365 # days ahead to forecast
teststart <- as.Date(enddate)+1  # start date
testend <- teststart + period
tail(testdf)
testdf <- filter(dd, time>=teststart & time <=testend)
report_mape(testdf, mr_int_poly) # 3.5% across a whole year yeah boi! DOn't even need to loop thru with step-ahead CV, cos can predict really far out already!
fitted_vals_plot(testdf, mr_int_poly, 'mr_int_poly')


