# specify test data dates
period <- 365 # days ahead to forecast
teststart <- as.Date(enddate)+1  # start date
testend <- teststart + period

testdf <- filter(dd, time>=teststart & time <=testend)
report_mape(testdf, glsVarIdent) # 3.5% across a whole year yeah boi! DOn't even need to loop thru with step-ahead CV, cos can predict really far out already!

report_mape(testdf, mr_int_poly)

fitted_vals_plot(testdf, mr_int_poly, 'mr_int_poly_testdata', panels=5)
fitted_vals_plot(testdf, glsVarIdent, 'mr_int_poly_holVariance_testdata', panels=5)

