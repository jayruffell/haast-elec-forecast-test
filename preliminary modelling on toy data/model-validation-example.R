#+++++++++++++++++++
# Model validation and resids
#+++++++++++++++++++

## *** from Hyndman book, resids chapter: ***
# For many (but not all) time series models, the residuals are equal to the difference between the observations and the corresponding fitted values.
# 
# Residuals are useful in checking whether a model has adequately captured the information in the data. A good forecasting method will yield residuals with the following properties:
#   
# - 1. The residuals are uncorrelated. If there are correlations between residuals, then there is information left in the residuals which should be used in computing forecasts.
# - 2. The residuals have zero mean. If the residuals have a mean other than zero, then the forecasts are biased.

# If either of these properties is not satisfied, then the forecasting method can be modified to give better forecasts. Adjusting for bias is easy: if the residuals have mean m then simply add m to all forecasts and the bias problem is solved. Fixing the correlation problem is harder, and we will not address it until Chapter 9.
# 
# In addition to these essential properties, it is useful (but not necessary) for the residuals to also have the following two properties.
# 
# - 3. The residuals have constant variance.
# - 4. The residuals are normally distributed.
# These two properties make the calculation of prediction intervals easier (see Section 3.5 for an example). However, a forecasting method that does not satisfy these properties cannot necessarily be improved. Sometimes applying a Box-Cox transformation may assist with these properties, but otherwise there is usually little that you can do to ensure that your residuals have constant variance and a normal distribution. Instead, an alternative approach to obtaining prediction intervals is necessary. Again, we will not address how to do this until later in the book.

mymod <-  m3 # from energy-forecasts-arima.R
myxreg <- xreg_3

# fitted vs predicted vals
elecdaily %>%
  as_tibble() %>%
  select(Demand) %>%
  mutate(period=1:nrow(.), 
         fitted=forecast(m3, xreg=myxreg)$fitted) %>%
  gather(key, value, -period) %>%
  ggplot(aes(period, value, colour=key)) + geom_line() 

#Resids plot
checkresiduals(mymod) # :(  fixable? is it cos non-stationary?

# check resids against fitted values
residsByPredictor <- elecdaily %>%
  as_tibble() %>%
  mutate(resids=residuals(mymod)) %>%
  gather(key, value, -resids) 
residsByPredictor %>%
  ggplot(aes(value, resids)) + geom_point() + geom_smooth() + facet_wrap(~key, scales='free') # ok, except for autocorrn

# heterosked. in workday var, or is tis just cos more obs. for weekedays?
residsByPredictor %>%
  filter(key=='WorkDay') %>%
  ggplot(aes(resids, fill=as.character(value))) + geom_density(alpha=0.5)

