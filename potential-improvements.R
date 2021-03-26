#________________________________________________________________

# Model requirements based on EDA:
#________________________________________________________________

# - long-term trend effect which can probably be linear
# - nonlinear hour effect
# - Non-linear temperature effect
# - day of week effect - At least separating out Monday to Friday from remaining days (and public hols?)
# - Time of year effect, with either a dummy variable for Christmas/New Year or a flexible enough model to account for this. Aside from this temperature alone is probably enough
# - hr of day explains a lot more than day of week, if i have to choose
# - interactions:
# -- between hr and day, interaction defo required
# -- Between temperature and day of week - although main effects only could suffice
# -- between temperature and hour of day
# -- Possibly others, including higher order interactions

#________________________________________________________________

# POTENTIAL IMPROVEMENTS
#________________________________________________________________

# - Include public holidays
# - make dummy variable for Christmas/New Year period Based on day of year rather than week of year - or include a more flexible spline to account for this properly.
# - Use model averaging/ensembling to decide which city's temperature variables to include. 
# - Build in power supply rather than just demand, with data on hydro lake levels etc
# - include higher order interactions, if data can support
# - Test different training windows
# - Rather than specifying day as a category, just add a frequency that accounts for continuous changing across the week
# - NA interpolation: Use method that doesn't rely on knowing future values in advance
# - didnt investigate ARIMA models / stl ARIMA models - cos unsure how they can handle nonlinear interactions, which EDA clearly showed are a thing
# - try different spline methods - B- & cubic rather than mgcv default of 'thin plate' - but in practice similar, and cubic likely best if you have a lot of data. See https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package. see also 'bs' param in gam() function, e.g. s(temp_ak, bs='cr', k=10) or s(temp_ak, bs='ps', k=10)
# - ensemble models to combine best of both worlds
# - factor in seasonality separate from temperature - or at least test if required
# - check vars for MR model
# - autocorrelation, if i havne't factored in
# - public holiday/rain interaction
