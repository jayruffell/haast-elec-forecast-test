
# POTENTIAL IMPROVEMENTS
# - Include public holidays
# - make dummy variable for Christmas/New Year period Based on day of year rather than week of year - or include a more flexible spline to account for this properly.
# - Use model averaging/ensembling to decide which city's temperature variables to include. 
# - Build in power supply rather than just demand, with data on hydro lake levels etc
# - include higher order interactions, if data can support
# - Test different training windows
# - Rather than specifying day as a category, just add a frequency that accounts for continuous changing across the week
# - NA interpolation: Use method that doesn't rely on knowing future values in advance
