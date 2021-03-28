

# *** NB must ensure vars getting plotted are the actual ones in the model, e.g. daychar vs day vs dayf ***

# would be nice to have this as a function to save replication, if I end up re-using this code...

# Specifiy model
mymod <- m_int_poly_xmasInteraction
mymod_char <- 'mr_int_poly_xmasInteraction'

# tmp-hr interaction
p1 <- create_newdata_for_fitted_vals_plot(mymod, 
                                    varsToPlot = c('temp_ak', 'hourchar')) %>%
  sample_n(1000) %>%
  ggplot(aes(temp_ak, fitted, colour=hourchar)) + geom_line() + 
  ylim(min(mdf$load), max(mdf$load)) + theme(legend.position = 'none')

# temp-day interaction
p2 <- create_newdata_for_fitted_vals_plot(mymod,
                                    varsToPlot = c('temp_ak', 'daychar')) %>%
  sample_n(1000) %>%
  ggplot(aes(temp_ak, fitted, colour=daychar)) + geom_line()  + 
  ylim(min(mdf$load), max(mdf$load))+ theme(legend.position = 'none')

# hour-day interaction
p3 <- create_newdata_for_fitted_vals_plot(mymod, 
                                    varsToPlot = c('hourchar', 'daychar')) %>%
  sample_n(1000) %>%
  mutate(hour=as.numeric(hourchar)) %>%
  ggplot(aes(hour, fitted, colour=daychar)) + geom_line()  + 
  ylim(min(mdf$load), max(mdf$load))+ theme(legend.position = 'none')

# days since start
p4 <- create_newdata_for_fitted_vals_plot(mymod, 
                                    varsToPlot = c('daysSinceStart')) %>%
  sample_n(1000) %>%
  ggplot(aes(daysSinceStart, fitted)) + geom_line()  + 
  ylim(min(mdf$load), max(mdf$load))

# Hol effect
p5 <- create_newdata_for_fitted_vals_plot(mymod, 
                                          varsToPlot = c('hol')) %>%
  sample_n(1000) %>%
  mutate(hol=as.factor(hol)) %>%
  ggplot(aes(hol, fitted)) + geom_point(size=4)  + 
  ylim(min(mdf$load), max(mdf$load))

# xmas effect
p5 <- create_newdata_for_fitted_vals_plot(mymod, 
                                          varsToPlot = c('xmasbreak')) %>%
  sample_n(1000) %>%
  # mutate(hol=as.factor(hol)) %>%
  ggplot(aes(xmasbreak, fitted)) + geom_point(size=4)  + 
  ylim(min(mdf$load), max(mdf$load))

# Week effect
p6 <- create_newdata_for_fitted_vals_plot(mymod, 
                                    varsToPlot = c('weekchar')) %>%
  sample_n(1000) %>%
  mutate(week=as.numeric(weekchar)) %>%
  ggplot(aes(week, fitted)) + geom_point()  + 
  ylim(min(mdf$load), max(mdf$load))

library(gridExtra)
grid.arrange(p1, p2, p3,p4,p5, p6) # visualise before saving
combined <- arrangeGrob(p1, p2, p3,p4,p5, p6)
ggsave(paste0('fittedRelationships_', mymod_char, '.png'), combined,
       height=6, width=10) 

gc()