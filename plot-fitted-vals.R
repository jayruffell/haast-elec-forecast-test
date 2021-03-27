
mymod <- mr_int_poly
varsToPlot <- c('temp_ak', 'hourchar', 'daychar')

# create dataframe of median values for all data
newdata <- mdf %>%
  mutate_if(is.numeric, median) %>%
  # replace factors and char with new median versions
  mutate(hourchar=as.character(hour),
         hourf=as.factor(hour),
         daychar=as.character(day),
         dayf=as.factor(day),
         # from read in data function
         daygrouped=ifelse(daychar %in% c('1', '5', '6', '7'), daychar, '0'),
         weekchar=as.character(week),
         weekf=as.factor(week),
         xmasbreakf=as.factor(xmasbreak),
         holf=as.factor(hol))
# replace the focal vars with treu values
newdata[, varsToPlot] <- mdf[, varsToPlot]
newdata %>%
  mutate(fitted=predict(mymod, newdata=newdata)) %>%
  sample_n(500) %>%
  ggplot(aes(temp_ak, fitted, colour=hourchar)) + geom_line()
  
