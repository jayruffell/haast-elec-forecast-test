#________________________________________________________________

# GAM with tensor (most flexible) interactions on 3yr data. No seasonal effect cos temp will soak this up ----
#________________________________________________________________

# from here https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
# nb also see choose.k documentation (mgcv package) for important model checks for whether model is over/underfitted
# also here https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package for type of spline to use & choice of k
# -- Thin plate splines tend to have better RMSE performance than the other three you mention but are more computationally expensive to set up. Unless you have a reason to use the P or B spline bases, use thin plate splines unless you have a lot of data and if you have a lot of data consider the cubic spline option.
# -- The choice of k is important and the default is arbitrary and something you want to check (see gam.check()), but the critical observation is that you want to set k to be large enough to contain the envisioned dimensionality of the underlying function you are trying to recover from the data. In practice, one tends to fit with a modest k given the data set size and then use gam.check() on the resulting model to check if k was large enough. If it wasn't, increase k and refit. Rinse and repeat...

#________________________________________________________________

# read in data ----
#________________________________________________________________

source('read-in-data.r')

#________________________________________________________________

# tensor-based gams ----
#________________________________________________________________

# nb if i'm using 'bam' function this is faster version of gam, for use on 'tens of thousands of obs' (my data has 10K at the mo)
# note also that unlike gam(), bam says default spline fit takes too long and should be avoided.

#++++++++++++++
# model
#++++++++++++++
start <- Sys.time(); gam_te <- bam(load ~
                                     # hour-day interaction
                                     # te(hour, day,
                                     #    k=c(24, 7),
                                     #    bs=c('cr','cr')) +
                                     # hour-temp interaction
                                     te(hour, temp_ak,
                                        k=c(24, 10),
                                        bs=c('cc','cr')) +
                                     # # temp-day interaction
                                     # te(temp_ak, day,
                                     #    k=c(100, 7),
                                     #    bs=c('cr','cr')) +
                                     s(day, k=7, bs='cr') +
                                     daysSinceStart + hol
                                   ,
                                   data = mdf,
                                   family = gaussian,
                                   discete=TRUE)# , control = gam.control(trace = TRUE))
gam_report(starttime = start, mygam = gam_te)

#++++++++++++++
# model validation
#++++++++++++++

gam.check(gam_te)
fitted_vals_summer_winter_plot(mydf = mdf, mymod= gam_te, mymod_char='gam_te')
resids_plot(mydf = mdf, mymod= gam_te, mymod_char='gam_te')

#++++++++++++++
# visualise modelled relationships
#++++++++++++++

plot(gam_te, all.terms=T, pages=1)
vis.gam(gam_te, view=c('hour', 'temp_ak'), theta =170, phi = 5, zlab = "load")
vis.gam(gam_te, view=c('day', 'temp_ak'), theta =170, phi = 5, zlab = "")
vis.gam(gam_te, view=c('hol', 'temp_ak'), theta =170, phi = 5, zlab = "")
graphics.off()





#________________________________________________________________

# prelimnary gams that didn't work ----
#________________________________________________________________

#++++++++++++++
# GAM with interactions via 'by' term  ** TOO SLOW ***
#++++++++++++++
# 
# # from here https://stats.stackexchange.com/questions/32730/how-to-include-an-interaction-term-in-gam
# 
# tmp <- mutate(mdf, dayf=as.factor(daychar))
# str(tmp)
# Sys.time()
# gam_by <- gam(load ~
#                 # hour-day interaction with cyclic smooth
#                 s(hour, bs='cc', by=dayf, k=24) +
#                 # day-temp interaction
#                 s(temp, bs='cs', by=dayf, k=200) +
#                 # # temp-hour interaction
#                 # te(temp_ak, day,
#                 #    k=c(100, 7),
#                 #    bs=c('cr','cr')) +
#                 daysSinceStart + hol
#               ,
#               data = tmp,
#               family = gaussian)
# Sys.time()
# summary(gam_by)# $r.sq
# 
# # plot
# layout(matrix(1:4, nrow = 1))
# plot(gam_by, shade = TRUE)
# graphics.off()

# #++++++++++++++
# # prelim gam - like first one from above link. 
# #++++++++++++++
# 
# # 'cr' and 'ps' are cubic- and p-splines, the ones chosen in the link. k should be set to maximum no. of unique values (and knots are then chosen automatically I believe)
# # from above stackoverflow link: 
# gam_prelim <- gam(load ~ s(hour, bs='cr', k = 24) +
#                s(day, bs='cr', k = 7) +
#                daysSinceStart + hol +
#                s(temp_ak, bs='cr', k=200) # max 684 uniques values - but see above for how to check this. Note I'm probably not using a good value of k!
#                ,
#              data = mdf,
#              family = gaussian)
# summary(gam_prelim)$r.sq
# 
# # check if k vals are ok - # see printout, it tells you what to do :)
# gam.check(gam_prelim) 
# 
# # plot 
# layout(matrix(1:4, nrow = 1))
# plot(gam_prelim, shade = TRUE)
# graphics.off()


