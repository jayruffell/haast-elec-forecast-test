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
                                     daychar + hol
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

gc()




