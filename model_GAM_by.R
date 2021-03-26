# using 'by' instead of tensor cos resids plot shows that hour var is not fitted well - use hourchar to see if that helps instead.
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
start <- Sys.time(); gam_by <- gam(load ~
                                     # day-temp interaction
                  s(temp, bs='cs', by=daychar, k=10) +
                  daychar + # main effect needed?
                daysSinceStart + hol
              ,
              data = tmp,
              family = gaussian)

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


