# using 'by' instead of tensor cos 

# RESULTS: slightly better AIC than gam_te


#________________________________________________________________

# read in data ----
#________________________________________________________________

source('read-in-data-and-helper-functions.r')

#________________________________________________________________

# gam ----
#________________________________________________________________

#++++++++++++++
# model
#++++++++++++++
tmp <- mutate(mdf, hourchar=as.factor(hourchar)) # needed for 'by'
start <- Sys.time(); gam_by <- gam(load ~
                                     # day-temp interaction
                                     s(temp_ak, bs='cs', by=hourchar, k=10) +
                                     # main effects needed?
                                     s(temp_ak, bs='cs', k=10) + 
                                     hourchar + 
                                     s(day, k=7, bs='cr') +
                                     daysSinceStart + hol,
              data = tmp,
              family = gaussian)
gam_report(starttime = start, mygam = gam_by)

#++++++++++++++
# model validation
#++++++++++++++

gam.check(gam_by)
resids_plot(mydf = mdf, mymod= gam_by, mymod_char='gam_by')

#++++++++++++++
# visualise modelled relationships
#++++++++++++++

plot(gam_te, all.terms=T, pages=1)
# vis.gam(gam_te, view=c('hour', 'temp_ak'), theta =170, phi = 5, zlab = "load")
# vis.gam(gam_te, view=c('day', 'temp_ak'), theta =170, phi = 5, zlab = "")
# vis.gam(gam_te, view=c('hol', 'temp_ak'), theta =170, phi = 5, zlab = "")
# graphics.off()
