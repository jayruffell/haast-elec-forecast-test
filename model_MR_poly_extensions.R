
# *** read in model_MR_poly first ****

#________________________________________________________________

# unequal variances for holiday effect ----
#________________________________________________________________

# eda, downsampling non-hol days
hol_ds <- dd %>%
  filter(hol==0) %>%
  sample_n(10000) %>%
  bind_rows(
    dd %>%
      filter(hol==1))
ggplot(hol_ds, aes(hour, load, colour=xmasbreakf)) + geom_jitter(alpha=0.5) +
  ggsave('holVsNonholVariance.png', height=6, width=10)
ggplot(hol_ds, aes(temp_ak, load, colour=hourchar)) + 
  geom_point(alpha=0.5) + geom_smooth(se=F) + facet_wrap(~xmasbreakf) +
  ggsave('holVsNonholVariance2.png', height=6, width=10)

#++++++++++++++
# fix with gls?
#++++++++++++++

glsbase <- gls(load ~ poly(temp_ak, 3) + daychar + hourchar + 
              poly(temp_ak, 3):daychar + 
              poly(temp_ak, 3):hourchar + daychar:hourchar + 
              holf + daysSinceStart + weekchar, data=mdf)
glsVarIdent <- gls(load ~ poly(temp_ak, 3) + daychar + hourchar + 
              poly(temp_ak, 3):daychar + 
              poly(temp_ak, 3):hourchar + daychar:hourchar + 
              holf + daysSinceStart + weekchar, data=mdf,
              weights=varIdent(form=~1|hol)) # long time to fit
AIC(glsbase, glsVarIdent)
fitted_vals_plot(newdata=mdf, glsVarIdent, 'mr_int_poly_HolVariance', 
                 panels=5,
                 mystartdate='2016-07-01', myenddate='2017-07-01')
# ==> RESULT: no! pearson resids may be correcteed, but predictions are still rubbish! And mape is way worse than std model. Think problem is actually that it's not the resids of the predictsion that need fixing but rather the actual hour/day/temp relationship that changes.
# hol_ds %>%
#   mutate(resids=predict(glsVarIdent, newdata=hol_ds)-load) %>%
#   ggplot(aes(resids, fill=holf)) + geom_density(alpha=0.3)
# ggplot(aes(holf, resids)) + geom_point(alpha=0.3)
# tmp <- update(mr_int_poly, ~ . + hol:temp_ak + hol:hourchar + hol)
gc()

#++++++++++++++
# Fix with interactions
#++++++++++++++

m_int_poly_xmasInteraction <- 
  fit_best_polynomial_model(form=
                              'load ~ 
       poly1(temp_ak) + daychar + hourchar +
       daysSinceStart + weekchar + xmasbreak +
       poly2(temp_ak):daychar + poly3(temp_ak):hourchar + 
       poly4(temp_ak):xmasbreak +
       daychar:hourchar + daychar:xmasbreak + hourchar:xmasbreak', 
                            data=mdf, maxorder=3)

resids_plot(mydf = mdf, mymod= m_int_poly_xmasInteraction, 
            mymod_char='mr_int_poly_xmasInteraction')

#++++++++++++++
# Need to account for autocorrelation?
#++++++++++++++

modResids <- resid(m_updated)
acf(modResids)
# pick params automatically, as described here https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
auto.arima(ts(modResids), stationary = T) # ARMA(3, 1)

m_int_poly_xmasInteraction_ARMA <- gls(load ~ 
                  # main effects
                  poly(temp_ak, 3) + daychar + hourchar +
                  daysSinceStart + weekchar + xmasbreak +
                  # interations with temp
                  poly(temp_ak, 3):daychar + poly(temp_ak, 3):hourchar + 
                  poly(temp_ak, 3):xmasbreak +
                  # day/hour interactions
                  daychar:hourchar + daychar:xmasbreak + hourchar:xmasbreak, 
                  data = mdf, correlation = corARMA(p = 3, q = 1))
resids_plot(mydf = mdf, mymod= m_int_poly_xmasInteraction, 
            mymod_char='mr_int_poly_xmasInteraction')

#++++++++++++++
# Fix nonlinearity in dayssincestart
#++++++++++++++

# debugonce(fit_best_polynomial_model)
m_int_poly_xmasInteraction2 <- 
  fit_best_polynomial_model(form=
  'load ~ 
       poly1(temp_ak) + daychar + hourchar +
       poly5(daysSinceStart) + weekchar + xmasbreak +
       poly2(temp_ak):daychar + poly3(temp_ak):hourchar + 
       poly4(temp_ak):xmasbreak +
       daychar:hourchar + daychar:xmasbreak + hourchar:xmasbreak', 
     data=mdf, maxorder=3)


resids_plot(mydf = mdf, mymod= m_int_poly_xmasInteraction2, 
            mymod_char='mr_int_poly_xmasInteraction2')


AIC(m_int_poly_xmasInteraction2, m_int_poly_xmasInteraction)









