library(MuMIn)

#try each combination of explanatory variables
mx_comb = dredge(mx)

#best models
head(mx_comb,6)

#run gam with lowest AICc
mx2 = gam(call_duration ~
            s(temp2, bs ='cr', k = 20) +
            s(wdsp, bs = 'cr', k = 20) +
            s(fraction, bs ='cr', k=20) +
            s(hour,bs='cc',k=8) +
            s(days, bs= 'cr',k=12) +
            ti(fraction,cloud, bs = c('cr','cr')) +
            s(site, bs = 're'),
         data = g,
         na.action = "na.fail",
         method ='REML',
         family = gaussian('identity'))

#check output
summary(mx2)

#k-basis
k.check(mx2)

#model performance
model_performance(mx2)

#diagnostics
gratia::appraise(mx2)
concurvity(mx2, full = FALSE)
shapiro.test(residuals(mx2))

#visual
print(plot(getViz(mx2), allTerms = TRUE), pages = 1)

#Full DHARMa diagnostics
res = simulateResiduals(mx2, plot = T)

#plot each covariate against residuals
{
  plotResiduals(res, form = g$temp2)
  plotResiduals(res, form = g$wdsp)
  plotResiduals(res, form = g$cloud)
  plotResiduals(res, form = g$fraction)
  plotResiduals(res, form = g$days)
  plotResiduals(res, form = g$hour)
  plotResiduals(res, form = g$site)
}

#Dispersion & outliers
DHARMa::testDispersion(res)
DHARMa::testOutliers(res)

#temporal autocorrealation
acf(residuals(mx2))


#END