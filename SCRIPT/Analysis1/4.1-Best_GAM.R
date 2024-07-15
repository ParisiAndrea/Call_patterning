library(MuMIn)

mx_comb = dredge(mx)
head(mx_comb,6)

#run gam with only + varables
mx2 = gam(call_duration ~
            s(temp1, bs ='tp') +
            s(wdsp, bs = 'tp') +
            s(cloud, bs = 'tp', k=20) +
            s(fraction, bs ='tp', k=50) +
            s(hour,bs='cc',k=24) +
            s(days, bs= 'tp',k=30) +
            ti(cloud,temp1,bs = c('tp','tp'))+
            ti(fraction,cloud, bs = c('tp','tp')) +
            rival +
            s(site, bs = 're'),
         data = g,
         na.action = "na.fail",
         method ='REML',
         family = gaussian('identity'))

#summary
summary(mx2)
gam.check(mx2)
model_performance(mx2)
gratia::appraise(mx2)
print(plot(getViz(mx2), allTerms = TRUE), pages = 1)
concurvity(mx2, full = F)

#DHARMa
res = simulateResiduals(mx2, plot = T)

{
  plotResiduals(res, form = g$temp1)
  plotResiduals(res, form = g$wdsp)
  plotResiduals(res, form = g$cloud)
  plotResiduals(res, form = g$fraction)
  plotResiduals(res, form = g$days)
  plotResiduals(res, form = g$hour)
  
}

DHARMa::testDispersion(res)
DHARMa::testOutliers(res)

acf(residuals(mx2))

pcaf(residuals(mx2))

#END