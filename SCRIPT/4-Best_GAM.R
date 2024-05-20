library(MuMIn)

mx_comb = dredge(mx)
head(mx_comb,6)

#run gam with only + varables
mx2 = gam(call_duration ~
           #s(temp1, bs ='tp') +
           s(wdsp, bs = 'tp') +
           s(cloud, bs = 'tp') +
           #s(fraction, bs ='tp') +
           s(hour, bs = 'cc') +
           s(days, bs = 'tp') +
           ti(temp1, days, bs = c('tp','tp')) +
           ti(temp1, cloud, bs = c('tp','tp')) +
           ti(fraction, cloud, bs = c('tp','tp')) +
           s(site, bs = 'fs'),
         data = g,
         na.action = "na.fail",
         method ='REML',
         family = gaussian('identity'))

#summary
summary(mx2)
gam.check(mx2)
model_performance(mx2)
gratia::appraise(mx2)
gratia::draw(mx2)
concurvity(mx2, full = F)

#DHARMa
res = simulateResiduals(mx2, plot = T, n=1000)

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

#END