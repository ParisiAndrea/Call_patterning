library(MuMIn)

mx_comb = dredge(mx)
head(mx_comb,6)

#run gam with only + varables
mx2 = gam(call_duration ~
            s(temp2, bs ='cr', k = 20) +
            s(wdsp, bs = 'cr', k = 20) +
            #s(cloud, bs = 'tp', k=20) +
            s(fraction, bs ='cr', k=20) +
            s(hour,bs='cc',k=8) +
            s(days, bs= 'cr',k=12) +
            #ti(cloud,temp2,bs = c('tp','tp'))+
            ti(fraction,cloud, bs = c('cr','cr')) +
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
res = simulateResiduals(mx2, plot = T, re.form = NULL)

{
  plotResiduals(res, form = g$temp2)
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