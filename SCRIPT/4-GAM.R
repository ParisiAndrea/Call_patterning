#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance',
         'effects','lmtest', 'DHARMa'), 
       require, 
       character.only=T)

#reformat variables
g$site = factor(g$site)
g$days = as.numeric(g$days)
g$hour = as.numeric(str_sub(g$hour,1,2))
g$call_duration = log(g$call_duration)

#run gam
mx = gam(call_duration ~
           s(temp1, bs ='tp') +
           s(wdsp, bs = 'tp') +
           s(cloud, bs = 'tp') +
           s(fraction, bs ='tp') +
           s(hour, bs = 'cc') +
           s(days, bs = 'tp') +
           ti(temp1, days, bs = c('tp','tp')) +
           ti(temp1, cloud, bs = c('tp','tp')) +
           ti(fraction, cloud, bs = c('tp','tp')) +
           s(site, bs = 're'),
         data = g,
         na.action = "na.fail",
         method ='REML',
         family = gaussian(link = 'identity'))

summary(mx)
gam.check(mx)
model_performance(mx)
gratia::appraise(mx)
gratia::draw(mx)
concurvity(mx, full = FALSE)

#DHARMa
res = simulateResiduals(mx, plot = T)

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

acf(residuals(mx))

#END