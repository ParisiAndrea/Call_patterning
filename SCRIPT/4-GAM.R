#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance',
         'effects','lmtest', 'DHARMa','mgcViz'), 
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
           s(cloud, bs = 'tp', k=20) +
           s(fraction, bs ='tp', k=26) +
           s(hour,bs='cc',k=24) +
           s(days, bs= 'tp',k=30) +
           ti(cloud,temp1,bs = c('tp','tp')) +
           ti(fraction,cloud, bs = c('tp','tp')) +
           #rival +
           s(site, bs = 're'),
         data = g,
         knots=list(hour=c(0,23)),
         na.action = "na.fail",
         method ='REML',
         family = gaussian('identity'))


summary(mx)
k.check(mx)
model_performance(mx)
gratia::appraise(mx)
print(plot(getViz(mx), allTerms = TRUE), pages = 1)
concurvity(mx, full = FALSE)

shapiro.test(residuals(mx))

#DHARMa
res = simulateResiduals(mx, plot = T)

{
plotResiduals(res, form = g$temp1)
plotResiduals(res, form = g$wdsp)
plotResiduals(res, form = g$cc)
plotResiduals(res, form = g$fraction)
plotResiduals(res, form = g$days)
plotResiduals(res, form = g$hour)

}

DHARMa::testDispersion(res)
DHARMa::testOutliers(res)

acf(residuals(mx))

#END