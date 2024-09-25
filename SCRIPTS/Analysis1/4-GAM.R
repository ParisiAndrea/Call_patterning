#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance',
         'effects','lmtest', 'DHARMa','mgcViz'), 
       require, 
       character.only=T)

#reformat variables
g$site = factor(g$site)
g$days = as.numeric(g$days)
g$hour = as.numeric(hour(g$time))
g$call_duration = log(g$call_duration)

#run gam
mx = bam(call_duration ~
           s(temp2, bs ='cr', k = 20) +
           s(wdsp, bs = 'cr', k = 20) +
           s(cloud, bs = 'cr', k = 20) +
           s(fraction, bs ='cr', k = 20) +
           s(hour,bs='cc',k=10) +
           s(days, bs= 'cr',k=20) +
           ti(cloud,temp2,bs = c('cr','cr')) +
           ti(fraction,cloud, bs = c('cr','cr')) +
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

#shapiro.test(residuals(mx))

#DHARMa
res = simulateResiduals(mx, plot = T)

{
plotResiduals(res, form = g$temp2)
plotResiduals(res, form = g$wdsp)
plotResiduals(res, form = g$cloud)
plotResiduals(res, form = g$fraction)
plotResiduals(res, form = g$days)
plotResiduals(res, form = g$hour)
plotResiduals(res, form = g$site)
}

DHARMa::testDispersion(res)
DHARMa::testOutliers(res)

acf(residuals(mx))

#END