#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'effects','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

#reformat variables
g$site = factor(g$site)
g$days = as.numeric(g$days)
g$hour = as.numeric(str_sub(g$hour,1,2))

#run gam
mx = gam(log(call_duration) ~
           s(wdsp, bs = 'tp') +
           s(days, bs = 'tp') +
           s(hour, bs = 'cc') +
           s(fraction, bs ='tp') +
           #te(temp,days, bs = c('tp','tp')) +
           #te(days,fraction, bs = c('tp','tp')) +
           s(site, bs = 're'),
         data = g,
         method ='REML',
         family = gaussian(link = 'identity'))

summary(mx)
gam.check(mx)
res = simulateResiduals(mx, plot = T)
model_performance(mx)
gratia::appraise(mx)
gratia::draw(mx)

#END