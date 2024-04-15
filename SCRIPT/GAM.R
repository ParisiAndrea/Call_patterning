#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance',
         'effects','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

g$site = factor(g$site)
g$days = as.numeric(g$days)
g$hour = as.numeric(str_sub(g$hour,1,2))

mx = gam(call_duration ~
           s(temp, bs = 'tp') +
           s(rhum, bs = 'tp') +
           s(wdsp, bs = 'tp') +
           s(days, bs = 'cr') +
           s(hour, bs = 'cc') +
           s(fraction, bs = 'cc') +
           s(site, bs = 're'),
         data = g,
         method ='REML',
         family = Gamma(link = 'log'))

summary(mx)
gam.check(mx)
res = simulateResiduals(mx, plot = T)
model_performance(mx)
gratia::appraise(mx)
gratia::draw(mx)

#END