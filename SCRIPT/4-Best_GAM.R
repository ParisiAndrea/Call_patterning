library(MuMIn)

mx_comb = dredge(mx)
head(mx_comb,1)

#run gam with only + varables
mx2 = gam(log(call_duration) ~
           s(temp, bs ='tp') +
           s(wdsp, bs = 'tp') +
           s(fraction, bs ='tp') +
           s(hour, bs = 'cc') +
           s(days, bs = 'tp') +
            te(temp, days, bs = c('tp','tp')) +
            te(fraction, days, bs = c('tp','tp')) +
            te(hour, days, bs = c('cc','tp')) +
           s(site, bs = 're'),
         data = g,
         na.action = "na.fail",
         method ='REML',
         family = gaussian(link = 'identity'))

#summary
summary(mx2)
gratia::draw(mx2)

#Diagnostic
DHARMa::simulateResiduals(mx2, plot=T)
DHARMa::testOverdispersion(mx2)
DHARMa::testOutliers(mx2)
acf(mx2)
gam.check(mx2)

#temporal autocorrealtion?
bacf1 <- acf(na.omit(lag(residuals(mx2))), plot = T)

#END