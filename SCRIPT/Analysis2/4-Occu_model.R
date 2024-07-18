#sc = cbind(lat, month)

ufo = unmarkedFrameOccu(y = det, siteCovs = lat, obsCovs = oc)

ufo
summary(ufo)

fm = occu(formula = ~ temp + wdsp + prec + cloud*fraction
          ~ lat,
          data = ufo)

summary(fm)
plot(residuals(fm))
dredge(fm)

fm1 = occu(formula = ~ wdsp + cloud + fraction 
          ~ 1,
          data = ufo)

summary(fm1)

#plot

pl = list()

for (i in c('wdsp','cloud','fraction')) {
  
  ah = plotEffectsData(fm1, 'det', i)
  
  d0 = ggplot(ah, aes(covariateValue, Predicted)) +
    geom_line(size = 1) +
    geom_ribbon(aes(x= covariateValue,
                    y = Predicted,
                    ymax = upper,
                    ymin = lower),
                alpha = .1) +
    xlab(i) +
    ylab('Detection probability') +
    theme_classic(15)
  
  pl[[i]] = d0
  
}

ggarrange(plotlist = pl,
          nrow = 1)

attr(, 'scaled:scale')

t$temp2 * attr(t$temp2, 'scaled:scale') + attr(t$temp2, 'scaled:center')



