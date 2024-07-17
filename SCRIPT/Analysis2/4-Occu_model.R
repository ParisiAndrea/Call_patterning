ufo = unmarkedFrameOccu(y = det, siteCovs = lat, obsCovs = oc)

ufo
summary(ufo)

fm = occu(formula = ~ temp + wdsp + cloud + fraction 
          ~ 1,
          data = ufo)

summary(fm)
plot(residuals(fm))
dredge(fm)