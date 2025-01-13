#build unmarked dataset with site and observation covariates

ufo = unmarkedFrameOccu(y = det, siteCovs = lat, obsCovs = oc)

#check
ufo
summary(ufo)

#global model
fm = occu(formula = ~ temp + wdsp + cloud*fraction
          ~ 1,
          data = ufo)

#check
summary(fm)
plot(residuals(fm))

#model selection
dredge(fm) 

#fwrite(x, 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/model_selection2.csv')

#best model
fm1 = occu(formula = ~ wdsp + cloud + fraction 
          ~ 1,
          data = ufo)

summary(fm1)

#plots

b1 = ggplot(plotEffectsData(fm1, 'det', 'wdsp'), aes(covariateValue, Predicted)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(x= covariateValue,
                  y = Predicted,
                  ymax = upper,
                  ymin = lower),
              alpha = .1) +
  xlab('Wind speed') +
  ylab('Detection probability') +
  theme_ggeffects(15)

b1

b2 = ggplot(plotEffectsData(fm1, 'det', 'cloud'), aes(covariateValue, Predicted)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(x= covariateValue,
                  y = Predicted,
                  ymax = upper,
                  ymin = lower),
              alpha = .1) +
  xlab('Cloud cover') +
  ylab('') +
  theme_ggeffects(15)

b2

b3 = ggplot(plotEffectsData(fm1, 'det', 'fraction'), aes(covariateValue, Predicted)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(x= covariateValue,
                  y = Predicted,
                  ymax = upper,
                  ymin = lower),
              alpha = .1) +
  xlab('Moon fraction') +
  ylab('') +
  theme_ggeffects(15)

b3


gratia::draw(fm1, 'det')


bb = ggarrange(b1,b2,b3, 
          nrow = 1)

bb

#save
ggsave('Plot_occu.pdf',
       bb,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS',
       width = 300,
       height = 100,
       units = 'mm',
       dpi = 800)
