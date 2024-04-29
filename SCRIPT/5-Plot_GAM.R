#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

#extract plotting info from model
px = plot.gam(mx, select = 1)

#create empty list to keep all plots
pl = data.frame()

#PLOT DF
for (i in 1:(length(px)-3)) { #for each smooth (minus random effect and 2 interactions)
  
  #create a new df for plotting
  dx = data.frame(x = px[[i]]$x, # x
                  fit = px[[i]]$fit, # y
                  up.conf.int = px[[i]]$fit + (px[[i]]$se*1.96), #calculate confidence intervals
                  low.conf.int = px[[i]]$fit - (px[[i]]$se*1.96),
                  smooth = toupper(px[[i]][["xlab"]]))
  
  pl = rbind(dx,pl)
  
}

#plot
p = ggplot(pl, aes(x,fit)) +
  geom_line(lwd = 2) +
  geom_ribbon(aes(x = x, y = fit, ymin=low.conf.int, ymax=up.conf.int),
              lty = 2,
              alpha=0.1,
              color = NA,
              fill = 'darkgreen') +
  facet_wrap(~smooth, scales = 'free') +
  ylab('Log(Call duration)') +
  theme_classic(15) +
  theme(axis.title.x = element_blank())

p

#save
ggsave('Plot.png',
       p,
       path = 'Your_path',
       width = 180,
       height = 120,
       units = 'mm',
       dpi = 600)

#END


