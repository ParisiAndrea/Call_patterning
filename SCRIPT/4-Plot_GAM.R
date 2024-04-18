#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

#extract plotting info from model
px = plot.gam(mx, select = 1)

#create empty list to keep all plots
pl = list()

for (i in seq(1:4)) { #for each smooth
  
  #create a new df for plotting
  dx = data.frame(x = px[[i]]$x, # x
             fit = px[[i]]$fit, # y
             up.conf.int = px[[i]]$fit + (px[[i]]$se*1.96), #calculate confidence intervals
             low.conf.int = px[[i]]$fit - (px[[i]]$se*1.96),
             smooth = toupper(px[[i]]$xlab))

  #plot
  p = ggplot(dx, aes(x,fit)) +
    geom_line(lwd = 2) +
    geom_ribbon(aes(x = x, y = fit, ymin=low.conf.int, ymax=up.conf.int),
                lty = 2,
                alpha=0.1,
                color = NA,
                fill = 'darkgreen')+
    scale_x_continuous(name = dx$smooth) +
    ylab('Log(Call duration)') +
    theme_pubr(15)
  
  #store plots
  pl[[i]] = p
  
  rm(dx)
}

#arrange in grid
ggarrange(plotlist = pl,
          align = 'hv',
          nrow = 2,
          ncol = 2)

#save
ggsave('Picture.png',
       p,
       path = 'Your_path',
       width = 180,
       height = 120,
       units = 'mm',
       dpi = 600)

#END