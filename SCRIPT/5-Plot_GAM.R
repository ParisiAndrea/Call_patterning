#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

#extract plotting info from model
px = gratia::draw(mx)

#create empty list to keep all plots
pl = data.frame()

#PLOT DF
for (i in 1:(length(px)-4)) { #for each smooth (minus random effect and 3 interactions)
  
  colnames(px[[i]]$data)[8] <-'pred'
  
  #create a new df for plotting
  dx = data.frame(x = px[[i]]$data$pred, # x
                  fit = px[[i]]$data$est, # y
                  up.conf.int = px[[i]]$data$upper_ci, #calculate confidence intervals
                  low.conf.int = px[[i]]$data$lower_ci,
                  smooth = px[[i]]$data$smooth)
  
  pl = rbind(dx,pl)
  
}

#plot
p = ggplot(pl, aes(x,fit)) +
  geom_line(lwd = 2) +
  geom_ribbon(aes(x = x, y = fit, ymin=low.conf.int, ymax=up.conf.int),
              lty = 2,
              alpha=0.1,
              color = NA,
              fill = 'black') +
  facet_wrap(~smooth, 
             scales = 'free',
             labeller = as_labeller(c('s(days)'='Days',
                                      's(fraction)'='Moon fraction',
                                      's(hour)'='Hour',
                                      's(temp)'='Temperature (°C)',
                                      's(wdsp)'='Wind speed (km/h)'))) +
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

fwrite(pl, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/Guest_lecture_Tralee/presentation/dfs/call_gam.csv')
