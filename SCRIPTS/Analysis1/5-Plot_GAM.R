#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'DHARMa','ggstats'), 
       require, 
       character.only=T)

#extract plotting info from model
px = gratia::draw(mx2)
length(px) #right terms in the model

#create empty list to keep all plots
pl = data.frame()

#PLOT DF
for (i in 1:(length(px)-2)) { #for each smooth (minus random effect and 3 interactions)
  
  colnames(px[[i]]$data)[8] <-'pred'
  
  #create a new df for plotting
  dx = data.frame(x = px[[i]]$data$pred, # x
                  fit = px[[i]]$data$est, # y
                  up.conf.int = px[[i]]$data$upper_ci, #calculate confidence intervals
                  low.conf.int = px[[i]]$data$lower_ci,
                  smooth = px[[i]]$data$smooth)
  
  pl = rbind(dx,pl)
  
}

setDT(pl)

pl$smooth = factor(pl$smooth, levels = c('s(temp2)',
                             's(wdsp)',
                             's(fraction)',
                             's(hour)',
                             's(days)'))

#plot
p = ggplot(pl, aes(x,fit)) + #exclude NS effects
  geom_line(lwd = 1) +
  geom_ribbon(aes(x = x, y = fit, ymin=low.conf.int, ymax=up.conf.int),
              lty = 2,
              alpha=0.1,
              color = NA,
              fill = 'black') +
  facet_wrap(~smooth, 
             scales = 'free',
             ncol = 5,
             labeller = as_labeller(c('s(temp2)'='Temperature (°C) ***',
                                      's(wdsp)'='Wind speed (km/h) ***',
                                      's(fraction)'='Lunar fraction *',
                                      's(hour)'='Hour ***',
                                      's(days)' = 'Days ***'))) +
  ylab('Log(Call duration)') +
  theme_ggeffects(15) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(.2,.8,.2,.2), "cm"))

p

#save
ggsave('Plot.pdf',
       p,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS',
       width = 350,
       height = 70,
       units = 'mm',
       dpi = 800)

#END

#fwrite(pl, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/Guest_lecture_Tralee/presentation/dfs/call_gam.csv')
