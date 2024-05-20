#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'viridis','ggstats'), 
       require, 
       character.only=T)

#SCRIPT OT PLOT INTERACTION

#BY DEFAULT FOCUSING ON TEMPERATURE AND DAYS INTERACTION

#EXTRACT INFO FROM GRATIA DRAW FUNCTION
px = gratia::draw(mx2)[[7]] #check which one is the te(temp,days), 5 in this case 

#CREATE A DATAFRAME WITH TWO COVARIATE AND RESPONSE
px2 = data.frame(var1 = px$data$fraction,
                 var2 = px$data$cloud,
                 call_duration = px$data$est)

#EXCLUDE NAs
px2 = na.omit(px2)

#PLOT
ggplot(px2, aes(var1,var2, color = call_duration)) +
  geom_point() +
  scale_color_viridis(name = 'Log(Call duration)') +
  #scale_x_continuous(name = 'Temperature °C') + 
  #scale_y_continuous(name = 'Days since 01/01') + 
  theme_ggeffects(20) +
  theme(legend.position = 'bottom')

#END