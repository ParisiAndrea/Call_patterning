#load packages
sapply(c('data.table','dplyr','mgcv','tidyverse','sjPlot','performance','ggplot2',
         'gratia','ggpmisc','ggpubr','ggeffects', 'viridis','ggstats'), 
       require, 
       character.only=T)

#SCRIPT OT PLOT INTERACTION

#BY DEFAULT FOCUSING ON TEMPERATURE AND DAYS INTERACTION

#EXTRACT INFO FROM GRATIA DRAW FUNCTION
px = gratia::draw(mx2)[[6]] #check which one is the te(temp,days), 5 in this case 

#CREATE A DATAFRAME WITH TWO COVARIATE AND RESPONSE
px2 = data.frame(var1 = px$data$fraction,
                 var2 = px$data$cloud,
                 call_duration = px$data$est)

#EXCLUDE NAs
px2 = na.omit(px2)

#PLOT
p2 = ggplot(px2, aes(var1,var2, color = call_duration)) +
  geom_point() +
  scale_color_viridis(name = 'Log(Call duration)',
                      breaks = seq(-.4,.8,.4)) +
  scale_x_continuous(name = 'Lunar fraction') + 
  scale_y_continuous(name = 'Cloud cover (%)') + 
  theme_ggeffects(15) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 10))

p3 = ggarrange(p,
               p2,
               ncol = 1,
               labels = c('','*'),
               font.label = list(size = 13, color = "grey20"))

p3 

#save
ggsave('Figure3.png',
       p3,
       path = './GRAPHS',
       width = 280,
       height = 140,
       units = 'mm',
       dpi = 800)
#END
