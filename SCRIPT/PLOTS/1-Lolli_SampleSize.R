#load packages
sapply(c('data.table','dplyr','tidyr','lubridate','ggpubr'), 
       require, 
       character.only=T)

g1 = ggplot(b) +
  geom_segment( aes(x=time_bin ,xend=time_bin, y=0, yend=call_duration), 
                color="#A7A7A0", lwd = 1) +  
  geom_point(aes(time_bin,call_duration), 
             size = 6,
             shape = 21,
             fill = 'white',
             color = '#A7A7A0') +
  scale_y_continuous(name = 'Cumulative call seconds',
                     breaks = seq(0,30000,5000)) +
  scale_x_discrete(name = 'Time') +
  theme_pubclean(15) +
  theme(axis.text.x = element_text(angle = 90))

print(g1)

ggsave('LOLLIPOP.jpg',
       g1,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS',
       width = 180,
       height = 120,
       units = 'mm',
       dpi = 600)

#END