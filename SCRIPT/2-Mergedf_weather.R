#load packages
sapply(c('data.table','dplyr','tidyr','lubridate','ggpubr'), 
       require, 
       character.only=T)

#read CSV
k = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/call_data.csv')

#reclass variable
k$folder = factor(k$folder)
k$file_name = factor(k$file_name)
k$date = as.Date(k$date)
k$hour = as.ITime(k$hour)
k$rival = replace_na(k$rival, 0)
k$notes = as.character(k$notes)

#filter out some columns
k = k[,-c('call_start','call_end')] #no need for these

#new variable with
k$time_start = as.POSIXct(paste(k$date, k$time_start, sep = ' '), 'GMT')
k$time_end = as.POSIXct(paste(k$date, k$time_end, sep = ' '), 'GMT')

#extract hour info
k$hour_start = hour(k$time_start)
k$hour_end = hour(k$time_end)

#correct for over-midnight time and recalculate duration
#calculate duration
f = k %>%
  mutate(time_end = case_when(
    hour_start == 23 & hour_end == 0 ~ time_end + (24*60*60),
    TRUE ~ time_end),
    call_duration = as.numeric(time_end-time_start)) %>%
  
  #define order and arrangement
  select(folder,file_name,time_start,time_end,call_duration,rival,notes) %>%
  arrange(folder,time_start)

head(f)

b = f %>%
  group_by(time_bin = strftime(floor_date(f$time_start, "1 hour"),format="%H:%M")) %>%
  mutate(call_duration = sum(call_duration)) %>%
  as.data.table()

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

