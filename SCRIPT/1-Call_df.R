#load packages
sapply(c('data.table','dplyr','tidyr','lubridate','ggpubr'), 
       require, 
       character.only=T)

#read CSV
k = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/call_data.csv')
p = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/deploy_info.csv')
  
k = merge(k,p, by = 'folder')

#reclass variable
k$population = factor(k$population)
k$site = factor(k$site)  
k$folder = factor(k$folder)
k$file_name = factor(k$file_name)
k$date = as.Date(k$date)
k$hour = as.ITime(k$hour)
k$rival = as.factor(replace_na(k$rival, 0))
k$notes = as.character(k$notes)

#filter out some columns
k = k[,-c('call_start','call_end')] #no need for these

#new variable with
k$time_start = as.POSIXct(paste(k$date, k$time_start, sep = ' '), 'GMT')
k$time_end = as.POSIXct(paste(k$date, k$time_end, sep = ' '), 'GMT')

#extract hour and date info
k$hour_start = hour(k$time_start)
k$hour_end = hour(k$time_end)

#correct for over-midnight time and recalculate duration
#calculate duration
f = k %>%
  mutate(time_end = case_when(
    hour_start == 23 & hour_end == 0 ~ time_end + (24*60*60),
    TRUE ~ time_end),
    call_duration = as.numeric(time_end-time_start),
    id = 1:n()) %>%
  
  #define order and arrangement
  select(id,population,site,folder,file_name,date,time_start,time_end,call_duration,rival,notes) %>%
  arrange(folder,time_start)

head(f)

#END