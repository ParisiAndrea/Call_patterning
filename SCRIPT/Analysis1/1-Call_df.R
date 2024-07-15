#load packages
sapply(c('data.table','dplyr','tidyr','lubridate','ggpubr'), 
       require, 
       character.only=T)

#read CSV
k = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/call_data.csv')
p = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/deploy_info.csv') %>%
  mutate(folder = paste(batch,sd_slotA,sep = '_')) %>% # same variable as in k for merging
  dplyr::select(c(folder,site,start_date:end_time)) #keep relevant columns only

#combine k with deployment info 
k = merge(k,p, by = 'folder')

#reclass variable
k$site = factor(k$site)  
k$folder = factor(k$folder)
k$file_name = factor(k$file_name)
k$date = as.Date(k$date)
k$rival = as.numeric(replace_na(k$rival, 0))
k$notes = as.character(k$notes)

#format time variables with posixct extension
k$time_start = as.POSIXct(paste(k$date, k$time_start, sep = ' '), 'GMT')
k$time_end = as.POSIXct(paste(k$date, k$time_end, sep = ' '), 'GMT')

#extract hour info to calculate call durations
k$hour_start = hour(k$time_start)
k$hour_end = hour(k$time_end)

#correct for over-midnight time and recalculate duration
#calculate call duration
f = k %>%
  mutate(time_end = case_when(
    hour_start == 23 & hour_end == 0 ~ time_end + (24*60*60),
    TRUE ~ time_end),
    call_duration = as.numeric(as.ITime(call_duration))) %>%
  
  #define order and arrangement
  dplyr::select(site,folder,file_name,date,time_start,time_end,call_duration,rival) %>%
  arrange(folder,time_start)

head(f)

#CREATE 1-HOUR TIME BIN 
f$time = strftime(floor_date(f$time_start, "hours"),format="%H:%M:%S", tz = 'UTC')
f$time = as.POSIXct(paste(as.Date(f$date),f$time, sep = ' '), 'UTC')
f$hour = as.factor(hour(f$time))

#END


unique(f$site)
