#load packages
sapply(c('data.table','dplyr','tidyr','lubridate','ggpubr'), 
       require, 
       character.only=T)

#read CSV
k = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/call_data.csv')
p = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/deploy_info.csv') %>%
  mutate(folder = paste(batch,sd_slotA,sep = '_')) %>% # same variable as in k for merging
  dplyr::select(c(folder,site)) #keep relevant columns only

#combine k with deployment info 
k = merge(k,p, by = 'folder')

#reclass variable
k$site = factor(k$site)  
k$folder = factor(k$folder)
k$file_name = factor(k$file_name)

#format time variables with posixct extension
k$time_start = as.POSIXct(paste(k$date_start, k$time_start, sep = ' '), 'GMT')
k$time_end = as.POSIXct(paste(k$date_end, k$time_end, sep = ' '), 'GMT')

#calculate call duration
k$call_duration = as.numeric(k$time_end-k$time_start)

#correct for over-midnight time and recalculate duration
#calculate call duration
f = k %>%
  
  #define order and arrangement
  dplyr::select(site,folder,file_name,time_start,time_end,call_duration) %>%
  
  arrange(folder,time_start)

head(f)

#CREATE rounded 1-HOUR TIME BIN 
f$time = round_date(f$time_start, unit="1 hour")
#f$int_b = round_date(f$time_end, unit="1 hour")

#f$time = round_date(f$time_start, unit="1 hour")
#f$hour = as.factor(hour(f$time))

#END