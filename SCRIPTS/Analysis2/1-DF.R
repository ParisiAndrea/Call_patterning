#load packages
sapply(c('data.table','dplyr','suncalc','lubridate','fitdistrplus','MuMIn','boot','ggeffects'), 
       require, 
       character.only=T)

p = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/CSV/input/deploy_info.csv') %>%
  mutate(folder = paste(batch,sd_slotA,sep = '_'))

#new dataframe to create a time sequence between deployment start and end
u = p %>%
  group_by(folder,site) %>%
  summarise(start = as.POSIXct(paste(start_date, start_time, sep = ' ')),
            end = as.POSIXct(paste(end_date, end_time, sep = ' ')),
            lat = n_coor,
            lon = w_coor,
            ELC = as.factor(ELC)) %>%
  as.data.table()

head(u)

#new empty df
z = data.frame()

for (i in 1:nrow(u)) {
  
  tmp = data.frame()
  
  start = as.POSIXct(u$start)[i]
  end = as.POSIXct(u$end)[i]
  site = u$site[i]
  folder = u$folder[i]
  latitude = u$lat[i]
  elc = u$ELC[i]
  
  tmp = seq.POSIXt(start,end, by = "hour")
  
  z = rbind(z, cbind(merge(tmp,folder),site,latitude,elc))
  
}

#rename first two columns
colnames(z)[1] <- 'time_seq'
colnames(z)[2] <- 'folder'

#extract date and hour info from rounded POSIXct object
z$time = round_date(z$time_seq, unit = '1 hour')
z$days = as.numeric(format(z$time, "%j"))
z$hour = as.factor(hour(z$time))

#merge with weather data using the rounded POSIXct object
z = merge(z,a,by=c('site','time'),all.x = T)

#merge with call data
z = merge(z,f, by=c('folder','site','time'),all.x = T)

#NAs will be created and those represent the time when there are no calls
z$call_duration = replace_na(z$call_duration, 0)

z = dplyr::select(z, c(folder,site,time,days,hour,temp2:fraction,latitude,elc,call_duration))

#END