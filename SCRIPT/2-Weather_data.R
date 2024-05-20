#load packages
sapply(c('data.table','dplyr','suncalc','lubridate','ncdf4','stringr'), 
       require, 
       character.only=T)

setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/WEATHER_nc')

#get all files in the weather directory
dd = dir(getwd(),
    pattern = '.nc')

#create an empty datset to fill
e = data.frame()

#run loop to extract weather information from each NC file
for (i in 1:length(dd)) { #for each file in the directory
  
#open  NC file
wb = nc_open(paste(getwd(),dd[i],
                    sep = '/'))

#to check single files
#nc_open(paste(getwd(),'magheroarty.nc',sep='/'))

#extract variables
x = data.frame(
  'time' = as.POSIXct(
    ncvar_get(wb, varid = 'time')*3600,  #time as seconds since 1900-01-01
    origin = "1900-01-01",
    tz = "GMT"), #time in usual format
  wind_n = ncvar_get(wb, varid = 'u10'), #wind X vector
  wind_e = ncvar_get(wb,varid = 'v10'), #wind Y vector
  #temp_skn = ncvar_get(wb,varid = 'skt')-273.15, #skin temperature (from K to C) 
  temp1 = ncvar_get(wb,varid = 'stl1')-273.15, #soil temperature
  #temp2 = ncvar_get(wb,varid = 't2m')-273.15, #temperature at 2m
  cloud = ncvar_get(wb,varid = 'tcc'), #cloud amount in %
  #cloud_h = ncvar_get(wb,varid = 'cbh'), #cloud height
  prec = ncvar_get(wb,varid = 'stl1'), #total precipitation
  lai = ncvar_get(wb,varid = 'lai_lv'), #leaf area index
  site = str_remove(dd[i],
                    pattern = '.nc'))

#calculate wind speed based on wind vectors
x$wdsp = sqrt((x$wind_n^2+x$wind_e^2))*3.6 

#store results in empty df
e = rbind(e,x) 

#remove unused x
rm(x)

}

#keep relevant variables only
e = dplyr::select(e, c(site,time,temp1:wdsp))

head(e) # N=

#add ID column
setDT(e)[, ID := .I]

#get moon variables
m = suncalc::getMoonIllumination(e$time)
colnames(m)[1] <-'time'

#add ID column
setDT(m)[, ID := .I]

head(m)
head(e)

#merge by time and ID
a = left_join(e,m,by=c('ID','time'))

#END