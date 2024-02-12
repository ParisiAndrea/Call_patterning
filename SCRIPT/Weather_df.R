#load packages
sapply(c('data.table','dplyr','lubridate',
         'stringr','tidyr','tibble'), 
       require, 
       character.only=T)

#OPEN AND MODIFY EACH WEATHER DATASET
ga = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_patterning/weather_data/galway.csv')
ga$station = 'galway'
ga = dplyr::select(ga, c(station,date,rain,temp,rhum,msl,wdsp,wddir))

be = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_patterning/weather_data/belmullet.csv')
be$station = 'belmullet'
be = dplyr::select(be, c(station,date,rain,temp,rhum,msl,wdsp,wddir))

ds = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_patterning/weather_data/donegal_s.csv')
ds$station = 'donegal_s'
ds = dplyr::select(ds, c(station,date,rain,temp,rhum,msl,wdsp,wddir))

dn = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_patterning/weather_data/donegal_n.csv')
dn$station = 'donegal_n'
dn = dplyr::select(dn, c(station,date,rain,temp,rhum,msl,wdsp,wddir))

###RBIND
wea = rbind(ga,be,ds,dn)

#CHANGE DATE FORMATS AND SPLIT IN TWO COLUMNS
wea$date = ymd_hm(wea$date)
wea = wea[date>'2022-01-01 00:00']

wea$hour = format(ymd_hms(wea$date),'%H:%M')
wea$date = format(ymd_hms(wea$date),'%Y-%m-%d')

wea = dplyr::select(wea, c(station,date,hour,temp,rain,rhum,msl,wdsp,wddir))
head(wea)

#WRITE IT DOWN
fwrite(wea, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_patterning/weather_data.csv')

#date:  -  Date and Time (utc)
#"rain:  -  Precipitation Amount (mm)	  "
#"temp:  -  Air Temperature (C)	"
#rhum:  -  Relative Humidity (%)
#msl:   -  Mean Sea Level Pressure (hPa)
#wdsp:  -  Mean Wind Speed (knot)
#wddir: -  Predominant Wind Direction (degree)
