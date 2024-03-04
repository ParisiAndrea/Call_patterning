#load packages
sapply(c('data.table','dplyr','suncalc','lubridate'), 
       require, 
       character.only=T)

setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning')

#MOON DATA
m = getMoonIllumination(date = b$time_start,
                    keep = c("fraction", "phase","angle"))

setDT(m)[,id := .I]

m = m[, c('id','fraction','phase','angle')]

##GET SUNLIGHT TIMES
s = getSunlightTimes(date = as.Date(b$time_start),
                     lon = -9.1876,
                     lat = 54.5780,
                     tz = "UTC",
                     keep = c('dawn','dusk')) #middle of NW coast

#calculate night duration
setDT(s)[, night_dur:= round(24-as.numeric(dusk-dawn),2)]

#id col
s[,id := .I]

s = s[, c('id','night_dur')]

#combine call + moon + sun dfs
g = merge(b,merge(m,s,by = 'id'), by = 'id')

#extract Julian date
g[, days := as.numeric(format(g$time_start, "%j"))]

#END