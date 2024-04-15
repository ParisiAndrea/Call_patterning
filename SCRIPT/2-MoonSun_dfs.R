#load packages
sapply(c('data.table','dplyr','suncalc','lubridate'), 
       require, 
       character.only=T)

setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning')

setDT(a)[,id := .I]

#MOON DATA
m = getMoonIllumination(date = a$time_start,
                    keep = c("fraction", "phase","angle"))

setDT(m)[,id := .I]

m = m[, c('id','fraction','phase','angle')]

#combine call + moon + sun dfs
g = merge(a,m, by = 'id')

#extract Julian date
g[, days := as.numeric(format(g$time_start, "%j"))]

#END