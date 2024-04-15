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

g = g %>%
  group_by(file_name) %>%
  mutate(across(temp:days, \(x) mean(x)),
         call_duration = sum(call_duration)) %>%
  filter(!duplicated(file_name)) %>%
  dplyr::select(population,site,date,hour,time_bin,temp:days,call_duration) %>%
  as.data.table()
  
#END
