#load packages
sapply(c('data.table','dplyr','suncalc','lubridate'), 
       require, 
       character.only=T)

#check working datasets
head(f)
head(a)

#combine call & weather dfs
g = merge(f,a, by = c('site','time'), all.x = T)

#extract Julian date
g[, days := as.numeric(format(g$time, "%j"))]

#sum call duration for each file
g = g %>%
  group_by(folder,time) %>%
  
  mutate(across(temp2:angle, \(x) mean(x)), #average environmental variables
         call_duration = sum(call_duration)) %>% #create a new variable
  filter(!duplicated(time)) %>%
  
  #keep only relevant columns
  dplyr::select(folder,file_name,site,time,call_duration,temp2:fraction,days) %>%
  as.data.table()

#check
head(g)

#check if there are any NAs
table(is.na(g))

#END