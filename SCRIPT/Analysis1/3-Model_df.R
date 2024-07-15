#load packages
sapply(c('data.table','dplyr','suncalc','lubridate'), 
       require, 
       character.only=T)

head(f)
head(a)

#combine call & weather dfs
g = merge(f,a, by = c('site','time'), all.x = T)

#extract Julian date
g[, days := as.numeric(format(g$time_start, "%j"))]

colnames(g)

#sum call duration for each file
g = g %>%
  group_by(folder,site,time) %>%
  mutate(across(temp1:angle, \(x) mean(x)),
         call_duration = sum(call_duration)) %>% #create a new variable called hour
  filter(!duplicated(time)) %>%
  #dplyr::select(population,site,date,hour,time_bin,temp:days,call_duration) %>%
  as.data.table()

head(g)

table(is.na(g))# no NAs

#END

#show correlation among weather variables
#corrplot::corrplot(cor(g[,c(10:23)]), bg = 'white',method = 'color',type = 'upper')