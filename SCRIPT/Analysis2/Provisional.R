#load packages
sapply(c('data.table','dplyr','suncalc','lubridate','fitdistrplus'), 
       require, 
       character.only=T)

#new dataframe to create a time sequence between deployment start and end
d = p %>%
  group_by(folder,site) %>%
  summarise(start = as.POSIXct(paste(start_date, start_time, sep = ' ')),
            end = as.POSIXct(paste(end_date, end_time, sep = ' '))) %>%
  as.data.table()

#new empty df
z = data.frame()

for (i in 1:nrow(d)) {
  
  tmp = data.frame()
  
  start = as.POSIXct(d$start)[i]
  end = as.POSIXct(d$end)[i]
  site = d$site[i]
  folder = d$folder[i]
  
  tmp = seq.POSIXt(start,end, by = "hour")
  
  z = rbind(z, cbind(merge(tmp,folder),site))
  
}

colnames(z)[1] <- 'time_seq'
colnames(z)[2] <- 'folder'

z$date = date(z$time_seq)
z$hour = as.factor(hour(z$time_seq))

z = merge(z,a,by=c('site','date','hour'),all.x = T)

z = merge(z, f, by=c('folder','site','date','hour','time'),all.x = T)

z = dplyr::select(z, -c(time_seq, file_name:time_end))

z$call_duration = replace_na(z$call_duration, 0)

####

t = z %>%
  filter(hour %in% 22:23 | hour %in% 0:4) %>%
  group_by(folder,date,hour) %>%
  mutate(call_duration = sum(call_duration)) %>% #create a new variable called hour
  filter(!duplicated(hour)) %>%
  mutate(hour = factor(hour, levels = c(0,1,2,3,4,22,23))) %>%
  arrange(site,date,hour)

#create fake date 
t$fake_date = if_else(t$hour == 22 | t$hour == 23, t$date+1, t$date)
    
t = t %>%
  ungroup() %>%
  group_by(folder,site,fake_date) %>%
  mutate(ID = cur_group_id()) %>%
  group_by(folder,site,ID) %>%
  mutate(call_duration = sum(call_duration),
         across(temp1:angle, \(x) mean(x)))

t$site = as.factor(t$site)
t$days = as.numeric(format(t$fake_date, "%j"))
hist(t$call_duration)
t = na.omit(t)

mh = gam(call_duration ~
           s(temp1, bs ='tp') +
           s(wdsp, bs = 'tp') +
           s(cloud, bs = 'tp') +
           s(days, bs = 'tp') +
           s(fraction, bs ='tp') +
           s(site, bs = 're'),
         data = t,
         knots=list(hour=c(0,23)),
         na.action = "na.fail",
         method ='REML',
         family = gaussian())

summary(mh)
res = simulateResiduals(mh, plot = T, re.form = NULL)
gratia::draw(mh)
gratia::appraise(mh)

plot(residuals(mh))
shapiro.test(residuals(mh))
