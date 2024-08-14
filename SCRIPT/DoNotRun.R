install.packages("mpathsenser")
library("mpathsenser")

#create time bins of 1min for each deployment

ed = data.frame()

for (i in unique(u$folder)) {

  u2 = u[folder == i]
  
  b = bin_data(u2, start, end, 'min')
  
  b$folder = i

  b$bin_data <- NULL
  
  ed = rbind(ed,b)
  
  rm(b)
  
  ed = filter(ed, hour(bin) %in% 22:23 | hour(bin) %in% 0:3)
  
}

int = c('1 minute', '5 minutes', '10 minutes','20 minutes', '30 minutes', '45 minutes', '1 hour', '6 hour')

gh = data.frame()

for (i in int) {

  e = ed %>%
    group_by(folder) %>%
    reframe(bini = floor_date(bin, i))
  
  c = f %>%
    filter(hour(time_start) %in% 22:23 | hour(time_start) %in% 0:3) %>%
    mutate(bini = floor_date(time_start, i)) %>%
    dplyr::select(site:file_name,call_duration, bini)

  ec = merge(e,c, by=c('folder','bini'), all.x = T) %>%
    reframe(det = i,
            prop = nrow(filter(., call_duration > 10))/nrow(.)*100)
  
  gh = rbind(gh,ec)
  
}

gh

gh$det = factor(gh$det, levels = int)

ggplot(gh, aes(det, prop)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(name = 'Visit length') +
  scale_y_continuous(name = 'Probability of detecting a corncrake') +
  theme_classic()


nrow(filter(t, call_duration < 10))





t = z %>%
  filter(hour %in% 22:23 | hour %in% 0:4) %>%
  group_by(folder,days,hour) %>%
  mutate(call_duration = sum(call_duration)) %>% #create a new variable called hour
  filter(!duplicated(hour)) %>%
  mutate(hour = factor(hour, levels = c(0,1,2,3,4,22,23))) %>%
  arrange(site,days,hour)

#create fake date 
t = t %>%
  group_by(folder) %>%
  mutate(fake_date = case_when(hour == 22 | hour == 23 ~ days+1,
                               TRUE ~ days))

t = t %>%
  group_by(folder,site,fake_date) %>%
  mutate(night_ID = cur_group_id()) %>%
  group_by(folder,site,night_ID) %>%
  mutate(call_duration = sum(call_duration)) %>%
  filter(!duplicated(night_ID)) %>%
  as.data.table()

