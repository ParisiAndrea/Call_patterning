####

t = z %>%
  filter(hour %in% 22:23 | hour %in% 0:4) %>%
  group_by(folder,days,hour) %>%
  mutate(call_duration = sum(call_duration),
         across(temp2:fraction, \(x) mean(x))) %>% #create a new variable called hour
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
  mutate(call_duration = sum(call_duration),
         across(temp2:fraction, \(x) mean(x))) %>%
  filter(!duplicated(night_ID)) %>%
  as.data.table()
  
t$site = as.factor(t$site)

#END