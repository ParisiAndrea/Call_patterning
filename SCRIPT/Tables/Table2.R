#load packages
sapply(c('data.table', 'dplyr', 'psycModel', 'kableExtra','tidyverse'), 
       require, 
       character.only=T)

smm = g %>%
  group_by(folder) %>%
  summarise(total_call = sum(call_duration),
            n = n(),
            mean_call = mean(call_duration),
            sd_call = sd(call_duration),
            max_call = max(call_duration),
            min_call = min(call_duration),
            folder = cur_group_id())

smm

unique(g$folder)
