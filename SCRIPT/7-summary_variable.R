
colnames(g)

#summary table for response and covariates
g %>%
  gather(variable,value,temp:call_duration) %>%
  group_by(variable) %>%
  summarise(mean_var = round(mean(value),3),
            min_var = round(min(value),3),
            max_var = round(max(value),3),
            sd_var = round(sd(value),3)) %>%
  as.data.table()
            

  