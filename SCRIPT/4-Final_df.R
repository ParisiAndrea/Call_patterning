##FINAL

b = f %>%
  group_by(time_bin = strftime(floor_date(f$time_start, "1 hour"),format="%H:%M")) %>%
  mutate(call_duration = sum(call_duration)) %>%
  filter(!duplicated(time_bin)) %>%
  as.data.table()

w$time_bin = as.POSIXct(paste(w$date,w$hour, sep = ' '), 'UTC')

b$time_bin = as.POSIXct(paste(as.Date(b$time_start),b$time_bin, sep = ' '), 'UTC')

a = merge(b,w, by = 'time_bin', all.x = T, all.y = F)


#CHECK ONCE POPULATION INFO IS AVAILABLE
