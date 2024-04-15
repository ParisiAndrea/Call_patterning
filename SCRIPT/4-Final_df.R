##FINAL
f$time_bin = strftime(floor_date(f$time_start, "1 hour"),format="%H:%M")

f = f %>%
  mutate(population = case_when(
    site == 'GAA Gortnamalin' ~ 'donegal_n',
    site == 'Binghamstown cemetery' ~ 'belmullet',
    site == 'Mushroom factory' ~ 'belmullet',
    site == "Boyle's Island" ~ 'donegal_s',
    site == 'Malin Head' ~ 'donegal_n',
    site == 'Termoncarragh' ~ 'belmullet',
    site == 'Omey Island (holiday house)' ~ 'galway'
  ))

w$time_bin = as.POSIXct(paste(w$date,w$hour, sep = ' '), 'UTC')

f$time_bin = as.POSIXct(paste(as.Date(f$time_start),f$time_bin, sep = ' '), 'UTC')

a = merge(f,w, by = c('population','date','time_bin'))

a$hour = strftime(floor_date(a$time_start, "1 hour"),format="%H")

#END