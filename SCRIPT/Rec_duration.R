library('warbleR')



dur = duration_sound_files(path = 'E:\13_S07',
                           skip.error = T)

## 
sum(dur$duration, na.rm = T)




read.table()



setwd('E:/')

filelist = list.files(path = getwd(),
                      pattern = ".*.txt",
                      recursive = T)

o = data.frame()

for (i in filelist) {
  
  y = read.delim(i, header=T, sep = ',')
  
  y$file = substr(i, 1,6)
  
  y$DATE = as.POSIXct(paste(y$DATE,y$TIME, sep = ' '), "%Y-%b-%d %H:%M:%S", tz = 'GMT')
  
  y$dur = as.numeric(last(y$DATE) - first(y$DATE))*24
  
  o = rbind(o, y)
  
}

o = o %>%
  mutate(dur = round(dur, 1)) %>%
  filter(!duplicated(file)) %>%
  dplyr::select(file,dur)


sum(o$dur) # # total hours = 2878h
max(o$dur)
min(o$dur)
mean(o$dur)
sd(o$dur)
