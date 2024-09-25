#load packages
sapply(c('data.table', 'dplyr', 'psycModel', 'kableExtra','tidyverse'), 
       require, 
       character.only=T)

head(u)

smm = f %>%
  group_by(folder) %>%
  summarise(total_call = sum(call_duration)/3600)

sum(smm$total_call)

sm = data.frame(
  'Deployment' = u$folder,
  'Latitude' = round(u$lat,3),
  'Longitude' = round(u$lon,3),
  'Deployment start' = format(u$start,'%Y-%m-%d %H:%M'),
  'Deployment end' = format(u$end, '%Y-%m-%d %H:%M'),
  'Hours recorded' = paste0(trunc(as.numeric((u$end-u$start)*24)),"h",' ',round(as.numeric((u$end-u$start)*24)%%1 * 60,0),"'"),
  'Total call duration' = paste0(trunc(smm$total_call),"h", ' ',round(smm$total_call%%1*60,0),"'")
)

smk = sm %>%
  kbl(row.names = T,
      col.names = c(
        'Deployment',
        'Latitude',
        'Longitude',
        'Deployment start',
        'Deployment end',
        'Hours recorded',
        'Total call duration'),
      align = 'r',
      digits = 3,
      escape = F) |>
  kable_classic(full_width = F, html_font = "Times New Roman", 'hover')

smk

#smk %>% save_kable(file = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS/Table2.html', self_contained = T)

#END