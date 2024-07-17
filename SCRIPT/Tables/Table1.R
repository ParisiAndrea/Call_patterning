#load packages
sapply(c('data.table', 'dplyr', 'psycModel', 'kableExtra','tidyverse'), 
       require, 
       character.only=T)

head(a)

wtab = data.frame(
  'Temperature (°C)' = c(
    mean(a$temp2),
    sd(a$temp2),
    max(a$temp2),
    min(a$temp2)),
  
  'Cloud cover (%)' = c(
    mean(a$cloud)*100,
    sd(a$cloud)*100,
    max(a$cloud)*100,
    min(a$cloud)*100),
  
  'Wind spped (Km/h)' = c(
    mean(a$wdsp),
    sd(a$wdsp),
    max(a$wdsp),
    min(a$wdsp)),
  
  'Fraction (0-1)' = c(
    mean(a$fraction),
    sd(a$fraction),
    max(a$fraction),
    min(a$fraction)),
  
  row.names =  c('Mean','Std.Dev.','Max.','Min.'))

kw = wtab %>%
  kbl(row.names = T,
      col.names = c('Temperature (°C)',
                    'Cloud cover (%)',
                    'Wind speed (Km/h)',
                    'Fraction (0-1)'),
      align = 'c',
      digits = 2,
      escape = F) %>%
  kable_classic(full_width = F, html_font = "Times New Roman", 'hover')

kw

kw %>% save_kable(file = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS/Table1.html', self_contained = T)
