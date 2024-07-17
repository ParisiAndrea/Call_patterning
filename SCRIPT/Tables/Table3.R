#load packages
sapply(c('data.table', 'dplyr', 'psycModel', 'kableExtra','tidyverse'), 
       require, 
       character.only=T)


d = summary(mx2)
s = as.data.frame(d[['s.table']])

s$F <- as.numeric(d[['chi.sq']])

rownames(s) <- c(
  's(Temperature)',
  's(Wind speed)',
  's(Fraction)',
  's(Hour)',
  's(Days)',
  'ti(Fraction*Cloud)',
  's(site)')

colnames(s) <- c(
  'Edf',
  'Ref.Df',
  '(Signed G<sup>2</sup>)',
  'P'
)

s

skt = s |>
  kbl(row.names = T,
      col.names = c(
        'Edf',
        'Ref.Df',
        'X<sup>2</sup>',
        'P'),
      align = 'r',
      digits = 3,
      escape = F,
      linesep = 10) |>
  kable_classic(full_width = F, html_font = "Times New Roman", 'hover')

skt

skt = gsub(' 0.000 ', '<0.001', skt)

skt %>% save_kable(file = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS/Table3.html', self_contained = T)

           