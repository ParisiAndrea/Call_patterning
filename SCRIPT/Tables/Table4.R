#load packages
sapply(c('data.table', 'dplyr', 'psycModel', 'kableExtra','tidyverse'), 
       require, 
       character.only=T)

##Table with summary from occupancy model
r = summary(fm1)

r2 = rbind(as.data.frame(r['state'], check.names = F, col.names = colnames(r['state']), fix.empty.names = F),
      as.data.frame(r['det'], check.names = F, col.names = colnames(r['det']), fix.empty.names = F),
      make.row.names = T)

r2$data = rownames(r2)
r2$data =  c('Intercept','Intercept','Wind speed','Cloud','Fraction','Cloud*Fraction')
rownames(r2) <- NULL
r2 = dplyr::select(r2, c('data','Estimate':'P(>|z|)'))

r2

r2k = r2 %>%
  kbl(row.names = F,
      col.names = c('','Esimate','Std.Err.','z','P(>|z|)'),
      align = 'r',
               digits = 3,
               escape = F) |>
  kable_classic(full_width = T, html_font = "Times New Roman", 'hover') |>
  pack_rows('Occupancy ψ (logit-scale)', 1,1) %>%
  pack_rows('Detection probability p (logit-scale)', 2,5) %>%
  gsub(' 0.000 ', '<0.001', .)
  

r2k %>% save_kable(file = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS/Table4.html', self_contained = T)


#backtransform intercept occupancy probability
mean(predict(fm1, 'state')$Predicted, na.rm = T)
mean(predict(fm1, 'state')$SE, na.rm = T)

#backtransform intercept detectability probability
mean(predict(fm1, 'det')$Predicted, na.rm = T)
mean(predict(fm1, 'det')$SE, na.rm = T)

#END
