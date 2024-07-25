#load packages
sapply(c('data.table','dplyr','stringr',
         'ggplot2','raster','tidyr',
         'sf','terra','rgdal','Hmisc',
         'landscapemetrics','gdata','corrplot'), 
       require, 
       character.only=T)

####WORK IN ARCGIS PRO

#read shapefile contaniing habitat info
shp = vect('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/SHP/habitat.shp')
shp$area = expanse(shp) #calculate area

head(shp)

#EXTRACT HABITAT FEATURES
####dist
b = shp %>%
  as.data.table() %>%
  dplyr::select(batch:w_coor,sd_slotA,LEVEL_2_VA,area) %>%
  filter(LEVEL_2_VA == 'Wet Grassland' |
           LEVEL_2_VA == 'Dry Grassland' |
           LEVEL_2_VA == 'Sand Dunes') %>%
  group_by(batch, site) %>%
  reframe(grass = sum(area)/10^6,
            folder = paste(batch,sd_slotA,sep = '_')) %>%
  filter(!duplicated(folder))

b

#END