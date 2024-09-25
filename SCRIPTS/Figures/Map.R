#load packages
sapply(c('data.table', 'dplyr', 'tidyr','ggplot2',
         'rgdal','rgeos','maptools','tmap','ggpubr',
         'terra','ggsn','broom','maps','grid'), 
       require, 
       character.only=T)

#SET WD
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/SHP')

#SET BASE LAYER
p = ggplot() + coord_fixed() +
  xlab("") + ylab("")

recs = readOGR('./recs.shp')

recs = spTransform(recs, CRS('EPSG: 29902')) %>% 
  as.data.table() %>%
  group_by(site) %>%
  mutate(n_site = as.factor(n())) %>%
  as.data.table()

recs = fortify(recs)

##GALWAY
gal_shp = readOGR('./galway.shp')

gal_shp = aggregate(gal_shp)

gal_shp = spTransform(gal_shp, CRS('EPSG: 29902'))

gal_shp = fortify(gal_shp)


gal <- p + 
  geom_polygon(data=gal_shp, aes(x=long, y=lat, group=group), 
               colour='black', fill="grey80") +
  geom_point(data=recs[population == 'gal'], 
             aes(x=coords.x1, y=coords.x2), fill = '#BD9D5E', colour="black", pch = 21, size = 4) +
  ggsn::scalebar(gal_shp, dist_unit = 'km', dist = 1, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_ps()

gal

###MULLET
mul_shp = readOGR('./mullet.shp')

mul_shp = aggregate(mul_shp)

mul_shp = spTransform(mul_shp, CRS('EPSG: 29902'))

mul_shp = fortify(mul_shp)

mul <- p + 
  geom_polygon(data=mul_shp, aes(x=long, y=lat, group=group), 
               colour='black', fill="grey80") +
  geom_point(data=recs[population == 'mul'], 
             aes(x=coords.x1, y=coords.x2, size = n_site), fill = '#BD9D5E', colour="black", pch = 21) +
  scale_size_discrete('Deployments',range = c(6,4)) +
  ggsn::scalebar(mul_shp, dist_unit = 'km', dist = 4, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_ps()

mul

##W_DONEGAL
wdo_shp = readOGR('./donegal_w.shp')

wdo_shp = aggregate(wdo_shp)

wdo_shp = spTransform(wdo_shp, CRS('EPSG: 29902'))

wdo_shp = fortify(wdo_shp)

wdo <- p + 
  geom_polygon(data=wdo_shp, aes(x=long, y=lat, group=group), 
               colour='black', fill="grey80") +
  geom_point(data=recs[population == 'wdo'], 
             aes(x=coords.x1, y=coords.x2), fill = '#BD9D5E', colour="black", pch = 21, size= 4) +
  ggsn::scalebar(wdo_shp, dist_unit = 'km', dist = 2, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_ps()

wdo

###e_donegal
edo_shp = readOGR('./donegal_e.shp')

edo_shp = aggregate(edo_shp)

edo_shp = spTransform(edo_shp, CRS('EPSG: 29902'))

edo_shp = fortify(edo_shp)

edo <- p + 
  geom_polygon(data=edo_shp, aes(x=long, y=lat, group=group), 
               colour='black', fill="grey80") +
  geom_point(data=recs[population == 'edo'], 
             aes(x=coords.x1, y=coords.x2, size = n_site), fill = '#BD9D5E', colour="black", pch = 21) +
  scale_size_discrete('Deployments',range = c(6,4)) +
  ggsn::scalebar(edo_shp, dist_unit = 'km', dist = 1, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomleft') +
  theme_ps()

edo

###IRELAND
ire_shp = readOGR('./ireland.shp')
ire_shp = spTransform(ire_shp, CRS('EPSG: 29902'))
ire_shp = fortify(ire_shp)

#NORTHERN IRELAND
nire_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/n_ireland.shp')
nire_shp = spTransform(nire_shp, CRS('EPSG: 29902'))
nire_shp = fortify(nire_shp)

ire <- p + geom_polygon(data=ire_shp, aes(x=long, y=lat, group=group), 
                        colour='black', fill="grey80") +
  geom_polygon(data=nire_shp, aes(x=long, y=lat, group=group), 
               colour='black', fill="grey80") +
  geom_polygon(data=gal_shp, aes(x=long, y=lat, group=group), 
               colour='#BD9D5E', fill="#BD9D5E") +
  geom_polygon(data=mul_shp, aes(x=long, y=lat, group=group), 
               colour='#BD9D5E', fill="#BD9D5E") +
  geom_polygon(data=edo_shp, aes(x=long, y=lat, group=group), 
               colour='#BD9D5E', fill="#BD9D5E") +
  geom_polygon(data=wdo_shp, aes(x=long, y=lat, group=group), 
               colour='#BD9D5E', fill="#BD9D5E") +
  annotate("text", x=.3e+5, y=2.57e+5, label= "A", fontface =2, size = 8) +
  annotate("text", x=.4e+5, y=3.4e+5, label= "B", fontface =2, size = 8) +
  annotate("text", x=1.9e+5, y=4.6e+5, label= "C", fontface =2, size = 8) +
  annotate("text", x=2.4e+5, y=4.8e+5, label= "D", fontface =2, size = 8) +
  theme_ps() +
  ggsn::scalebar(ire_shp, dist_unit = 'km', dist = 50, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  ggsn::north(data = nire_shp, symbol=3, scale = 0.25)

ire

###MERGE
mp = ggarrange(ggarrange(gal,
                         mul,
                         wdo,
                         edo,
                         legend = 'bottom',
                         common.legend = T,
                         ncol = 2, nrow = 2,
                         labels = c('A','B','C','D'),
                         font.label = list(size = 20),
                         align = 'v'),
               widths = c(.8,.3),
               ire,
               ncol = 2)

print(mp)

ggsave('Figure1.pdf',
       mp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/Call_patterning/GRAPHS/MS',
       width = 300,
       height = 180,
       units = 'mm',
       dpi = 600
)

#END