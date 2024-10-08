library(unmarked)

{
  t$temp2 = scale(t$temp2)
  t$cloud = scale(t$cloud)
  t$wdsp = scale(t$wdsp)
  t$prec = scale(t$prec)
  t$latitude = scale(t$latitude)
  t$fraction = scale(t$fraction)
}

#detection
det = t %>%
  ungroup() %>%
  mutate(y = case_when(call_duration > 3600 ~ 1,
                       TRUE ~ 0)) %>%
  dplyr::select(folder,y) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, y) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  rename_with(~c(paste('y',seq(1:14),sep='.')))

det

#latitude (fixed covariate)
lat = t %>%
  ungroup() %>%
  dplyr::select(folder,latitude) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, latitude) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(1) %>%
  rename_with(~'lat')

#elc (fixed covariate)
elc = t %>%
  ungroup() %>%
  dplyr::select(folder,elc) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, elc) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(1) %>%
  rename_with(~'elc')

### temperature
temp = t %>%
  ungroup() %>%
  dplyr::select(folder,temp2) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, temp2) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  rename_with(~c(paste('temp',seq(1:14),sep='.')))

### wind speed
wdsp = t %>%
  ungroup() %>%
  dplyr::select(folder,wdsp) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, wdsp) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  rename_with(~c(paste('wdsp',seq(1:14),sep='.')))

### cloud
cloud = t %>%
  ungroup() %>%
  dplyr::select(folder,cloud) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, cloud) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  rename_with(~c(paste('cloud',seq(1:14),sep='.')))

### fraction
fraction = t %>%
  ungroup() %>%
  dplyr::select(folder,fraction) %>%
  group_by(folder) %>%
  mutate(ID = 1:n()) %>%
  spread(., folder, fraction) %>%
  dplyr::select(-ID) %>%
  t() %>%
  as.data.frame() %>%
  rename_with(~c(paste('fraction',seq(1:14),sep='.')))

oc = list(temp = temp,
          wdsp = wdsp,
          cloud = cloud,
          fraction = fraction)

#END