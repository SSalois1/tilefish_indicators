#### ------ Adding bottom salinity to catch data ------ ####
## Pull in catch data and rasters 
# Catch data: 
sfobs <-readRDS(here::here('data/catch_data/gold_tile_sf_ob_v1_temp_price.rds'))
# wrangle catch data
sfob.env <- sfobs %>% 
  mutate(mesh_bin = case_when(mesh_size <= 5.6 ~ 'SM', mesh_size >= 5.6 ~ 'LG',
                              TRUE ~ 'NA'),
         cpue_hr = SUM_GT_CATCH/effort_dur) %>% 
  filter(YEAR %in% c(1998:2022) & mesh_bin == 'SM') %>%
  dplyr::select(DATE, YEAR, MONTH, YDAY,trip_id, haul_id, hull_num, area, effort_dur, 
                SUM_GT_CATCH, cpue_hr, mesh_size, mesh_bin, depth, start_lat, start_lon,
                bottomT, bottomT_avg, MIN_TEMP_C, MEAN_TEMP_C, MAX_TEMP_C,
                TEMP_VARIANCE, TEMP_DEVIATION, MEAN_DPTH_M, tri, sed) %>% 
  mutate(YEAR = as.integer(YEAR)) %>% 
  rename_all(., .funs = tolower)

# Pull in salinity files
sal.files <- list.files(path = here::here('data/glorys/spat_rast'),
                        pattern = glob2rx('dd_bsal*.tif'), 
                        full.names = TRUE)

sal <- c(terra::rast(sal.files))
# Isolate unique years
date_vec <- sfob.env %>% group_by(haul_id) %>% 
  arrange(date) %>%
  .$date %>% unique()
# create new column to store salinity data in
sfob.env$btm_sal <- NA
# Run loop, extract at particular locations for each given date/haul_id
for(i in 1:length(date_vec)){
  tmp <- subset(sfob.env, date == date_vec[i])
  hauls = sort(unique(tmp$haul_id))
  for(j in 1:length(hauls)){
    tmp2 <- subset(tmp, haul_id == hauls[j])
    rtmp = terra::subset(sal, which(terra::time(sal) == date_vec[i]))
    b_sal<- terra::extract(rtmp, tmp2[, c('start_lon', 'start_lat')])
    locs <- which(sfob.env$date == date_vec[i] & sfob.env$haul_id == hauls[j])
    sfob.env$btm_sal[locs] <- b_sal[,2]
  }
}

write.csv(sfob.env, 'study_fleet_obs_env.csv', row.names = FALSE)

#### ------ Adding bottom temp and salinity to length data ------ ####

# Length data: 
lengths <-read.csv(here::here('data/catch_data/gt_data_length_andy.csv'))
lengths.env <- lengths %>% rename_all(., .funs = tolower) %>% 
  dplyr::select(year,month, tripid, haulnum, lathbeg, lonhbeg, area, sex,
                sampweight, agestrct, nagestrct, lenanml, disp) %>% 
  mutate(date = lubridate::ym(paste(year,month, sep = '-'))) # add column to index by
## Convert dms to dd 
dms2dd = function (lats, lons){
  a= as.numeric(substr(lats,1,2))
  b= as.numeric(substr(lats,3,4))
  c= as.numeric(substr(lats,5,6))
  d = as.numeric(substr(lons,1,2))
  e = as.numeric(substr(lons,3,4))
  f = as.numeric(substr(lons,5,6))
  b = b/60
  c = c/3600
  e = e/60
  f = f/3600
  lat = a +b +c
  lon = d+e+f
  lon = lon *(-1)
  return(data.frame(lat, lon))
}
dd = dms2dd(lengths.env$lathbeg, lengths.env$lonhbeg)

lengths.env$lat <- dd[,1]
lengths.env$lon <- dd[,2]

# Pull in bottom temp files
bt.files <- list.files(path = here::here('data/glorys/spat_rast'),
                        pattern = glob2rx('dd_bt*.tif'), 
                        full.names = TRUE)

# Length data has monthly time-step, taking monthly averages of bt
bt <- c(terra::rast(bt.files))
bt.ym <- terra::tapp(bt, "yearmonths", mean)
# clean up raster layer names to reflect the year/month to match length data
dates <- names(bt.ym)
clean.dates = str_extract(dates, '\\d+([.,]\\d+)?')
date.split <- t(sapply(clean.dates, function(x) substring(x, first=c(1,5), last=c(4,6))))
date.string <- paste(date.split[,1], date.split[,2], sep = '-')
# converting to date
dates2 <-(lubridate::ym(date.string))
names(bt.ym) <- dates2 # replacing layer names in raster


# Pull in salinity files
sal.files <- list.files(path = here::here('data/glorys/spat_rast'),
                        pattern = glob2rx('dd_bsal*.tif'), 
                        full.names = TRUE)

sal <- c(terra::rast(sal.files))
# Length data has monthly time-step, taking monthly averages of bt
sal.ym <- terra::tapp(sal, "yearmonths", mean)
# clean up raster layer names to reflect the year/month to match length data
dates <- names(sal.ym)
clean.dates = str_extract(dates, '\\d+([.,]\\d+)?')
date.split <- t(sapply(clean.dates, function(x) substring(x, first=c(1,5), last=c(4,6))))
date.string <- paste(date.split[,1], date.split[,2], sep = '-')
# converting to date
dates2 <-(lubridate::ym(date.string))
names(sal.ym) <- dates2 # replacing layer names in raster

## --- Set up to run extraction loop --- ##
# Isolate unique years
date_vec <- lengths.env %>% group_by(tripid) %>% 
  arrange(date) %>%
  .$date %>% unique()
# Create new column to store salinity data in
lengths.env$btm_temp <- NA
lengths.env$btm_sal <- NA
# Run loop, extract at particular locations for each given date/trip_id
for(i in 1:length(date_vec)){
  tmp <- subset(lengths.env, date == date_vec[i])
  trips = sort(unique(tmp$tripid))
  for(j in 1:length(trips)){
    tmp2 <- subset(tmp, tripid == trips[j])
    sal.tmp = terra::subset(sal, which(terra::time(sal) == date_vec[i]))
    b_sal<- terra::extract(sal.tmp, tmp2[, c('lon', 'lat')])
    locs <- which(lengths.env$date == date_vec[i] & lengths.env$tripid == trips[j])
    lengths.env$btm_sal[locs] <- b_sal[,2]
    bt.tmp = terra::subset(bt, which(terra::time(bt) == date_vec[i]))
    b_temp<- terra::extract(bt.tmp, tmp2[, c('lon', 'lat')])
    locs <- which(lengths.env$date == date_vec[i] & lengths.env$tripid == trips[j])
    lengths.env$btm_temp[locs] <- b_temp[,2]
  }
}

#setwd(here::here('data/catch_data'))
write.csv(lengths.env, 'lengths_env.csv', row.names = FALSE)

