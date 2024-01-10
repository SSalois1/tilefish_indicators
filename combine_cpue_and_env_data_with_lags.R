# Wrangle each data source
# Study fleet observer
sfob.env <- read.csv('study_fleet_obs_env.csv')
df = sfob.env %>% filter(year > 2006 & depth > 50 & cpue_hr > 0 & cpue_hr < 15) %>%
  arrange(year,month) %>% 
  group_by(year,month) %>% 
  summarise(mean_dpth = mean(depth),
            mean_bt = mean(bottomt),
            min_bt = min(bottomt),
            max_bt = max(bottomt),
            sd_bt = sd(bottomt),
            mean_bs = mean(btm_sal), 
            min_bs = min(btm_sal),
            max_bs = max(btm_sal),
            sd_bs = sd(btm_sal),
            mean_tri = mean(tri),
            mean_sed = mean(sed),
            mean_cpue = mean(cpue_hr)) 
# Need to do annual lags because not all months are present each year
df2 <- df %>% 
group_by(year) %>%
  summarise(ann_bt = mean(mean_bt),
            ann_bs = mean(mean_bs)) %>%
  mutate(bt_lag3 = lag(ann_bt,3),
         bt_lag4 = lag(ann_bt,4),
         bs_lag3 = lag(ann_bs,3),
         bs_lag4 = lag(ann_bs,4))
df_full <- dplyr::full_join(df, df2, by = join_by(year))

## sst 
sst <- sst %>% 
  arrange(year,month)

# SST by month with lag
sst.lag <- sst %>%
  arrange(year,month) %>% 
  mutate(mean_sst_lag2 = lag(weighted_mean_sst,24),
         mean_sst_lag3 = lag(weighted_mean_sst,36),
         mean_sst_lag4 = lag(weighted_mean_sst,48))

# Shelf water volume
shlfvol <- read.csv(here::here('data/shelf_water_volume/ShelfWaterVolume_BSB_update.csv'))

# wrangling date info, converting doy to date and month 
yrs <- as.vector(nrow(shlfvol))
shlfvol$Year <- as.character(shlfvol$Year)
for (i in 1:nrow(shlfvol)){
  yrs[i] <- strsplit(shlfvol$Year, ".", fixed = TRUE)[[i]][1]
}
shlfvol$year <- yrs
shlfvol <- shlfvol %>% mutate(date_= as.Date(Year.Day-1, 
                                             origin=paste0(year, "-01-01")), 
                              month= strftime(date_, "%m"), 
                              day=strftime(date_,"%d"), 
                              year = as.integer(year),
                              month = as.numeric(month))  %>% 
  arrange(year,month)

shlfvol2 = shlfvol %>%
  filter(year > 1997) %>%
  group_by(year) %>%
  summarise(mean_shw_v = mean(ShW.Vol),
            mean_shw_s = mean(ShW.S),
            mean_shw_t = mean(ShW.T)) %>%
  mutate(shw_v_lag3 = lag(mean_shw_v,3),
         shw_v_lag4 = lag(mean_shw_v,4),
         shw_s_lag3 = lag(mean_shw_s,3),
         shw_s_lag4 = lag(mean_shw_s,4), 
         shw_t_lag3 = lag(mean_shw_t,3),
         shw_t_lag4 = lag(mean_shw_t,4))

# gulf stream index 
gsi.m <- read.csv(here::here('data/gulf_stream_index/mm_gsi_1954_2022_chen.csv'))
gsi <- gsi.m %>%  
  arrange(year,month) %>% 
  mutate(gsi_lag3 = lag(GSI,36),
         gsi_lag4 = lag(GSI,48)) %>% 
  dplyr::select(year,month, GSI, gsi_lag3, gsi_lag4) %>% 
  rename_all(., .funs = tolower)

## for fun adding in categorical data
# mutate(pos = ifelse(m.gsi > 0, 'Northerly', 'Southerly'), 
#          n.pos = ifelse(m.gsi > 0, 1, 0))


# microplankton
micro <-read.csv(here::here('data/phyto_size_class/microplankton_ts_gtf_strata.csv'))
micro2 <- micro %>%  
  arrange(year,month) %>% 
  mutate(micro_lag3 = lag(weighted_mean_micro,36),
         micro_lag4 = lag(weighted_mean_micro,48)) %>% 
  dplyr::select(year,month, weighted_mean_micro, micro_lag3, micro_lag4)

# cold pool 
cp <- ecodata::cold_pool
# Selecting just the extent
cp.ex <- cp %>% filter(Time %in% c(1994:2020), 
                       Var == 'extent_index') %>%
  mutate(cp_ext = Value,
         cp_ext_lag3 = lag(cp_ext,3), 
         cp_ext_lag4 = lag(cp_ext,4), 
         year = as.numeric(Time)) %>% 
  dplyr::select(year, cp_ext, cp_ext_lag3, cp_ext_lag4)

cp.pers <- cp %>% filter(Time %in% c(1994:2020), 
                         Var == 'persistence_index') %>%
  mutate(cp_pers = Value,
         cp_pers_lag3 = lag(cp_pers,3), 
         cp_pers_lag4 = lag(cp_pers,4),
         year = as.numeric(Time)) %>% 
  dplyr::select(year, cp_pers, cp_pers_lag3, cp_pers_lag4)







# Join all environmental data with sf/ob data
sst.sfob <- dplyr::full_join(sst.lag, df_full, by = join_by(year, month)) %>% 
  dplyr::select(everything())
swv.sfob <-  dplyr::full_join(shlfvol2, sst.sfob, by = join_by(year)) %>% 
  dplyr::select(everything())
gsi.sfob <- dplyr::full_join(swv.sfob, gsi, by = join_by(year, month)) %>% 
  dplyr::select(everything())
micro.sfob <- dplyr::full_join(gsi.sfob, micro2, by = join_by(year, month)) %>% 
  dplyr::select(everything())
cp.ex.sfob <- dplyr::full_join(micro.sfob, cp.ex, by = join_by(year)) %>% 
  dplyr::select(everything())
cp.pers.sfob <- dplyr::full_join(cp.ex.sfob, cp.pers, by = join_by(year)) %>% 
  dplyr::select(everything())

write.csv(cp.pers.sfob, 'gtf_sfob_cpue_env_with_lags.csv')


