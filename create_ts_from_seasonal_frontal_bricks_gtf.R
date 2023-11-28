library(terra)
library(sf)
library(dplyr)
library(tidyverse)
library(exactextractr) # includes fraction within extract function! 
bts_strata <- st_read(here::here('shapefiles/NES_BOTTOM_TRAWL_STRATA.shp'),
                      quiet = TRUE)
# plot(bts_strata) # to see all bottom trawl strata
# sf object 
gtf_strata_sf <- bts_strata %>% 
  filter(STRATUMA %in% c('01030', '01040', '01070', '01080', '01110', '01120', 
                         '01140', '01150', '01670', '01680', '01710', '01720', 
                         '01750', '01760')) # select just the gtf strata

#gtf_strata_sv <- vect(gtf_strata_sf) # use this for terra::extract (needs spatvect)

########### THIS IS IT ################
path = here::here('/data/sst_fronts/raster_bricks/3x3')
# Pull in files
b.seasfprob <- list.files(path = here::here('data/sst_fronts/raster_bricks/3x3'),
                    pattern = glob2rx('sea_acspo_3x3_fprob_*all_years.tif'), 
                    full.names = TRUE)
b = rast(b.seasfprob)

gtf_fprob <- 
  exact_extract(
    b, 
    gtf_strata_sf, 
    progress = F  # this is for not displaying progress bar 
  )

s_combined <- bind_rows(gtf_fprob, .id = 'id') %>% 
  as_tibble()

#--- pivot to a longer format ---#
gtf_sst_long <- pivot_longer(
  s_combined, 
  -c(id, coverage_fraction), 
  names_to = 'year_season',
  values_to = 'fprob'
)  

# -- Calculate stats and save! -- #
### mean per each strata 
fprob_seasonal_ts_gtf <- gtf_sst_long %>% 
  mutate(yearnum = str_split_i(gtf_sst_long$year_season, '_', 8),
         season = str_split_i(gtf_sst_long$year_season, '_', 5), 
         year = case_when(
           yearnum == 1 ~ 2000, 
           yearnum == 2 ~ 2001, 
           yearnum == 3 ~ 2002, 
           yearnum == 4 ~ 2003, 
           yearnum == 5 ~ 2004, 
           yearnum == 6 ~ 2005, 
           yearnum == 7 ~ 2006, 
           yearnum == 8 ~ 2007, 
           yearnum == 9 ~ 2008,
           yearnum == 10 ~ 2009, 
           yearnum == 11 ~ 2010, 
           yearnum == 12 ~ 2011, 
           yearnum == 13 ~ 2012, 
           yearnum == 14 ~ 2013, 
           yearnum == 15 ~ 2014, 
           yearnum == 16 ~ 2015, 
           yearnum == 17 ~ 2016, 
           yearnum == 18 ~ 2017, 
           yearnum == 19 ~ 2018, 
           yearnum == 20 ~ 2019, 
           yearnum == 21 ~ 2020, 
           yearnum == 22 ~ 2021, 
           yearnum == 23 ~ 2022
         )) %>% 
  group_by(year,season) %>% 
  summarize(weighted_mean_fprob = sum(fprob * coverage_fraction) / sum(coverage_fraction), 
            mean_fprob = mean(fprob), 
            median_fprob = median(fprob), 
            sd_fprob = sd(fprob),
            max_fprob = max(fprob), 
            min_fprob = min(fprob)) %>% 
  mutate(year = as.numeric(year))

write.csv(fprob_seasonal_ts_gtf, 'fprob_seasonal_ts_gtf_3x3.csv') 


### mean per each strata 
fprob_seasonal_ts_gtf <- gtf_sst_long %>% 
  mutate(yearnum = str_split_i(gtf_sst_long$year_season, '_', 8),
         season = str_split_i(gtf_sst_long$year_season, '_', 5), 
         year = case_when(
           yearnum == 1 ~ 2000, 
           yearnum == 2 ~ 2001, 
           yearnum == 3 ~ 2002, 
           yearnum == 4 ~ 2003, 
           yearnum == 5 ~ 2004, 
           yearnum == 6 ~ 2005, 
           yearnum == 7 ~ 2006, 
           yearnum == 8 ~ 2007, 
           yearnum == 9 ~ 2008,
           yearnum == 10 ~ 2009, 
           yearnum == 11 ~ 2010, 
           yearnum == 12 ~ 2011, 
           yearnum == 13 ~ 2012, 
           yearnum == 14 ~ 2013, 
           yearnum == 15 ~ 2014, 
           yearnum == 16 ~ 2015, 
           yearnum == 17 ~ 2016, 
           yearnum == 18 ~ 2017, 
           yearnum == 19 ~ 2018, 
           yearnum == 20 ~ 2019, 
           yearnum == 21 ~ 2020, 
           yearnum == 22 ~ 2021, 
           yearnum == 23 ~ 2022
         )) %>% 
  group_by(id,year,season) %>% 
  summarize(weighted_mean_fprob = sum(fprob * coverage_fraction) / sum(coverage_fraction), 
            mean_fprob = mean(fprob), 
            median_fprob = median(fprob), 
            sd_fprob = sd(fprob),
            max_fprob = max(fprob), 
            min_fprob = min(fprob)) %>% 
  mutate(year = as.numeric(year))

write.csv(fprob_seasonal_ts_gtf, 'fprob_seasonal_ts_gtf_indv_substrata_3x3.csv') 


## do this for N vs S instead of individuals 


plot(gtf_strata_sf$geometry)
par(new=TRUE)
plot((gtf_strata_sf$geometry[1:6]), col = 'green')
plot(gtf_strata_sf$geometry)
par(new=TRUE)
plot(gtf_strata_sf$geometry[c(7:14)], col = 'blue')

result <- gtf_sst_long %>%
  mutate(yearnum = str_split_i(gtf_sst_long$year_season, '_', 8),
         season = str_split_i(gtf_sst_long$year_season, '_', 5), 
         year = case_when(
           yearnum == 1 ~ 2000, 
           yearnum == 2 ~ 2001, 
           yearnum == 3 ~ 2002, 
           yearnum == 4 ~ 2003, 
           yearnum == 5 ~ 2004, 
           yearnum == 6 ~ 2005, 
           yearnum == 7 ~ 2006, 
           yearnum == 8 ~ 2007, 
           yearnum == 9 ~ 2008,
           yearnum == 10 ~ 2009, 
           yearnum == 11 ~ 2010, 
           yearnum == 12 ~ 2011, 
           yearnum == 13 ~ 2012, 
           yearnum == 14 ~ 2013, 
           yearnum == 15 ~ 2014, 
           yearnum == 16 ~ 2015, 
           yearnum == 17 ~ 2016, 
           yearnum == 18 ~ 2017, 
           yearnum == 19 ~ 2018, 
           yearnum == 20 ~ 2019, 
           yearnum == 21 ~ 2020, 
           yearnum == 22 ~ 2021, 
           yearnum == 23 ~ 2022),
         substrat = case_when( 
           id %in% c(1:6) ~ 'n_strata',
           id %in% c(7:14) ~ 's_strata')
          ) %>% 
  group_by(substrat,year,season) %>% 
  summarize(weighted_mean_fprob = sum(fprob * coverage_fraction) / sum(coverage_fraction), 
            mean_fprob = mean(fprob), 
            median_fprob = median(fprob), 
            sd_fprob = sd(fprob),
            max_fprob = max(fprob), 
            min_fprob = min(fprob))

write.csv(result, 'fprob_seasonal_ts_gtf_n_v_s_substrata_3x3.csv') 



