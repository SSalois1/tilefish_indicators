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
path = here::here('/data/sst/raster_bricks')
# Pull in files
b.sst <- list.files(path = here::here('data/sst/raster_bricks'),
                    pattern = glob2rx('mm*.tif'), 
                    full.names = TRUE)
# stack
s <- c(rast(b.sst[2:23])) # c will 'stack', rast opens list of rasters
## This will be tricky bc 2000 and 2023 are not fully updated, add those after
sst.00 <- rast(here::here('data/sst/raster_bricks/mm_sst_mean_2000.tif')) 
sst.23 <- rast(here::here('data/sst/raster_bricks/mm_sst_mean_2023.tif'))  
# extract data across gtf strata
# Note: 'exact_extract' function extracts both the value of the intersecting 
# raster cells, and the fraction of the intersecting area relative to 
# the full raster grid
gtf_sst <- 
  exact_extract(
    s, 
    gtf_strata_sf, 
    progress = F  # this is for not displaying progress bar 
  )

s_combined <- bind_rows(gtf_sst, .id = 'id') %>% 
  as_tibble()

#--- pivot to a longer format ---#
gtf_sst_long <- pivot_longer(
  s_combined, 
  -c(id, coverage_fraction), 
  names_to = 'year_month',
  values_to = 'sst'
)  

# -- Calculate stats and save! -- #
sst_ts_gtf <- gtf_sst_long %>% 
  mutate(year = str_split_i(gtf_sst_long$year_month, '_', 4),
         month = str_split_i(gtf_sst_long$year_month, '_', 5)) %>% 
  group_by(year,month) %>% 
  summarize(weighted_mean_sst = sum(sst * coverage_fraction) / sum(coverage_fraction), 
            mean_sst = mean(sst), 
            median_sst = median(sst), 
            sd_sst = sd(sst),
            max_sst = max(sst), 
            min_sst = min(sst)) %>% 
  mutate(year = as.numeric(year))

# write.csv(sst_ts_gtf, 'sst_ts_gtf.csv') can normally save here, not with sst
# Have to manually do 2000 and 2023 because of incomplete years: 

## Fixing and adding in the two years w/irregular number of months
sst.00 <- rast(here::here('data/sst/raster_bricks/mm_sst_mean_2000.tif')) 
sst.23 <- rast(here::here('data/sst/raster_bricks/mm_sst_mean_2023.tif'))  
names(sst.00) <- as.numeric(c(2:12))
names(sst.23) <- as.numeric(c(1:8))

# 2000
gtf_sst00 <- 
  exact_extract(
    sst.00, 
    gtf_strata_sf, 
    progress = F  
  )

s00_combined <- bind_rows(gtf_sst00, .id = 'id') %>% 
  as_tibble()

gtf_sst00_long <- pivot_longer(
  s00_combined, 
  -c(id, coverage_fraction), 
  names_to = 'month',
  values_to = 'sst'
)  
gtf_sst00_long$year <- 2000

sst_ts_gtf00 <- gtf_sst00_long %>% 
  na.omit() %>% 
  group_by(year,month) %>% 
  summarize(weighted_mean_sst = sum(sst * coverage_fraction) / sum(coverage_fraction), 
            mean_sst = mean(sst), 
            median_sst = median(sst), 
            sd_sst = sd(sst),
            max_sst = max(sst), 
            min_sst = min(sst))
# -- 2023 -- #
gtf_sst23 <- 
  exact_extract(
    sst.23, 
    gtf_strata_sf, 
    progress = F  
  )

s23_combined <- bind_rows(gtf_sst23, .id = 'id') %>% 
  as_tibble()

gtf_sst23_long <- pivot_longer(
  s23_combined, 
  -c(id, coverage_fraction), 
  names_to = 'month',
  values_to = 'sst'
)  
gtf_sst23_long$year <- 2023


sst_ts_gtf23 <- gtf_sst23_long %>% 
  group_by(year,month) %>% 
  summarize(weighted_mean_sst = sum(sst * coverage_fraction) / sum(coverage_fraction), 
            mean_sst = mean(sst), 
            median_sst = median(sst), 
            sd_sst = sd(sst),
            max_sst = max(sst), 
            min_sst = min(sst))

## --- Combining all and saving! --- ## 
full.ts <- rbind(sst_ts_gtf00, sst_ts_gtf, sst_ts_gtf23)
write.csv(full.ts, 'sst_ts_gtf.csv')

ggplot(full.ts, aes(x=year,y=weighted_mean_sst)) +
  geom_line() +
  facet_wrap(~month)+
  theme_bw()








