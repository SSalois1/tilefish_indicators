library(sf)
library(ggplot2)
library(ggnewscale)
library(marmap)
library(ecodata)
library(shadowtext)
library(ggnewscale)
library(wesanderson)
library(ggspatial)

#%>% st_transform(., "+proj=longlat +datum=NAD83 +no_defs")
## Bathy and ports
bathy <- marmap::getNOAA.bathy(-78,-64, 35, 45)
bathy = fortify.bathy(bathy)
blues = colorRampPalette(brewer.pal(9,'Blues'))(25)
depths <- c(0,50,100,200,300,500,Inf)
depths2 <- c('0','50','100','200','>300')
blues2 <- blues[c(5,7,9,11,13,24)]
load("C:/Users/sarah.salois/Documents/github/ssalois1/shiny_indicators/app/data/app_shapes.RData")
st_crs(canyons) <- 'epsg:4326'
interestingcanyons <- c("Norfolk Canyon", "Wilmington Canyon", 
                        "Hudson Canyon",
                        'Veatch Canyon',
                        'Hydrographers Canyon', 'Atlantis Canyon') # "Spencer Canyon", 

# loading in shape files for maps
US.areas <- st_read(here::here('shapefiles/USA.shp'), quiet = TRUE)
canada.areas <- st_read(here::here('shapefiles/Canada.shp'), quiet = TRUE)
bts_strata <- st_read(here::here('shapefiles/NES_BOTTOM_TRAWL_STRATA.shp'),
                      quiet = TRUE)
#canyons <- st_read(here::here('shapfiles/major_canyons.shp')) 
# plot(bts_strata) # to see all bottom trawl strata
us.coast = st_union(US.areas %>% st_as_sf())
st_crs(us.coast) <- 'epsg:4326'
ca.coast = st_union(canada.areas %>% st_as_sf())
st_crs(ca.coast) <- 'epsg:4326'

gtf_strata <- bts_strata %>% 
  filter(STRATUMA %in% c('01030', '01040', '01070', '01080', '01110', '01120', 
                         '01140', '01150', '01670', '01680', '01710', '01720', 
                         '01750', '01760')) # select just the gtf strata

gtf_strata_union = st_union(gtf_strata %>% st_as_sf())
st_crs(gtf_strata_union) <- 'epsg:4326'


# Recruitment index time series figure
ggplot(recruit, aes(x = factor(year), y = recruit_est, group = 1))+
  geom_rect(aes(xmin = '2007', xmax = '2022', ymin = -Inf, ymax = Inf), 
            fill = 'red', alpha = 0.02) +
  geom_rect(aes(xmin = '2000', xmax = '2022', ymin = -Inf, ymax = Inf), 
            fill = 'lightblue', alpha = 0.05) +
  geom_vline(xintercept = c('1973','1993','1999', '2005', '2013'), lty = 2) +
  geom_line(color = 'black', size = 1.5) +
  annotate("text", label = "*",
           x = 26, y = 14000, size = 8, colour = "red" )+
  xlab('Year') + 
  ylab('Recruitment estimate') + 
  theme(axis.text.x = element_text(color = 'black',
                                   size = 16, angle = 45, vjust = 1, hjust=1)) +
  ecodata::theme_facet() + 
  theme(legend.position = c(0.85, 0.2)) +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="grey20")) +
  theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm")) 
ggsave(path = here::here('figures'),'recruitment_index_ts.png', 
       width = 14, height = 8, units = "in", dpi = 300)

## Study fleet/observer time series figure
# note: sfob.env filtered for just small mesh
sfob.env %>% # aggregate by year
  group_by(year) %>% 
  summarise(ttl_sum = sum(sum_gt_catch)) %>% 
  ggplot(., aes(x = factor(year), y = ttl_sum, group = 1))+
  geom_rect(aes(xmin = '2007', xmax = '2022', ymin = -Inf, ymax = Inf), 
            fill = 'red', alpha = 0.02) +
  geom_rect(aes(xmin = '2000', xmax = '2022', ymin = -Inf, ymax = Inf), 
            fill = 'lightblue', alpha = 0.05) +
  geom_vline(xintercept = c('1973','1993','1999', '2005', '2013'), lty = 2) +
  geom_line(color = 'black', size = 1.5) +
  xlab('Year') + 
  ylab('Total tilefish catch') + 
  # facet_wrap(~month)+
  theme(axis.text.x = element_text(color = 'black',
                                   size = 15, angle = 45, vjust = 1, hjust=1)) +
  ecodata::theme_facet() +
theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm")) 
ggsave(path = here::here('figures'),'study_fleet_observer_ts.png', 
       width = 14, height = 8, units = "in", dpi = 300)


## Study fleet/observer catch map
gt_data_model_cpue %>% 
  filter(start_lat < 42.5 & depth_est > 50) %>%
  #group_by(year) %>% 
  mutate(bin = cut(year, seq(min(year), max(year) + 4, 4), right = FALSE)) %>%
  #mutate(mean_cpue = mean(cpue_hr),.groups = 'drop') %>% 
  ggplot() + 
  geom_sf(data = us.coast %>% st_as_sf() ,color = 'gray20', fill = 'lightgrey') +
  geom_contour(data = bathy,
               aes(x=x,y=y,z=-1*z),
               breaks=c(50,100,150,200, Inf),
               size=c(0.3),
               col = 'darkgrey') +
  stat_summary_2d(aes(x=start_lon, y=start_lat, z = sum_gt_catch),
                  fun=sum,binwidth=c(0.16666,0.16666)) + 
  scale_fill_viridis_c() + 
  geom_sf(data = canyons %>% st_as_sf() %>% 
            filter(Name %in% interestingcanyons), 
          fill = NA, colour = 'lightgrey', lwd = 1) +
  geom_sf_text(data = canyons %>% filter(Name %in% interestingcanyons[1:4]),
               aes(label = Name), nudge_x = 1.5,
               nudge_y = -0.2, fontface = 'bold')+
  geom_sf_text(data = canyons %>% filter(Name == "Atlantis Canyon"),
               aes(label = Name), nudge_x = 1,
               nudge_y = -0.45, fontface = 'bold') +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm")) +
    annotation_scale(location = 'tl') +
    coord_sf(xlim = c(-75,-65.5), ylim = c(36,44), datum = sf::st_crs(4326))  +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Total catch') +
  theme_bw() + 
  theme(legend.position = c(0.85, 0.2)) +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="grey20")) +
  theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) 
ggsave(path = here::here('figures'),'gtf_map_canyons_fish.png', 
       width = 7, height = 8, units = "in", dpi = 300)

