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

m2 = ggplot() +
  geom_contour_filled(data = bathy,
                      aes(x=x,y=y,z=-1*z),
                      breaks=c(0,50,100,250,500,Inf),
                      size=c(0.3)) + 
  scale_fill_manual(values = blues2, # 5:20
                    name = paste("Depth (m)"),
                    labels = depths2) +
  geom_contour(data = bathy,
               aes(x=x,y=y,z=-1*z),
               breaks=c(0,50,100,250,500,Inf),
               size=c(0.3), 
               col = 'lightgrey') +
  new_scale_fill() +
  geom_sf(data = us.coast %>% st_as_sf() ,color = 'gray20', fill = 'lightgrey') +
  geom_sf(data = ca.coast %>% st_as_sf() ,color = 'gray20', fill = 'lightgrey') +
  geom_sf(data = gtf_strata_union %>% st_as_sf(), lwd = 1,
          color = 'black', fill = NA, lty = 1) +
  geom_sf(data = canyons %>% st_as_sf() %>% 
            filter(Name %in% interestingcanyons), 
          fill = 'black', colour = 'grey20', lwd = 1) +
  geom_sf_text(data = canyons %>% filter(Name %in% interestingcanyons[1:4]),
               aes(label = Name), nudge_x = 1.4,
               nudge_y = -0.2, fontface = 'bold')+
  geom_sf_text(data = canyons %>% filter(Name == "Atlantis Canyon"),
               aes(label = Name), nudge_x = 1,
               nudge_y = -0.45, fontface = 'bold') +
  coord_sf(xlim = c(-76, -65), ylim = c(36,42), datum = sf::st_crs(4326))  +
  xlab('Longitude') + 
  ylab('Latitude') +
  annotation_scale(location = 'tl') +
  theme_bw()

m2 +  theme(legend.position = c(0.85, 0.2)) +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="grey20")) +
  theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) 

ggsave(path = here::here('figures'),'gtf_map_canyons_no_fish.png', 
       width = 7, height = 8, units = "in", dpi = 300)

