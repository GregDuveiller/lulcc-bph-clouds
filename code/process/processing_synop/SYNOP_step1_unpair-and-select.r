#!/usr/local/bin/Rscript
################################################################################
# Purpose:  SYNOP_STEP1 unpair and select
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(sf)
require(ggplot2)
require(here)


world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)
world_buffer <- st_buffer(world,-0.5) 

inPoints <- 'data/inter_data/synop_analysis/GlobalStationPairs_rev.shp'
pts <- sf::st_read(inPoints, quiet = TRUE)

pts.u <- as.data.frame(do.call('rbind', unique(pts$geometry))) %>% 
  sf::st_as_sf(coords = c(1,2), crs = 4326) %>%
  sf::st_intersection(world_buffer) %>%
  dplyr::mutate(POINT_ID = 1:length(geometry))
  
ggplot(pts.u) + 
  geom_sf(data = world, fill = 'grey20') + 
  geom_sf(aes(colour = POINT_ID)) + 
  coord_sf(xlim = c(20,40), ylim = c(50,70))


write_sf(pts.u, 'data/inter_data/synop_analysis/cleaned_points.shp')








