require(sf)
require(ggplot2)
require(here)

# at Work LINUX: 
if(Sys.info()['sysname']=='Linux'){
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
}
# at Home OSX: 
if(Sys.info()['sysname']=='Darwin'){
vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'
}

world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
world_buffer <- st_buffer(world,-0.5) 

inPoints <- 'dataProcessing/SYNOP/GlobalStationPairs.shp'
pts <- sf::st_read(inPoints, quiet = TRUE)

pts.u <- as.data.frame(do.call('rbind', unique(pts$geometry))) %>% 
  sf::st_as_sf(coords = c(1,2), crs = 4326) %>%
  sf::st_intersection(world_buffer) %>%
  dplyr::mutate(POINT_ID=1:length(geometry))
  
ggplot(pts.u)+geom_sf(data=world,fill='grey20') + 
  geom_sf(aes(colour=POINT_ID)) + 
  coord_sf(xlim=c(20,40),ylim=c(50,70))


write_sf(pts.u, 'dataProcessing/SYNOP/cleaned_points.shp')








