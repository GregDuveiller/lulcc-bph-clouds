require(sf)
require(ncdf4)
require(raster)
require(dplyr)
require(ggplot2)
require(here)

topoDataPath <- '/ESS_EarthObs/DATA_PRODUCTS/Elevation/Topo-description-moving-windows'

r1 <- raster(paste0(topoDataPath,'/global_005dd/DEM_0.05dd_avg_focal_diff.tif'))
r2 <- raster(paste0(topoDataPath,'/global_005dd/DEM_0.05dd_sd_focal_avg.tif'))
r3 <- raster(paste0(topoDataPath,'/global_005dd/DEM_0.05dd_sd_focal_diff.tif'))
r4 <- raster(paste0(topoDataPath,'/global_005dd/DEM_global_0.05dd_mask.tif'))

# load shp from GEE with extracted polygons and tree + water cover pcts
pts_circ <- sf::st_read('dataProcessing/SYNOP/SYNOP_points_with_local_cover.shp', quiet = TRUE) %>%
  as.data.frame() %>%
  select('POINT_ID', 'X0_occurren', 'X1_treecove')

# make pts
pts <- sf::st_read('dataProcessing/SYNOP/cleaned_points.shp', quiet = TRUE) %>% 
  left_join(pts_circ, by='POINT_ID') %>% 
  dplyr::select(-scalerank, -featurecla) %>%
  dplyr::rename(local_water_cover=X0_occurren, 
                local_tree_cover=X1_treecove) %>%
  dplyr::mutate(topo_Mask = factor(raster::extract(r4, pts)),
                avg_focal_diff = raster::extract(r1, pts),
                sd_focal_avg = raster::extract(r2, pts),
                sd_focal_diff = raster::extract(r3, pts))



vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)

# ggplot(pts)+geom_sf(data=world,fill='grey20') + 
#   geom_sf(aes(fill=local_tree_cover),shape = 21) +
#   coord_sf(xlim=c(20,140),ylim=c(50,70)) +
#   scale_fill_viridis_c(option = 'viridis')
# 
# 
# ggplot(pts)+geom_sf(data=world,fill='grey20') + 
#   geom_sf(aes(fill=local_water_cover),shape = 21) +
#   coord_sf(xlim=c(20,140),ylim=c(50,70))+
#   scale_fill_viridis_c(option = 'magma')


write_sf(pts, 'dataProcessing/SYNOP/SYNOPpoints_withAncillaryData.shp')

