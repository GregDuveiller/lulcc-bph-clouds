require(ncdf4)
require(raster)
require(dplyr)
require(ggplot2)
require(sf)
require(here)

vpath <- '/users/greg/Work/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_110m_land.shp'), quiet = TRUE)

dpath <- '/users/greg/jrcbox/passeplat'
s4tfname <- 'ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc'


r2 <- brick('dataFigures/rstr_dCFC_MOD05_FOR.nc')


r_chg <- brick(x = paste0(dpath, s4tfname), varname = 'delta', lvar = 4, level = 7)


plot(r_chg, nc = 1, col = brewer.pal(7,'RdBu'), 
     zlim = c(-0.12, 0.12), xlim = c(-20, 150), ylim = c(30, 70))

plot(world, add = T)

r_cfc <- raster(x = paste0(dpath, 'cfc_clim_global_aqua_m07.nc'))

r_cfc_reproj <- resample(x = r_cfc, y = r_chg, method="bilinear")

plot(r_cfc_reproj, nc = 1, col = brewer.pal(9,'Spectral'), 
     zlim = c(0, 1), xlim = c(-20, 150), ylim = c(30, 70))

r_chg_perc <- 100 * r_chg/r_cfc_reproj

plot(r_chg_perc, nc = 1, col = brewer.pal(7,'RdBu'), 
     zlim = c(-15, 15), xlim = c(-20, 150), ylim = c(30, 70))
