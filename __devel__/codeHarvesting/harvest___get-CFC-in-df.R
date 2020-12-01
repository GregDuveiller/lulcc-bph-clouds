### Harvest into df...

library(ncdf4)
library(raster)
library(dplyr)

dpath <- '/users/greg/jrcbox/passeplat'
fname <- 'ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked.nc'

r_CFC <- brick(x = paste0(dpath, '/', fname))


# dummy raster to aggregate the output data in
rs_dummy <- raster(nrows = 180, ncols = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   crs = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'), vals = NULL)

# aggregate tp 1dd
rs_CFC_1dd <- raster::resample(x = r_CFC, y = rs_dummy, method = 'bilinear')

# filter and export df at lower resolution
df_CFC_1dd <- as.data.frame(rs_CFC_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, CFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(CFC))
save('df_CFC_1dd', file = 'dataFigures/df_CFC_MOD05_1dd.Rdata')


