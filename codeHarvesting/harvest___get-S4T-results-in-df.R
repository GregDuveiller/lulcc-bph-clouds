#### Prepare dataset for use for figures ####

library(raster)
library(ncdf4)
library(dplyr)
library(tidyr)


dir.create('dataFigures', recursive = T)


### load S4T dataset
nc <- nc_open(filename = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc')
rs_DFO_delta <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'uncertainty', lvar = 3, level = 2)


### For both forest types combined (FOR)

# Combine both forest types in post-processing
rs_FOR_delta <- mosaic(x = rs_DFO_delta, y = rs_EFO_delta, fun = mean) 
# ==> could use weights based on actual proportions
names(rs_FOR_delta) <- names(rs_DFO_delta)

# Propagate uncertainty
rs_FOR_sigma <- mosaic(x = rs_DFO_sigma, y = rs_EFO_sigma, fun = mean) 
# ==> Should check if uncertainty is correctly propagated
names(rs_FOR_sigma) <- names(rs_DFO_sigma)


# # THIS BELOW is an option... to remove deltas that are in practice zero... 
# # but perhaps best set it to zero? or not doing it at all/
#
# datInSigma <- abs(rs_FOR_delta) - rs_FOR_sigma 
# hist(as.vector(datInSigma), breaks = c(-25,-10,-5,seq(-1,1,0.02),5), xlim = c(-1,1)) 
# 
# dat2exclude <- abs(rs_FOR_delta) < rs_FOR_sigma 
# rs_FOR_delta[dat2exclude] <- NA



qtls <- quantile(as.vector(rs_FOR_delta),probs = c(0.005,.995), na.rm = T)
rs_FOR_delta[rs_FOR_delta < qtls[1]] <- NA
rs_FOR_delta[rs_FOR_delta > qtls[2]] <- NA

# hist(as.vector(rs_FOR_delta), breaks = c(seq(-1,1,0.01)), xlim = c(-0.5,0.5))


df_FOR_delta <- as.data.frame(rs_FOR_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(delta_cfc))

save('df_FOR_delta', file = 'dataFigures/df_FOR_delta.Rdata')

# aggregate tto 1 degree for clearer view
rs_dummy <- raster(nrows = 180, ncols = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   crs = crs(rs_FOR_delta), vals = NULL)

rs_FOR_delta_1dd <- raster::resample(x = rs_FOR_delta, y = rs_dummy, method = 'bilinear')
df_FOR_delta_1dd <- as.data.frame(rs_FOR_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(delta_cfc))


save('df_FOR_delta_1dd', file = 'dataFigures/df_FOR_delta_1dd.Rdata')



### For deciduous forests (DFO)


qtls <- quantile(as.vector(rs_DFO_delta),probs = c(0.005,.995), na.rm = T)
rs_DFO_delta[rs_DFO_delta < qtls[1]] <- NA
rs_DFO_delta[rs_DFO_delta > qtls[2]] <- NA

# hist(as.vector(rs_DFO_delta), breaks = c(seq(-1,1,0.01)), xlim = c(-0.5,0.5))


df_DFO_delta <- as.data.frame(rs_DFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(delta_cfc))

save('df_DFO_delta', file = 'dataFigures/df_DFO_delta.Rdata')

# aggregate tto 1 degree for clearer view
rs_dummy <- raster(nrows = 180, ncols = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   crs = crs(rs_DFO_delta), vals = NULL)

rs_DFO_delta_1dd <- raster::resample(x = rs_DFO_delta, y = rs_dummy, method = 'bilinear')
df_DFO_delta_1dd <- as.data.frame(rs_DFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(delta_cfc))


save('df_DFO_delta_1dd', file = 'dataFigures/df_DFO_delta_1dd.Rdata')






### For evergreen forests (EFO)


qtls <- quantile(as.vector(rs_EFO_delta),probs = c(0.005,.995), na.rm = T)
rs_EFO_delta[rs_EFO_delta < qtls[1]] <- NA
rs_EFO_delta[rs_EFO_delta > qtls[2]] <- NA

# hist(as.vector(rs_EFO_delta), breaks = c(seq(-1,1,0.01)), xlim = c(-0.5,0.5))


df_EFO_delta <- as.data.frame(rs_EFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(delta_cfc))

save('df_EFO_delta', file = 'dataFigures/df_EFO_delta.Rdata')

# aggregate tto 1 degree for clearer view
rs_dummy <- raster(nrows = 180, ncols = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   crs = crs(rs_EFO_delta), vals = NULL)

rs_EFO_delta_1dd <- raster::resample(x = rs_EFO_delta, y = rs_dummy, method = 'bilinear')
df_EFO_delta_1dd <- as.data.frame(rs_EFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(delta_cfc))


save('df_EFO_delta_1dd', file = 'dataFigures/df_EFO_delta_1dd.Rdata')



