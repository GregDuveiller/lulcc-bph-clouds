# Prototype script to compare cloud effect data from different methods.
### ______________ ### ______________ ### ______________ ### ______________ ### 

library(dplyr)
library(ncdf4)
library(raster)
library(ggplot2)

# load Ram dataset
nc_RAM <- nc_open(filename = '/ESS_Datasets/USERS/Alkama/CLOUDS/Results/cloud_forest_sensitivity_4deg.nc')
rs_RAM <- stack(x = '/ESS_Datasets/USERS/Alkama/CLOUDS/Results/cloud_forest_sensitivity_4deg.nc', varname = 'Could_forest') 
df_RAM <- as.data.frame(x = rs_RAM, xy = T, long = T) %>%
  dplyr::rename(lon = x, lat = y, ccc = value) %>%
  dplyr::mutate(ccc = ccc/100,
                month = factor(format(as.Date(Z, format = '%Y-%m-%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-Z)


# quickplot to check
ggplot(df_RAM) + 
  geom_raster(aes(x = lon, y = lat, fill = ccc)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = c(-0.05,0.05),
                       oob = scales::squish) + 
  facet_wrap(~month) +
  coord_cartesian(expand = F)

# load S4T dataset
nc_S4T <- nc_open(filename = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc')
rs_S4T1 <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                varname = 'delta', lvar = 3, level = 1)
rs_S4T2 <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                varname = 'delta', lvar = 3, level = 2)
rs_S4T <- mosaic(x = rs_S4T1, y = rs_S4T2, fun = mean)

names(rs_S4T) <- names(rs_S4T1)
#rs_S4T_agr <- aggregate(x = rs_S4T, fact = 11)

rs_S4T_agr <- resample(x = rs_S4T, y = rs_RAM, method = "bilinear")

df_S4T <- as.data.frame(x = rs_S4T_agr, xy = T, long = T) %>%
  dplyr::rename(lon = x, lat = y, ccc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer)

# quickplot to check
ggplot(df_S4T) + 
  geom_raster(aes(x = lon, y = lat, fill = ccc)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = c(-0.05,0.05),
                       oob = scales::squish) + 
  facet_wrap(~month) +
  coord_cartesian(expand = F)



# combine dfs
df <- inner_join(x = df_S4T %>% rename(S4T = ccc), 
                 y = df_RAM %>% rename(RAM = ccc), 
                 by = c('lon','lat', 'month'))

ggplot(df, aes(x = S4T, y = RAM)) + 
  geom_point(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = lm, se = FALSE) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(-0.1, 0.1)) +
  geom_abline()

g1 <- ggplot(df %>% tidyr::gather(key = 'method', value = 'cfc', c('S4T','RAM'))) +
  geom_raster(aes(x = lon, y = lat, fill = cfc)) +
  facet_grid(month ~ method)+ 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = c(-0.05,0.05),
                       oob = scales::squish) + 
  coord_cartesian(expand = F)

ggsave('tempFigures/comparingMethods_RAMvsS4T_allvsavg.png', plot = g1, width = 6, height = 14)
