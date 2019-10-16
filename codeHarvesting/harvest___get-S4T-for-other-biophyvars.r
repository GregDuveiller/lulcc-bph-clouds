### clouds...
# Harvest data to try to see dCFC in the space between delta_LE and delta_H from NCOMM


require(ncdf4)
require(dplyr)
require(raster)
require(ggplot2)



# input delta_LE and delta_H

dpath1 <- '/ESS_Datasets/USERS/Duveiller/Workspace/BiophysLUCeffectsfromRS/dataCuration'

nc_lvar <- 4 # to specify that we use the 4th dimension (iTr) to select what to put in the raster bricks
nc_level <- 2 # to specify the level of the variable nc_lvar to select, in this case the second transition

nc_LE <- nc_open(filename = paste0(dpath1, '/LE_IGBPgen.nc'))


rs_LE_delta <- brick(x = paste0(dpath1, '/LE_IGBPgen.nc'),
                     varname = 'Delta_LE', lvar = nc_lvar, level = nc_level)
rs_LE_sigma <- brick(x = paste0(dpath1, '/LE_IGBPgen.nc'),
                     varname = 'SD_Delta_LE', lvar = nc_lvar, level = nc_level)

df_LE_delta <- as.data.frame(rs_LE_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LE = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LE))


rs_HG_delta <- brick(x = paste0(dpath1, '/HG_IGBPgen.nc'),
                     varname = 'Delta_HG', lvar = nc_lvar, level = nc_level)
rs_HG_sigma <- brick(x = paste0(dpath1, '/HG_IGBPgen.nc'),
                     varname = 'SD_Delta_HG', lvar = nc_lvar, level = nc_level)

df_HG_delta <- as.data.frame(rs_HG_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_HG = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_HG))


rs_SW_delta <- brick(x = paste0(dpath1, '/SWreflected_IGBPgen.nc'),
                     varname = 'Delta_SWreflected', lvar = nc_lvar, level = nc_level)
rs_SW_sigma <- brick(x = paste0(dpath1, '/SWreflected_IGBPgen.nc'),
                     varname = 'SD_Delta_SWreflected', lvar = nc_lvar, level = nc_level)

df_SW_delta <- as.data.frame(rs_SW_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_SW = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_SW))


rs_albedo_delta <- brick(x = paste0(dpath1, '/albedo_IGBPgen.nc'),
                         varname = 'Delta_albedo', lvar = nc_lvar, level = nc_level)
rs_albedo_sigma <- brick(x = paste0(dpath1, '/albedo_IGBPgen.nc'),
                         varname = 'SD_Delta_albedo', lvar = nc_lvar, level = nc_level)

df_albedo_delta <- as.data.frame(rs_albedo_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_albedo = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_albedo))



rs_LSTday_delta <- brick(x = paste0(dpath1, '/LSTday_IGBPgen.nc'),
                         varname = 'Delta_LSTday', lvar = nc_lvar, level = nc_level)
rs_LSTday_sigma <- brick(x = paste0(dpath1, '/LSTday_IGBPgen.nc'),
                         varname = 'SD_Delta_LSTday', lvar = nc_lvar, level = nc_level)

df_LSTday_delta <- as.data.frame(rs_LSTday_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTday = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTday))


rs_LSTnight_delta <- brick(x = paste0(dpath1, '/LSTnight_IGBPgen.nc'),
                           varname = 'Delta_LSTnight', lvar = nc_lvar, level = nc_level)
rs_LSTnight_sigma <- brick(x = paste0(dpath1, '/LSTnight_IGBPgen.nc'),
                           varname = 'SD_Delta_LSTnight', lvar = nc_lvar, level = nc_level)

df_LSTnight_delta <- as.data.frame(rs_LSTnight_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTnight = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTnight))



# need to clean this up and load cfc dataframe from scratch
load('dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata') # df_dCFC_MOD05_FOR_1dd
df_CFC_delta <- df_dCFC_MOD05_FOR_1dd


df_all <- df_CFC_delta %>%
  inner_join(df_LE_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_HG_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_SW_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_LSTday_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_LSTnight_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_albedo_delta, by = c('lon', 'lat', 'month'))


save('df_all', file = 'dataFigures/df_multiDeltaDF.Rda')
