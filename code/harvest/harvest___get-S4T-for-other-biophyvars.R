#!/usr/local/bin/Rscript
################################################################################
# Purpose: Harvest data of S4T for delta_LE, delta_H, delta_Rn, delta_albedo
# License: GPL v3
# Authors: Gregory Duveiller - Dec. 2020
################################################################################


require(ncdf4)
require(dplyr)
require(raster)
require(ggplot2)


# specify path to find the data
dpath1 <- 'data/input_data/S4T_ancillary'

nc_lvar <- 4 # to specify that we use the 4th dimension (iTr) to select what to put in the raster bricks
nc_level <- 2 # to specify the level of the variable nc_lvar to select, in this case the second transition

## LE ----
rs_LE_delta <- brick(x = paste0(dpath1, '/LE_IGBPgen_ext.nc'),
                     varname = 'Delta_LE', lvar = nc_lvar, level = nc_level)

df_LE_delta <- as.data.frame(rs_LE_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LE = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LE))


## HG ----
rs_HG_delta <- brick(x = paste0(dpath1, '/HG_IGBPgen_ext.nc'),
                     varname = 'Delta_HG', lvar = nc_lvar, level = nc_level)

df_HG_delta <- as.data.frame(rs_HG_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_HG = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_HG))

## Albedo ----
rs_albedo_delta <- brick(x = paste0(dpath1, '/albedo_IGBPgen_ext.nc'),
                         varname = 'Delta_albedo', lvar = nc_lvar, level = nc_level)

df_albedo_delta <- as.data.frame(rs_albedo_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_albedo = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_albedo))


## LSTday ----
rs_LSTday_delta <- brick(x = paste0(dpath1, '/LSTday_IGBPgen_ext.nc'),
                         varname = 'Delta_LSTday', lvar = nc_lvar, level = nc_level)

df_LSTday_delta <- as.data.frame(rs_LSTday_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTday = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTday))

## LST nighttime ----
rs_LSTnight_delta <- brick(x = paste0(dpath1, '/LSTnight_IGBPgen_ext.nc'),
                           varname = 'Delta_LSTnight', lvar = nc_lvar, level = nc_level)

df_LSTnight_delta <- as.data.frame(rs_LSTnight_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTnight = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTnight))


## SW reflected ----
rs_SW_delta <- brick(x = paste0(dpath1, '/SWreflected_IGBPgen_ext.nc'),
                     varname = 'Delta_SWreflected', lvar = nc_lvar, level = nc_level)

df_SW_delta <- as.data.frame(rs_SW_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_SW = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_SW))


## Calculate Rn from LW and SW ----
rs_LW_delta <- brick(x = paste0(dpath1, '/LWemitted_IGBPgen_ext.nc'),
                     varname = 'Delta_LWemitted', lvar = nc_lvar, level = nc_level)

rs_Rn_delta <- - (rs_SW_delta + rs_LW_delta)
rs_Rn_delta <- setZ(rs_Rn_delta, getZ(rs_SW_delta), name = 'mon (months)')

df_Rn_delta <- as.data.frame(rs_Rn_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_Rn = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_Rn))



## get ClimZones ----
cz5_map <- raster('data/inter_data/Results_from_RAM/koppen-Geiger_360x180_5zones.nc', varname = 'climzone')
cz5_df <- as.data.frame(cz5_map, xy = T, long = T) %>% 
  dplyr::mutate(lon = round(x, digits = 8), lat = round(y, digits = 8)) %>%
  dplyr::filter(!is.na(value), value < 5) %>%
  dplyr::mutate(region = dplyr::recode_factor(value, `1` = "Tropical", `2` = "Arid", `3` = "Temperate", `4` = "Boreal")) %>%
  dplyr::select(-x, -y, -layer, -value)


## Join to dCFC samples ----
# For all forest together
load(paste0(harvest_path, '/df_dCFC_MOD05_FOR_1dd.Rdata')) # <--- df_dCFC_MOD05_FOR_1dd
df_CFC_delta <- df_dCFC_MOD05_FOR_1dd %>%
  left_join(cz5_df, by = c("lon", "lat"))

## combine all and save ----
df_all <- df_CFC_delta %>%
  # the following are done with inner join because all are needed
  inner_join(df_LE_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_HG_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_Rn_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_albedo_delta, by = c('lon', 'lat', 'month'))

df_all <- df_all %>%
  # these instead are done with left join to avoid losing data due to NAs here
  left_join(df_SW_delta, by = c('lon', 'lat', 'month')) %>%
  left_join(df_LSTday_delta, by = c('lon', 'lat', 'month')) %>%
  left_join(df_LSTnight_delta, by = c('lon', 'lat', 'month'))


save('df_all', file = paste0(harvest_path, '/df_multiDeltaDF.Rdata'))
