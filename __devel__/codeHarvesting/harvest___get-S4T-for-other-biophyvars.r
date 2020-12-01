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


rs_Rn_delta <- brick(x = paste0(dpath1, '/Rn_IGBPgen.nc'),
                     varname = 'Delta_Rn', lvar = nc_lvar, level = nc_level)
rs_Rn_sigma <- brick(x = paste0(dpath1, '/Rn_IGBPgen.nc'),
                     varname = 'SD_Delta_Rn', lvar = nc_lvar, level = nc_level)

df_Rn_delta <- as.data.frame(rs_Rn_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_Rn = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_Rn))


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



# get ClimZones
cz5_map <- raster('/ESS_EarthObs/CLIMATE_DATA/koppen-Geiger/koppen-Geiger_360x180_5zones.nc', varname = 'climzone')
cz5_df <- as.data.frame(cz5_map, xy = T, long = T) %>% 
  dplyr::mutate(lon = round(x, digits = 8), lat = round(y, digits = 8)) %>%
  dplyr::filter(!is.na(value), value < 5) %>%
  dplyr::mutate(region = dplyr::recode_factor(value, `1` = "Tropical", `2` = "Arid", `3` = "Temperate", `4` = "Boreal")) %>%
  dplyr::select(-x, -y, -layer, -value)


# For all forest together
load('dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata') # df_dCFC_MOD05_FOR_1dd
df_CFC_delta <- df_dCFC_MOD05_FOR_1dd %>%
  left_join(cz5_df, by = c("lon", "lat"))

# # Separating between DFO and EFO
# # (but just on the CFC side, not on the other deltas... to be done?)
# load('dataFigures/df_dCFC_MOD05_DFO_1dd.Rdata') # df_dCFC_MOD05_DFO_1dd
# load('dataFigures/df_dCFC_MOD05_EFO_1dd.Rdata') # df_dCFC_MOD05_EFO_1dd
# 
# df_CFC_delta_bothPFT <- bind_rows(
#   df_dCFC_MOD05_DFO_1dd %>% mutate(PFT = 'Deciduous'),
#   df_dCFC_MOD05_EFO_1dd %>% mutate(PFT = 'Evergreen'))
# 
# df_CFC_delta <- df_CFC_delta_bothPFT %>% 
#   left_join(cz5_df, by = c("lon", "lat")) %>%
#   filter(!is.na(region))



### THE INNER JOINS MAY BE EXCLUDING TOO MUCH DATA!!!
# In the CRN region (corn belt) we see a step drop in the non-winter months...

# combine all
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


save('df_all', file = 'dataFigures/df_multiDeltaDF.Rda')
