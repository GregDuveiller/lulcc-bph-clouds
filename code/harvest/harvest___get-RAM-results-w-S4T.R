#!/usr/local/bin/Rscript
################################################################################
# Purpose: Harvest data from the RAM (time-for-time) method 
# License: GPL v3
# Authors: Gregory Duveiller - Dec. 2020
################################################################################


require(raster)
require(ncdf4)
require(dplyr)
require(tidyr)


# get RAM's data ----


# per ClimZone 5 region ... 
data.tag <- '_absolute_'
###data.tag <- '_binedby4percentChange_'
###data.tag <- '_binedby2percentChange_'

data.path <- paste0(results_path, '/', 'time4time_analysis')


CZ5_regions <- c('Tropical', 'Arid', 'Temperate', 'Boreal')
PFTs <- c('TOT','EFO','DFO')
df_CZ5_RAM <- data.frame()
for(region in CZ5_regions){
  for(pft in PFTs){
    dum <- read.csv(file = paste0(data.path, '/', region, data.tag, pft, '.csv')) %>%
      mutate(month = factor(month.abb, levels = month.abb),
             region = factor(region, levels = CZ5_regions),
             PFT = factor(pft, levels = PFTs),
             dCFC_CZ5 = Slope / 100,
             dCFC_CZ5_STD_err = STD_err / 100) %>%
      dplyr::select(-Slope, -STD_err)
    #dplyr::select(-Slope, -STD_err, -Pvalue)
    df_CZ5_RAM <- bind_rows(df_CZ5_RAM, dum)  
  }
}


# get my data
load(paste0(harvest_path, '/df_dCFC_MOD05_FOR_1dd.Rdata')) #  <--- df_dCFC_MOD05_FOR_1dd
load(paste0(harvest_path, '/df_dCFC_MOD05_DFO_1dd.Rdata')) #  <---  df_dCFC_MOD05_DFO_1dd
load(paste0(harvest_path, '/df_dCFC_MOD05_EFO_1dd.Rdata')) #  <---  df_dCFC_MOD05_EFO_1dd


# get ClimZones
cz5_map_fname <- paste0(results_path, '/ancillary_information/koppen-Geiger_360x180_5zones.nc')
cz5_map <- raster(cz5_map_fname, varname = 'climzone')
cz5_df <- as.data.frame(cz5_map, xy = T, long = T) %>% 
  dplyr::mutate(lon = round(x, digits = 8), lat = round(y, digits = 8)) %>%
  dplyr::filter(!is.na(value), value < 5) %>%
  dplyr::mutate(region = dplyr::recode_factor(value, `1` = "Tropical", `2` = "Arid", `3` = "Temperate", `4` = "Boreal")) %>%
  dplyr::select(-x, -y, -layer, -value)


# function to shift by 6 months if needed
regio.shift <- function(df){
dum <-  as_tibble(df) %>% 
  inner_join(cz5_df, by = c('lat', 'lon')) 

dum_S <- dum %>% filter(lat < 0) %>%
  mutate(new.month = recode(month, Jan = "Jul", Feb = "Aug", Mar = "Sep", 
                            Apr = "Oct", May = "Nov", Jun = "Dec")) %>%
  dplyr::select(-month) %>%
  dplyr::rename(month = new.month)

dum_N <- dum %>% filter(lat >= 0)

dum <- bind_rows(dum_N, dum_S) %>% 
  mutate(month = factor(month, levels = month.abb))

return(dum)}



# combine all
df_CZ5_S4T <- bind_rows(
  df_dCFC_MOD05_DFO_1dd %>% 
    #inner_join(cz5_df, by = c('lat', 'lon')) %>%
    regio.shift() %>%
    group_by(month, region) %>%
    summarize(dCFC_CZ5 = mean(dCFC, na.rm = T),
              dCFC_CZ5_STD_err = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC)))) %>%
    mutate(PFT = factor('DFO', levels = PFTs)),
  df_dCFC_MOD05_EFO_1dd %>% 
    #inner_join(cz5_df, by = c('lat', 'lon')) %>%
    regio.shift() %>%
    group_by(month, region) %>%
    summarize(dCFC_CZ5 = mean(dCFC, na.rm = T),
              dCFC_CZ5_STD_err = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC)))) %>%
    mutate(PFT = factor('EFO', levels = PFTs)),
  df_dCFC_MOD05_FOR_1dd %>% 
    #inner_join(cz5_df, by = c('lat', 'lon')) %>%
    regio.shift() %>%
    group_by(month, region) %>%
    summarize(dCFC_CZ5 = mean(dCFC, na.rm = T),
              dCFC_CZ5_STD_err = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC)))) %>%
    mutate(PFT = factor('TOT', levels = PFTs))) 


# combine
df_CZ5 <- bind_rows(
  df_CZ5_RAM %>% 
    dplyr::mutate(method = factor('RAM', levels = c('S4T', 'RAM'))),
  df_CZ5_S4T %>% 
    dplyr::mutate(method = factor('S4T', levels = c('S4T', 'RAM'))))


save('df_CZ5', 'cz5_df', file = paste0(harvest_path, '/df_RAM-vs-S4T_CZ5.RData'))
