

# get RAM's data

CZ5_regions <- c('Tropical', 'Arid', 'Temperate', 'Boreal')
PFTs <- c('TOT','EFO','DFO')
df_CZ5_RAM <- data.frame()
for(region in CZ5_regions){
  for(pft in PFTs){
  dum <- read.csv(file = paste0('dataResults/Results_from_RAM/CLIMzone5/',region,'_',pft,'.csv')) %>%
    mutate(month = factor(month.abb, levels = month.abb),
           region = factor(region, levels = CZ5_regions),
           PFT = factor(pft, levels = PFTs),
           delta_cfc_CZ5 = Slope / 100,
           delta_cfc_CZ5_STD_err = STD_err / 100) %>%
    dplyr::select(-Slope, -STD_err, -Pvalue)
  df_CZ5_RAM <- bind_rows(df_CZ5_RAM, dum)  
  }
}
# 
# ggplot(df_CZ5) +
#   geom_bar(aes(x = month, y = Slope, fill =  Pvalue), stat = 'identity') + 
#   geom_errorbar(aes(x = month, ymin = Slope - STD_err, ymax = Slope + STD_err)) +
#   facet_grid(PFT~region)
# 
# 


# get my data

load('dataFigures/df_FOR_delta_1dd.Rdata') # df_FOR_delta_1dd
load('dataFigures/df_DFO_delta_1dd.Rdata') # df_DFO_delta_1dd
load('dataFigures/df_EFO_delta_1dd.Rdata') # df_EFO_delta_1dd

# get ClimZones
cz5_map <- raster('/ESS_EarthObs/CLIMATE_DATA/koppen-Geiger/koppen-Geiger_360x180_5zones.nc', varname = 'climzone')
cz5_df <- as.data.frame(cz5_map, xy = T, long = T) %>% 
  dplyr::mutate(lon = round(x, digits = 8), lat = round(y, digits = 8)) %>%
  dplyr::filter(!is.na(value), value < 5) %>%
  dplyr::mutate(region = dplyr::recode_factor(value, `1` = "Tropical", `2` = "Arid", `3` = "Temperate", `4` = "Boreal")) %>%
  dplyr::select(-x, -y, -layer, -value)
  
  


df_CZ5_S4T <- bind_rows(
  df_DFO_delta_1dd %>% 
    inner_join(cz5_df, by = c('lat', 'lon')) %>%
    group_by(month, region) %>%
    summarize(delta_cfc_CZ5 = mean(delta_cfc, na.rm = T),
              delta_cfc_CZ5_STD_err = sd(delta_cfc, na.rm = T)/sqrt(sum(!is.na(delta_cfc)))) %>%
    mutate(PFT = factor('DFO', levels = PFTs)),
  df_EFO_delta_1dd %>% 
    inner_join(cz5_df, by = c('lat', 'lon')) %>%
    group_by(month, region) %>%
    summarize(delta_cfc_CZ5 = mean(delta_cfc, na.rm = T),
              delta_cfc_CZ5_STD_err = sd(delta_cfc, na.rm = T)/sqrt(sum(!is.na(delta_cfc)))) %>%
    mutate(PFT = factor('EFO', levels = PFTs)),
  df_FOR_delta_1dd %>% 
    inner_join(cz5_df, by = c('lat', 'lon')) %>%
    group_by(month, region) %>%
    summarize(delta_cfc_CZ5 = mean(delta_cfc, na.rm = T),
              delta_cfc_CZ5_STD_err = sd(delta_cfc, na.rm = T)/sqrt(sum(!is.na(delta_cfc)))) %>%
    mutate(PFT = factor('TOT', levels = PFTs))) 


# combine
df_CZ5 <- bind_rows(
  df_CZ5_RAM %>% 
    dplyr::mutate(method = factor('RAM', levels = c('S4T', 'RAM'))),
  df_CZ5_S4T %>% 
    dplyr::mutate(method = factor('S4T', levels = c('S4T', 'RAM'))))

# some plot
g_bars <- ggplot(df_CZ5) +
  geom_bar(aes(x = month, y = delta_cfc_CZ5, fill = method), stat = 'identity', position = 'dodge') +
  geom_hline(yintercept = 0, colour = 'grey40', size = 0.5) +
  facet_grid(region~PFT) + 
  scale_fill_discrete('Method used:') +
  scale_y_continuous('Change in cloud cover fraction') +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  ggtitle('Cloud fraction cover difference following afforestation\n as estimated using different methods')

ggsave('S4TvsRAM_bars.png', path = 'tempFigures/', plot = g_bars, width = 7, height = 8)

g_scatter <- ggplot(df_CZ5 %>%
         dplyr::select(-delta_cfc_CZ5_STD_err) %>%
         tidyr::spread(key = 'method', value = 'delta_cfc_CZ5')) +
  geom_point(aes(x = S4T, y = RAM, shape = PFT, colour = month), size = 2) +
  geom_abline(colour = 'grey40', size = 0.5) + 
  coord_equal(ylim = c(-0.1,0.05), xlim = c(-0.1,0.05)) + 
  scale_color_viridis_d('Month:', option = 'D') +
  scale_shape_discrete('Forest type:') +
  ggtitle('Cloud fraction cover difference following afforestation\nas estimated using different methods')


ggsave('S4TvsRAM_scatter.png', path = 'tempFigures/', plot = g_scatter, width = 6, height = 6)
