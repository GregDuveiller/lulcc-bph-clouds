require(dplyr)
require(tidyr)
require(ggplot2)

load('dataResults/Results_from_Andrej/FOR_Greg.RData') # paired

dist_sel <- 40
fofr_sel <- 0.2

df <- paired %>% 
  dplyr::filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  dplyr::filter(dist > dist_sel, hl > fofr_sel) %>%
  dplyr::group_by(hour, month) %>%
  dplyr::summarize(delta_cfc = mean(diff/100, na.rm = T),
                   delta_cfc_ttest_pval = t.test(diff/100)$p.value,
                   delta_cfc_stddev = sd(diff/100, na.rm = T),
                   delta_cfc_stderr = sd(diff/100, na.rm = T)/sqrt(sum(!is.na(diff)))) %>%
  dplyr::mutate(month = factor(month.abb[month], levels = month.abb))

lim.colors <- c(-0.1, 0.1)


#ggplot(dum, aes(x = hl, y = diff)) + geom_point() + xlim(0,1) + coord_cartesian(xlim = c(0,1)) + stat_smooth(method = 'lm', formula = y ~ x - 1, fullrange = TRUE)

ggplot(df) +
  geom_tile(aes(x = factor(month), y = factor(hour), fill = delta_cfc)) +
  geom_point(aes(x = factor(month), y = factor(hour), 
                 alpha = factor(delta_cfc_ttest_pval < 0.05))) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  coord_polar()


ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = delta_cfc)) +
  geom_point(aes(y = factor(month), x = factor(hour), 
                 alpha = factor(sign(abs(delta_cfc) - 2*delta_cfc_stderr)))) +
  scale_alpha_manual(values = c('-1' == 0, '1' == 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors/5, oob = scales::squish)+
  coord_polar()


ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = delta_cfc)) +
  geom_point(aes(y = factor(month), x = factor(hour), 
                 alpha = factor(delta_cfc_ttest_pval < 0.05))) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors/5, oob = scales::squish)+
  coord_polar()


### extrapolating to 100 %

df <- paired %>% 
  dplyr::filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  dplyr::filter(dist > dist_sel, hl > fofr_sel) %>%
  dplyr::group_by(hour, month) %>%
  dplyr::summarize(delta_cfc = mean(diff/100, na.rm = T),
                   delta_cfc_ttest_pval = t.test(diff/100)$p.value,
                   delta_cfc_stddev = sd(diff/100, na.rm = T),
                   delta_cfc_stderr = sd(diff/100, na.rm = T)/sqrt(sum(!is.na(diff)))) %>%
  dplyr::mutate(month = factor(month.abb[month], levels = month.abb))







# try map #

require(sf)
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_110m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)
landColor <- 'grey60'
seaColor <- 'grey20'

dist_sel <- 40
fofr_sel <- 0.3

##

time_sel <- 14

df <- paired %>% 
  #dplyr::filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  dplyr::filter(dist > dist_sel, hl > fofr_sel) %>%
  dplyr::filter(hour == time_sel) %>%
  mutate(month = factor(month.abb[month], levels = month.abb))
  
g_map_months <- ggplot(df) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_point(aes(x = lon, y = lat, color = diff/100), size = 0.5) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors, oob = scales::squish) + 
  facet_wrap(~month, nc = 3) + 
  coord_sf(xlim = c(-10, 40), ylim = c(36, 64)) +
  theme(legend.position = 'bottom',
        legend.key.width =  unit(2.4, "cm")) +
  ggtitle(paste0('Estimated effect of afforestation from SYNOP\nat ',time_sel,':00 for dist above ',dist_sel,'km and forest frac diff above ', fofr_sel))


ggsave(paste0('SYNOP_maps_months',time_sel,'-',dist_sel,'km-',fofr_sel,'.png'), path = 'tempFigures/', plot = g_map_months, width = 7, height = 10)


## 

mont_sel <- 'Jul'

df <- paired %>% 
  #dplyr::filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  dplyr::filter(dist > dist_sel, hl > fofr_sel) %>%
  mutate(month = factor(month.abb[month], levels = month.abb)) %>%
  dplyr::filter(month == mont_sel) 

g_map_times <- ggplot(df %>% filter(hour %in% seq(0,24,2))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_point(aes(x = lon, y = lat, color = diff/100), size = 0.5) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) + 
  facet_wrap(~hour, nc = 3) + 
  coord_sf(xlim = c(-10, 40), ylim = c(36, 64)) +
  theme(legend.position = 'bottom',
        legend.key.width =  unit(2.4, "cm")) +
  ggtitle(paste0('Estimated effect of afforestation from SYNOP\nin ',mont_sel,' for dist above ',dist_sel,'km and forest frac diff above ', fofr_sel))


ggsave(paste0('SYNOP_maps_times',mont_sel,'-',dist_sel,'km-',fofr_sel,'.png'), path = 'tempFigures/', plot = g_map_times, width = 7, height = 10)

