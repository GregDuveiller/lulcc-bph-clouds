require(dplyr)
require(tidyr)
require(ggplot2)

load('dataResults/Results_from_Andrej/FOR_Greg.RData') # paired



dist_sel <- 40
fofr_sel <- 0.2

df <- paired %>% 
  rename(forest.frc.chg = hl) %>%
  mutate(dCFC = diff/100) %>%
  filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  filter(dist > dist_sel, forest.frc.chg > fofr_sel) %>%
  group_by(hour, month) %>%
  summarize(dCFC_mu = mean(dCFC, na.rm = T),
            dCFC_ttest_pval = t.test(dCFC)$p.value,
            dCFC_stddev = sd(dCFC, na.rm = T),
            dCFC_stderr = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC)))) %>%
  dplyr::mutate(month = factor(month.abb[month], levels = month.abb))

lim.colors <- c(-0.1, 0.1)

#ggplot(dum, aes(x = hl, y = diff)) + geom_point() + xlim(0,1) + coord_cartesian(xlim = c(0,1)) + stat_smooth(method = 'lm', formula = y ~ x - 1, fullrange = TRUE)

ggplot(df) +
  geom_tile(aes(x = factor(month), y = factor(hour), fill = dCFC_mu)) +
  geom_point(aes(x = factor(month), y = factor(hour), 
                 alpha = factor(dCFC_ttest_pval < 0.05))) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors, oob = scales::squish) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  coord_polar()


ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = dCFC_mu)) +
  geom_point(aes(y = factor(month), x = factor(hour), 
                 alpha = factor(sign(abs(dCFC_mu) - 2*dCFC_stderr)))) +
  scale_alpha_manual(values = c('-1' == 0, '1' == 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors, oob = scales::squish)+
  coord_polar()


ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = dCFC_mu)) +
  geom_point(aes(y = factor(month), x = factor(hour), 
                 alpha = factor(dCFC_ttest_pval < 0.05))) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors/5, oob = scales::squish)+
  coord_polar()


# try extrapolating to 100 ----


dist_sel <- 30
fofr_sel <- 0


df.dum <- paired %>% 
  rename(forest.frc.chg = hl) %>%
  mutate(dCFC = diff/100) %>%
  filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 11) %>%
  filter(dist > dist_sel, forest.frc.chg > fofr_sel) 


ggplot(df.dum %>% 
         filter(hour == 10, month == 2) %>%
         distinct(area.h,area.l,dCFC, .keep_all = T)) +
  geom_point(aes(x = area.h, y = area.l, colour = dCFC), size = 1)  +
  geom_abline() +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1))




df.sub <- df.dum %>% 
  filter(hour == 20, month == 3) %>%
  distinct(area.h,area.l,dCFC, .keep_all = T)

n.grid = 101
df.mesh <- data.frame(area.h = rep(seq(0,1, length.out = n.grid),times = n.grid), 
                      area.l = rep(seq(0,1, length.out = n.grid), each = n.grid))

fit <- lm(dCFC ~ area.h + area.l - 1, data = df.sub)
df.pred <- df.mesh %>%
  mutate(dCFC = predict(fit, newdata = df.mesh))

## TO DO>>>
# according to Ale, try not to have 'sorted' data of from more forest to less 
# forest and instead have them randomly fill the space... 
# 
# then, the fit should be equivalnet to the linear 1-D fit thru zero of all 
# points collapsed on the [0,1] to [1,0] line.
# 
# I probably need an new clear script to sort this out in a clean way ... 
  
  
ggplot(df.sub) +
  # geom_contour(aes(x = area.h, y = area.l, z = dCFC), bins = 100)+
  # geom_density_2d(aes(x = area.h, y = area.l, colour = stat(level))) + 
#   stat_density_2d(aes(x = area.h, y = area.l, fill = stat(level)), geom = "polygon") + 
  geom_raster(data = df.pred, aes(x = area.h, y = area.l, fill = dCFC)) +
  geom_point(aes(x = area.h, y = area.l, fill = dCFC), shape = 21,  size = 2) +
  geom_abline() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1))  





# (here we do it by fitting a slope thru 0, where 0 is no change, but should
# this not be zero forest?)

df <- paired %>% 
  rename(forest.frc.chg = hl) %>%
  mutate(dCFC = diff/100) %>%
  filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58, nyrs >= 8) %>%
  filter(dist > dist_sel, forest.frc.chg > fofr_sel) %>%
  group_by(hour, month) %>%
  summarize(dCFC_mu_100 = lm(dCFC ~ forest.frc.chg - 1)$coefficients[1],
            dCFC_mu = mean(dCFC, na.rm = T),
            fofr_mu = mean(forest.frc.chg, na.rm = T)) %>%
  dplyr::mutate(month = factor(month.abb[month], levels = month.abb))

ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = dCFC_mu_100)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors, oob = scales::squish)+
  coord_polar()










# try map ----

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

