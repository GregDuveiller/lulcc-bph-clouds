# Regroup maps and make test summary plots

library(dplyr)
library(ncdf4)
library(raster)
library(ggplot2)
library(here)
library(tidyr)
library(grid)



# load S4T dataset
nc <- nc_open(filename = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc')
rs_DFO_delta <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = '/ESS_Datasets/USERS/Filipponi/clouds/s4t_cloud_modis_aqua_global/dataResults/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc',
                      varname = 'uncertainty', lvar = 3, level = 2)

# Combine both forest types in post-processing
rs_FOR_delta <- mosaic(x = rs_DFO_delta, y = rs_EFO_delta, fun = mean) 
# ==> could use weights based on actual proportions
names(rs_FOR_delta) <- names(rs_DFO_delta)

# Propagate uncertainty
rs_FOR_sigma <- mosaic(x = rs_DFO_sigma, y = rs_EFO_sigma, fun = mean) 
# ==> Should check if uncertainty is correctly propagated
names(rs_FOR_sigma) <- names(rs_DFO_sigma)


datInSigma <- abs(rs_FOR_delta) - rs_FOR_sigma 
hist(as.vector(datInSigma), breaks = c(-25,-10,-5,seq(-1,1,0.02),5), xlim = c(-1,1))


dat2exclude <- abs(rs_FOR_delta) < rs_FOR_sigma 
rs_FOR_delta[dat2exclude] <- NA

qtls <- quantile(as.vector(rs_FOR_delta),probs = c(0.005,.995), na.rm = T)
rs_FOR_delta[rs_FOR_delta < qtls[1]] <- NA
rs_FOR_delta[rs_FOR_delta > qtls[2]] <- NA

hist(as.vector(rs_FOR_delta), breaks = c(seq(-1,1,0.01)), xlim = c(-0.5,0.5))


plot(rs_FOR_delta[[5]], zlim = c(-0.1,0.1), 
     col = RColorBrewer::brewer.pal(9,'RdBu'))


zn.eur <- extent(-10,40,30,70)
zn.nam <- extent(-100,-70,30,50)

plot(zn.nam, add = T)

df_dum <- as.data.frame(rs_FOR_delta, xy = T, long = T)

df_FOR_delta <- df_dum %>% 
  dplyr::rename(lon = x, lat = y, delta_cfc = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(delta_cfc))



ggplot(df_FOR_delta %>% 
         filter(month == 'Jul'), 
       aes(x = lon, y = lat, fill = delta_cfc)) + 
  geom_raster() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.15,0.15), oob = scales::squish) +
  coord_cartesian(expand = F)



require(sf)
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)



col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'
latLims <- c(-56,86)

mk.zone <- function(lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = lbl, 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}

zn.eur <- mk.zone('eur',-10,20,42,58)
zn.nam <- mk.zone('nam',-120,-60,40,60)
zn.crn <- mk.zone('crn',-95,-82,36,44)
zn.ind <- mk.zone('ind',70,90,5,30)
zn.aus <- mk.zone('aus',140,155,-45,-18)
zn.rus <- mk.zone('rus',20,110,45,65)
zn.ama <- mk.zone('ama',-70,-45,-15,-5)
zn.afr <- mk.zone('afr',10,42,-25,-5)

zn <- bind_rows(zn.nam, zn.crn, zn.eur, zn.rus, zn.ama, zn.afr, zn.ind, zn.aus)

mk.tmp.plot <- function(zn.dum, mon = NULL, ylims = NULL){
  
  df.dum  <- df_FOR_delta %>%
    filter(lat > min(zn.dum$lat), lat < max(zn.dum$lat), 
           lon > min(zn.dum$lon), lon < max(zn.dum$lon)) %>%
    group_by(month) %>%
    summarize(mean_delta_cfc = mean(delta_cfc),
              stdE_delta_cfc = sd(delta_cfc)/sqrt(length(delta_cfc))) %>%
    mutate(sign = factor(sign(mean_delta_cfc), levels = c(-1,0,1) )) 
  
  if(!is.null(mon)){ df.dum$sign[df.dum$month == mon] <- 0 }
  
  g.tmp <- ggplot(df.dum) + 
    geom_bar(aes(x = month, y = mean_delta_cfc, fill = sign, colour = sign), stat = 'identity') +
    geom_errorbar(aes(x = month, colour = sign,
                      ymin = mean_delta_cfc - stdE_delta_cfc,
                      ymax = mean_delta_cfc + stdE_delta_cfc))+
    geom_hline(yintercept = 0) +
    scale_y_continuous('Change in CFC') + 
    scale_x_discrete('') +
    scale_fill_manual(values = c('-1' = col.pal[2], '1' = col.pal[8], '0' = 'Grey30')) +
    scale_colour_manual(values = c('-1'= col.pal[1], '1' = col.pal[9], '0' = 'Grey20')) +
    coord_cartesian(ylim = ylims) +
    theme_minimal()+
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_blank()) + 
    ggtitle(unique(zn.dum$lbl))
  
}

zns <- list(zn.eur, zn.nam, zn.crn, zn.ind, zn.aus, zn.rus, zn.ama, zn.afr)
ylims <- c(-0.07, 0.07)
# monthly maps 
for(mon in month.abb){
#g.ts <- lapply(X = zns, FUN = mk.tmp.plot, mon = mon)

g.eur <- mk.tmp.plot(zn.eur, mon = mon, ylims = ylims)
g.nam <- mk.tmp.plot(zn.nam, mon = mon, ylims = ylims)
g.ind <- mk.tmp.plot(zn.ind, mon = mon, ylims = ylims)
g.aus <- mk.tmp.plot(zn.aus, mon = mon, ylims = ylims)
g.rus <- mk.tmp.plot(zn.rus, mon = mon, ylims = c(-0.10, 0.04))
g.ama <- mk.tmp.plot(zn.ama, mon = mon, ylims = ylims)
g.afr <- mk.tmp.plot(zn.afr, mon = mon, ylims = ylims)
g.crn <- mk.tmp.plot(zn.crn, mon = mon, ylims = ylims)


g.map <- ggplot(df_FOR_delta %>% 
                  filter(month == mon)) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = delta_cfc)) +
  geom_path(data = zn, aes(group = lbl, x = lon, y = lat), color = 'white') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.12,0.12), oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


fpath <- 'tempFigures/'
fname <- paste0('Figure1_test_',which(month.abb == mon))
ncp <- 1/4
figW <- 14; figH <- 14; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map, vp = viewport(width = 1, height = 0.5, x = 0, y = 0.25, just = c(0,0)))
print(g.nam, vp = viewport(width = ncp, height = 0.25, x = 0*ncp, y = 0.75, just = c(0,0)))
print(g.crn, vp = viewport(width = ncp, height = 0.25, x = 1*ncp, y = 0.75, just = c(0,0)))
print(g.eur, vp = viewport(width = ncp, height = 0.25, x = 2*ncp, y = 0.75, just = c(0,0)))
print(g.rus, vp = viewport(width = ncp, height = 0.25, x = 3*ncp, y = 0.75, just = c(0,0)))
print(g.ama, vp = viewport(width = ncp, height = 0.25, x = 0*ncp, y = 0.00, just = c(0,0)))
print(g.afr, vp = viewport(width = ncp, height = 0.25, x = 1*ncp, y = 0.00, just = c(0,0)))
print(g.ind, vp = viewport(width = ncp, height = 0.25, x = 2*ncp, y = 0.00, just = c(0,0)))
print(g.aus, vp = viewport(width = ncp, height = 0.25, x = 3*ncp, y = 0.00, just = c(0,0)))

dev.off()
}





# Figure of max and min in the year

g.map.extr <- ggplot(df_FOR_delta %>% 
                       group_by(lat, lon) %>%
                       summarise(min_cfc = min(delta_cfc, na.rm = T),
                                 max_cfc = max(delta_cfc, na.rm = T)) %>%
                       gather(key = 'type', value = 'delta_cfc_xtr', c('min_cfc','max_cfc'))) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = delta_cfc_xtr)) +
  geom_path(data = zn, aes(group = lbl, x = lon, y = lat), color = 'white') +
  facet_wrap(~type, nc = 1) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.12,0.12), oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

fpath <- 'tempFigures/'
fname <- 'Figure2_test'
figW <- 14; figH <- 14; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.extr, vp = viewport(width = 1, height = 1, x = 0, y = 0, just = c(0,0)))
dev.off()


# Figure with 4 seasons

df.seas <- data.frame(month = month.abb, season = factor(c(rep('DJF',2),rep('MAM',3),rep('JJA',3),rep('SON',3),'DJF'), levels = c('DJF','MAM','JJA','SON')))

g.map.seas <- ggplot(df_FOR_delta %>% 
                       left_join(df.seas, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(delta_cfc_seas = mean(delta_cfc, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = delta_cfc_seas)) +
  geom_path(data = zn, aes(group = lbl, x = lon, y = lat), color = 'white') +
  facet_wrap(~season, nc = 2) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.08,0.08), oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



g.eur <- mk.tmp.plot(zn.eur, mon = NULL, ylims = ylims)
g.nam <- mk.tmp.plot(zn.nam, mon = NULL, ylims = ylims)
g.ind <- mk.tmp.plot(zn.ind, mon = NULL, ylims = ylims)
g.aus <- mk.tmp.plot(zn.aus, mon = NULL, ylims = ylims)
g.rus <- mk.tmp.plot(zn.rus, mon = NULL, ylims = c(-0.10, 0.04))
g.ama <- mk.tmp.plot(zn.ama, mon = NULL, ylims = ylims)
g.afr <- mk.tmp.plot(zn.afr, mon = NULL, ylims = ylims)
g.crn <- mk.tmp.plot(zn.crn, mon = NULL, ylims = ylims)




fpath <- 'tempFigures/'
fname <- 'Figure3b_test'
figW <- 14; figH <- 14; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.seas, vp = viewport(width = 1, height = 0.5, x = 0, y = 0.25, just = c(0,0)))
print(g.nam, vp = viewport(width = ncp, height = 0.25, x = 0*ncp, y = 0.75, just = c(0,0)))
print(g.crn, vp = viewport(width = ncp, height = 0.25, x = 1*ncp, y = 0.75, just = c(0,0)))
print(g.eur, vp = viewport(width = ncp, height = 0.25, x = 2*ncp, y = 0.75, just = c(0,0)))
print(g.rus, vp = viewport(width = ncp, height = 0.25, x = 3*ncp, y = 0.75, just = c(0,0)))
print(g.ama, vp = viewport(width = ncp, height = 0.25, x = 0*ncp, y = 0.00, just = c(0,0)))
print(g.afr, vp = viewport(width = ncp, height = 0.25, x = 1*ncp, y = 0.00, just = c(0,0)))
print(g.ind, vp = viewport(width = ncp, height = 0.25, x = 2*ncp, y = 0.00, just = c(0,0)))
print(g.aus, vp = viewport(width = ncp, height = 0.25, x = 3*ncp, y = 0.00, just = c(0,0)))
dev.off()




# Figure with 4 seasons (alternative)

df.seas <- data.frame(month = month.abb, season = factor(c(rep('DJF',2),rep('MAM',3),rep('JJA',3),rep('SON',3),'DJF'), levels = c('DJF','MAM','JJA','SON')))

g.map.seas <- ggplot(df_FOR_delta %>% 
                       left_join(df.seas, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(delta_cfc_seas = mean(delta_cfc, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = delta_cfc_seas)) +
  facet_wrap(~season, nc = 1) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.08,0.08), oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


fpath <- 'tempFigures/'
fname <- 'Figure3_test'
figW <- 9; figH <- 14; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.seas, vp = viewport(width = 1, height = 1, x = 0, y = 0, just = c(0,0)))
dev.off()
