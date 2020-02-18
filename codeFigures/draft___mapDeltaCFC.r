# Regroup maps and make test summary plots

library(dplyr)
library(ncdf4)
library(raster)
library(ggplot2)
library(here)
library(tidyr)
library(grid)


require(sf)
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)

# Load data...
load('dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata') # df_dCFC_MOD05_FOR_1dd.Rdata



col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'
latLims <- c(-56,86)

# get ROIs
source('codeFigures/ancillary__definingROIs.r')

# function to make subplots...
mk.tmp.plot <- function(zn.dum, mon = NULL, ylims = NULL){
  
  df.dum  <- df_dCFC_MOD05_FOR %>%
    filter(lat > min(zn.dum$lat), lat < max(zn.dum$lat), 
           lon > min(zn.dum$lon), lon < max(zn.dum$lon)) %>%
    group_by(month) %>%
    summarize(mean_dCFC = mean(dCFC),
              stdE_dCFC = sd(dCFC)/sqrt(length(dCFC))) %>%
    mutate(sign = factor(sign(mean_dCFC), levels = c(-1,0,1) )) 
  
  if(!is.null(mon)){ df.dum$sign[df.dum$month == mon] <- 0 }
  
  g.tmp <- ggplot(df.dum) + 
    geom_bar(aes(x = month, y = mean_dCFC, fill = sign, colour = sign), stat = 'identity') +
    geom_errorbar(aes(x = month, colour = sign,
                      ymin = mean_dCFC - stdE_dCFC,
                      ymax = mean_dCFC + stdE_dCFC))+
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


g.map <- ggplot(df_dCFC_MOD05_FOR_1dd %>% 
                  filter(month == mon)) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
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
fname <- paste0('Figure1_test_1dd_',which(month.abb == mon))
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

g.map.extr <- ggplot(df_dCFC_MOD05_FOR %>% 
                       group_by(lat, lon) %>%
                       summarise(min_cfc = min(dCFC, na.rm = T),
                                 max_cfc = max(dCFC, na.rm = T)) %>%
                       gather(key = 'type', value = 'dCFC_xtr', c('min_cfc','max_cfc'))) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC_xtr)) +
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

g.map.seas <- ggplot(df_dCFC_MOD05_FOR_1dd %>% 
                       left_join(df.seas, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(dCFC_seas = mean(dCFC, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC_seas)) +
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
fname <- 'Figure3b_test_1dd'
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

g.map.seas <- ggplot(df_dCFC_MOD05_FOR %>% 
                       left_join(df.seas, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(dCFC_seas = mean(dCFC, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC_seas)) +
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




# lat-month


g.lat.month <- ggplot(df_dCFC_MOD05_FOR %>% 
                       group_by(lat, month) %>%
                       summarise(dCFC_latmonth = mean(dCFC, na.rm = T))) +
  geom_raster(aes(x = month, y = lat, fill = dCFC_latmonth)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = c(-0.08,0.08), oob = scales::squish) +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



fpath <- 'tempFigures/'
fname <- 'Figure4_test'
figW <- 9; figH <- 6; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.lat.month, vp = viewport(width = 1, height = 1, x = 0, y = 0, just = c(0,0)))
dev.off()





