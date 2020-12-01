#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure showing some close-ups of the spatial data 
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################

require(ncdf4)
require(raster)
require(dplyr)
require(tidyr)
require(grid)
require(ggplot2)
require(sf)
require(here)



## Initial data preparation and parametrization ---- 

world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)

provs <- sf::st_read('data/input_data/world_vectors/ne_50m_admin_1_states_provinces_lines.shp', quiet = TRUE)
countries <- sf::st_read('data/input_data/world_vectors/ne_50m_admin_0_countries.shp', quiet = TRUE)
lakes <- sf::st_read('data/input_data/world_vectors/ne_50m_lakes.shp', quiet = TRUE)

# get ROIs
source('code/figures/ancillary__definingROIs.R')
zn <- zn %>% 
  filter(uid %in% c('crn', 'ama', 'ind', 'aus'))


## get map data from netcdf
require(raster)

# dpath <- 'dataResults/Results_from_Federico'
# fname <- 'ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc'

r <- brick(x = paste0(dat4fig_path, '/rstr_dCFC_MOD05_FOR.nc'))

plot.seq <- function(z, m, d, l){

zn.dum <- zn %>% filter(uid == z)
xLims <- c(min(zn.dum$lon), max(zn.dum$lon))
yLims <- c(min(zn.dum$lat), max(zn.dum$lat))

r_sub <- crop(r[[m]], c(xLims, yLims))

df.sub <- as.data.frame(r_sub, xy = T, long = T)
names(df.sub) <- c('lon', 'lat', 'month', 'dCFC')
df.sub <- df.sub %>% 
  mutate(month = factor(month.name[as.numeric(substr(month,2,2))],
                        levels = month.name)) %>%
  filter(!is.na(dCFC))

g.map <- ggplot(df.sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  geom_sf(data = provs, colour = 'grey20', size = 0.3, linetype = 'dotdash') +
  geom_sf(data = lakes, colour = 'blue', fill = 'lightblue', size = 0.4) +
  geom_sf(data = countries, colour = 'grey10', fill = NA, size = 0.4) +
  facet_wrap(~month, nr = d[1], nc= d[2]) +
  scale_fill_gradientn('Change in cloud fractional cover', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  ggtitle(zn.dum$lbl[1]) +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = ifelse(l == T, 'bottom', 'none'),
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

return(g.map)}


g.ama <- plot.seq(z = 'ama', m = 6:8, d = c(3,1), l = F)
g.crn <- plot.seq(z = 'crn', m = 6:8, d = c(3,1), l = F)
g.ind <- plot.seq(z = 'ind', m = 5:7, d = c(1,3), l = T)
# g.aus <- plot.seq(z = 'aus', m = 2:4, d = c(1,3), l = T)



## Printing the entire figure ----
fig.name <- 'figSM___map-delta-CFC-zoom'
fig.width <- 9; fig.height <- 12; #fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w1 <- 0.6; w2 <- 0.4
h1 <- 0.6; h2 <- 0.4; h3 <- 0

print(g.ama, vp = viewport(width = w1, height = h1, x = 0, y = h2 + h3, just = c(0,0)))
print(g.crn, vp = viewport(width = w2, height = h1, x = w1, y = h2 + h3, just = c(0,0)))
print(g.ind, vp = viewport(width = 1, height = h2, x = 0, y = h3, just = c(0,0)))
# print(g.aus, vp = viewport(width = 1, height = h3, x = 0, y = 0, just = c(0,0)))
# 
# 
# grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("b")), x = unit(0.02, "npc"), y = unit(0.71, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("c")), x = unit(0.02, "npc"), y = unit(0.48, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("d")), x = unit(0.02, "npc"), y = unit(0.25, "npc"), gp = gpar(fontsize = 18))
# 
dev.off()
