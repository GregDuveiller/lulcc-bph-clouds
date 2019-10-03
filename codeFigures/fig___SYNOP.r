
require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)



vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'

world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)




col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'

zlims <- c(-0.07, 0.07)
pointSize <- 1.5

xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)


# 
# 
# require(maps)
# mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
# 



load('dataFigures/df_SYNOP_agr_4polarplots_eur.RData')


pts_df <- df_SYNOP_loc %>% 
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

i.thr.dist.max <- 60
i.thr.dist.min <- 30
i.thr.dfor.min <- 0.2
i.thr.nyrs.min <- 7

# Location map
g.map.synop <- ggplot(pts_df %>%
                        filter(thr.dist.max == i.thr.dist.max,
                               thr.dist.min == i.thr.dist.min,
                               thr.nyrs.min == i.thr.nyrs.min,
                               thr.dfor.min == i.thr.dfor.min)) +
  geom_sf(data = europe_laea, fill = landColor, size = 0) +
  geom_sf(aes(colour =  n), size = pointSize) + 
  scale_colour_viridis_c() +
  coord_sf(xlim = xLims, ylim = yLims, expand = F) +
  ggtitle('Location of valid synop stations') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'none',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.text = element_text(size = rel(1.1)),
        axis.title = element_blank(),
        title = element_text(size = rel(1.3))) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# SYNOP wheel
ggplot(df_SYNOP_agr %>%
         filter(thr.dist.max == i.thr.dist.max,
                thr.dist.min == i.thr.dist.min,
                thr.nyrs.min == i.thr.nyrs.min,
                thr.dfor.min == i.thr.dfor.min), 
       aes(x = hour, y = month)) +
    geom_tile(aes(fill = dCFC)) +
    geom_point(aes(alpha = val.signif)) +
    scale_alpha_manual(values = c(0,1), guide = F) +
    scale_fill_gradientn('Change in cloud fraction cover',
                         colors = RColorBrewer::brewer.pal(9, 'RdBu'),
                         limits = zlims, oob = scales::squish) +
  coord_polar() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
