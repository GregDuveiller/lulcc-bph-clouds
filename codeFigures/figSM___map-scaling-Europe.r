require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)


# set projection stuff -----
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
#vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'


world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)




load('dataFigures/df_dCFC_MOD05_FOR.Rdata')     # df_dCFC_MOD05_FOR
load('dataFigures/df_dCFC_MOD02_FOR.Rdata')     # df_dCFC_MOD02_FOR
load('dataFigures/df_dCFC_MODo2_FOR.Rdata')     # df_dCFC_MODo2_FOR
load('dataFigures/df_dCFC_MOD02_FOR_agr.Rdata') # df_dCFC_MOD02_FOR_agr


df_all <- bind_rows(
  df_dCFC_MOD05_FOR %>% 
    mutate(source = 'PixSize:0.05dd_GrdSize:35km_WinSize:7pix'),
  df_dCFC_MOD02_FOR %>% 
    mutate(source = 'PixSize:0.02dd_GrdSize:14km_WinSize:7pix'),
  df_dCFC_MODo2_FOR %>% 
    mutate(source = 'PixSize:0.02dd_GrdSize:35km_WinSize:17pix'),
  df_dCFC_MOD02_FOR_agr %>% 
    mutate(source = 'PixSize:0.02dd_GrdSize:35km_WinSize:7pix'))


xLims <- c(-10,45)
yLims <- c(35,65)

clr.Lims <- c(-0.08,0.08)

landColor <- 'grey60'
seaColor <- 'grey20'

iMonth <- 'May'

df_sub <- df_all %>%
  filter(month == iMonth)

g.map <- ggplot(df_sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  facet_wrap(~source, nc = 2) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


ggsave(filename = paste0('figSM___ProductComp-Europe-FOR-', iMonth, '.', fig.fmt),
       width = 6, height = 7, path = fig.path)



