### XTRA FIGURE showing the scaling effect ----

# load necessary packages
require(dplyr)
require(tidyr)
require(grid)
require(ggplot2)
require(sf)
require(here)



## Initial data preparation and parametrization ---- 

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

# combine all
df_all <- bind_rows(
  df_dCFC_MOD05_FOR %>% 
    mutate(pix.size = '0.05 dd', grd.size = '35 km', win.size = '7 pix',
           lbl.long = 'a) Original\n[PixSize:0.05dd GrdSize:35km WinSize:7pix]',
           lbl.short = 'a'),
  df_dCFC_MOD02_FOR %>% 
    mutate(pix.size = '0.02 dd', grd.size = '14 km', win.size = '7 pix',
           lbl.long = 'b) Refined\n[PixSize:0.02dd GrdSize:14km WinSize:7pix]',
           lbl.short = 'b'),
  df_dCFC_MODo2_FOR %>% 
    mutate(pix.size = '0.02 dd', grd.size = '35 km', win.size = '17 pix',
           lbl.long = 'c) Refined over larger window\n[PixSize:0.02dd GrdSize:35km WinSize:17pix]',
           lbl.short = 'c'),
  df_dCFC_MOD02_FOR_agr %>% 
    mutate(pix.size = '0.02 dd', grd.size = '35 km', win.size = '7 pix',
           lbl.long = 'd) Refined reagregated\n[PixSize:0.02dd GrdSize:35km WinSize:7pix]',
           lbl.short = 'd'))

# set-up plotting parameters
xLims <- c(-10,45)
yLims <- c(35,65)

clr.Lims <- c(-0.08,0.08)

landColor <- 'grey60'
seaColor <- 'grey20'



## Plot the maps ----

iMonth <- 'Jul'

df_sub <- df_all %>%
  filter(month == iMonth)

g.map <- ggplot(df_sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  scale_fill_gradientn('Change in cloud cover fraction', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  facet_wrap(~lbl.long, nc = 2) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  ggtitle('The effect of scale on the estimation cloud fraction cover change due to afforestation', 
          subtitle = paste('Values for the month of', month.name[which(month.abb == iMonth)], 
                           'across Europe')) +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

fig.name <- 'figSM___show-scale-effect'
fig.width <- 8; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname, plot = g.map, 
       width = fig.width, height = fig.height)

