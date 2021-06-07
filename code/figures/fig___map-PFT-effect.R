#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure showing cloud formation due to different forest types
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(ggplot2)
require(dplyr)
require(sf)


## Data preparation -----

# general parametrization

xLims <- c(-10,45)
yLims <- c(35,65)
iMonths <- c('Jun','Jul','Aug')


# set projection stuff
world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)


# load data
load(paste0(dat4fig_path, "/df_dCFC_MOD02_DFO.Rdata")) # <-- "df_dCFC_MOD02_DFO"
load(paste0(dat4fig_path, "/df_dCFC_MOD02_EFO.Rdata")) # <-- "df_dCFC_MOD02_EFO"

# combine them
df_all <- bind_rows(
  df_dCFC_MOD02_DFO %>% mutate(PFT = 'Deciduous'),
  df_dCFC_MOD02_EFO %>% mutate(PFT = 'Evergreen'))

## Seasonal averages ---- 

# Labeller to add labels like 'sf' does on the maps
# works, but could have problem if degree symbol not properly available
geo_labeller <- function(x) {
  lbls <- c('1' = '°N', '-1' = '°S', '0' = '°')
  lbl <- paste0(abs(x), lbls[as.character(sign(x))])
  return(lbl)
}

yLims.zm <- c(36,66); chu <- 0.05
yLims.zm.2 <- c(yLims.zm[1] + chu, yLims.zm[2] - chu)

df.box <- data.frame(PFT = rep(c('Deciduous','Evergreen'), each = 5),
                     x = rep(c(5.5, 8.5, 8.5, 5.5, 5.5), times = 2),
                     # x = rep(c(iMonths[1], iMonths[3], iMonths[3], 
                     #           iMonths[1], iMonths[1]), times = 2),
                     y = rep(c(yLims.zm.2[1], yLims.zm.2[1], yLims.zm.2[2], 
                               yLims.zm.2[2], yLims.zm.2[1]), times = 2))


# the plot
g.lat.month <- ggplot(df_all %>%
                        mutate(lat_bin = cut(lat, breaks = seq(-90,90,2), 
                                             labels = seq(-89,89,2))) %>%
                        group_by(lat_bin, month, PFT) %>%
                        summarise(dCFC_latmonth = mean(dCFC, na.rm = T))) +
  geom_raster(aes(x = month, y = as.numeric(levels(lat_bin))[lat_bin], 
                  fill = dCFC_latmonth)) +
  geom_path(data = df.box, 
            aes(x = x, y = y), colour = 'grey20', size = 1) + 
  facet_wrap(~PFT) +
  scale_fill_gradientn('Change in cloud fractional cover',
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  scale_y_continuous(labels = geo_labeller) + 
  coord_cartesian(ylim = yLims.zm, expand = F) +
  ggtitle('Effect of afforestation on cloud fractional cover for different forest types') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'none',
        legend.key.width = unit(2.4, "cm"),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.2)),
        # axis.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.2)),
        panel.grid = element_line(color = seaColor),
        
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


## maps for summer ----

# NOTE: Ideally, it would be better to use the LAEA projection, but this needs 
# reprojecting the initial raster during the 'harvesting' phase... (to be done)

df_sub <- df_all %>%
  filter(month %in% iMonths) %>%
  group_by(lat, lon, PFT) %>%
  summarise(dCFC = mean(dCFC, na.rm = T)) %>%
  ungroup()

# df_sub_coord <- df_sub %>%
#   st_as_sf(coords = c("lon","lat")) %>%
#   st_set_crs(st_crs(world)) %>%
#   st_transform(laes_prj) %>% ungroup()
# 
# df_sub$X <- st_coordinates(df_sub_coord$geometry)[,1]
# df_sub$Y <- st_coordinates(df_sub_coord$geometry)[,2]

g.maps <- ggplot(df_sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  scale_fill_gradientn('Change in cloud fractional cover',
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  facet_wrap(~PFT) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  #  ggtitle('Effect of different forest types') +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))






## Printing the final plot -----
fig.name <- 'fig___PFT-effect-Europe'
fig.width <- 9; fig.height <- 10;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

ysh <- 0.42
print(g.lat.month, vp = viewport(width = 1, height = ysh, x = 0.0, y = 1 - ysh, just = c(0,0)))
print(g.maps, vp = viewport(width = 1, height = 1 - ysh, x = 0.0, y = 0.0, just = c(0,0)))

grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.02, "npc"), y = unit(0.54, "npc"), gp = gpar(fontsize = 18))


# add arrows to link two plots
grid.lines(x = c(0.28, 0.28), y = c(0.606, 0.55), 
           gp = gpar(col = "grey20", lwd = 2), 
           arrow = arrow(type = "open", length = unit(2,"mm")))
grid.lines(x = c(0.76, 0.76), y = c(0.606, 0.55), 
           gp = gpar(col = "grey20", lwd = 2),  
           arrow = arrow(type = "open", length = unit(2,"mm")))



dev.off()

