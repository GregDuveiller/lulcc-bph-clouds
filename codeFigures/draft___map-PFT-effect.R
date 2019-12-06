### FIGURE to show the PFT differences ----

# load packages

require(ggplot2)
require(dplyr)
require(sf)




## set projection stuff -----



world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)


# load data
load('dataFigures/df_dCFC_MOD02_DFO.Rdata')     # df_dCFC_MOD02_DFO
load('dataFigures/df_dCFC_MOD02_EFO.Rdata')     # df_dCFC_MOD02_EFO

# combine them
df_all <- bind_rows(
  df_dCFC_MOD02_DFO %>% mutate(PFT = 'Deciduous'),
  df_dCFC_MOD02_EFO %>% mutate(PFT = 'Evergreen'))



xLims <- c(-10,45)
yLims <- c(35,65)

clr.Lims <- c(-0.08,0.08)
zLims <- c(-0.12, 0.12)

landColor <- 'grey60'
seaColor <- 'grey20'

# iMonths <- c('Mar','Apr','May')
# iMonths <- c('Jun','Jul','Aug')
iMonths <- c('Apr','Aug','Dec')

df_sub <- df_all %>%
  filter(month %in% iMonths) 

# vctr.month.names <- month.name
# names(vctr.month.names) <- month.abb

g.maps <- ggplot(df_sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  facet_grid(PFT~month) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
#  ggtitle('Effect of different forest types') +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'top',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# ggsave(filename = paste0('fig___PFT-effect-Europe', '.', fig.fmt),
#        width = 9, height = 7, path = fig.path)


df_summary <- df_all %>%
  group_by(month, PFT) %>%
  summarise(median = median(dCFC, na.rm = T),
            pctl25 = quantile(dCFC, probs = 0.25, na.rm = T),
            pctl75 = quantile(dCFC, probs = 0.75, na.rm = T))


g.boxp <- ggplot(df_summary) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  # geom_ribbon(data = data.frame(month = factor(iMonths, levels = month.abb, ordered = T),
  #                               ymin = min(zLims), ymax = max(zLims)),
  #             aes(x = month, ymin = ymin, ymax = ymax), colour = 'grey60') +
  geom_errorbar(aes(x = month, 
                    ymin = pctl25, ymax = pctl75, 
                    colour = PFT), 
                position = position_dodge(width = 0.5)) +
  geom_point(aes(x = month, y = median, colour = PFT), 
             fill = 'white', shape = 21, size = 2,
             position = position_dodge(width = 0.5)) +
  scale_y_continuous('Change in cloud cover fraction') +
  scale_x_discrete('') +
  scale_colour_manual('Target forest type:', 
                      values = c('Deciduous' = 'chartreuse3',
                                 'Evergreen' = 'forestgreen')) +
  coord_cartesian(ylim = clr.Lims) +
  theme(legend.position = 'bottom')


# printing the final plot -----
fig.name <- 'fig___PFT-effect-Europe'
fig.width <- 9; fig.height <- 10;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

ysh <- 0.65
print(g.maps, vp = viewport(width = 1, height = ysh, x = 0.0, y = 1-ysh, just = c(0,0)))
print(g.boxp, vp = viewport(width = 1, height = 1-ysh, x = 0.0, y = 0.0, just = c(0,0)))
dev.off()

