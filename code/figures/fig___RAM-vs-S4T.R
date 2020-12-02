#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure comparing results from different methods (S4T vs RAM)
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(dplyr)
require(grid)
require(ggplot2)
require(here)


## Initial data preparation and parametrization ---- 
load(paste0(dat4fig_path, '/df_RAM-vs-S4T_CZ5.RData')) # <--- "df_CZ5"
iPFT <- 'TOT'
# NOTE: Should check if the df above actually used the right combo for TOT...
# (did that harvester run after the tweaks in the DFO + EFO weighted combo)

colour.meth1 <- c('#95559B', '#B892BB')
colour.meth2 <- c('#D89800', '#ECD8A6')

method.lbls <- c('S4T' = 'Space-for-time\nsubstitution', 
                 'RAM' = 'Based on real\nforest cover changes')

## Make the bar plot ----
g_bars <- ggplot(df_CZ5 %>% 
                   filter(PFT == iPFT)) +
  geom_bar(aes(x = month, y = dCFC_CZ5, fill = method, colour = method), 
           stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(x = month, color = method,
                    ymin = dCFC_CZ5 - dCFC_CZ5_STD_err, 
                    ymax = dCFC_CZ5 + dCFC_CZ5_STD_err),
                position = 'dodge', show.legend = F) +
  geom_hline(yintercept = 0, colour = 'grey40', size = 0.5) +
  facet_wrap(~region, nc = 2) + 
  scale_fill_manual('Method used:', labels = method.lbls, 
                    values = c('S4T' = colour.meth1[2], 'RAM' = colour.meth2[2])) +
  scale_color_manual('Method used:', labels = method.lbls, 
                     values = c('S4T' = colour.meth1[1], 'RAM' = colour.meth2[1])) +
  scale_y_continuous('Change in cloud fractional cover') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(size = 0.5, fill = 'NA', colour = 'Grey20'),
        strip.text = element_text(size = rel(1.2)),
        axis.line = element_line(size = 0.5, colour = 'Grey20'),
        axis.ticks = element_line(size = 0.5, colour = 'Grey20'),
        axis.title = element_text(size = rel(1.1))) + 
  ggtitle('Change in cloud fractional cover following afforestation',
          subtitle = 'Averages over major Koppen-Geiger climate zones with different methods')


## make the maps ----

world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position='none',
                         plot.background = element_rect(fill = "transparent",colour = NA),
                         strip.background = element_rect(colour = "white", fill = "white"),
                         strip.text = element_blank(),
                         plot.title = element_text(size = rel(1.7), face = "bold")))


mapcolour <- 'grey10'

gMapTro <- ggplot(cz5_df %>% filter(region == 'Tropical')) +
  geom_sf(data = world, fill = landColor, size = 0) + 
  geom_raster(aes(x = lon, y = lat), fill = mapcolour) +
  theme_opts

gMapAri <- ggplot(cz5_df %>% filter(region == 'Arid')) +
  geom_sf(data = world, fill = landColor, size = 0) + 
  geom_raster(aes(x = lon, y = lat), fill = mapcolour) +
  theme_opts

gMapTem <- ggplot(cz5_df %>% filter(region == 'Temperate')) +
  geom_sf(data = world, fill = landColor, size = 0) + 
  geom_raster(aes(x = lon, y = lat), fill = mapcolour) +
  theme_opts

gMapBor <- ggplot(cz5_df %>% filter(region == 'Boreal')) +
  geom_sf(data = world, fill = landColor, size = 0) + 
  geom_raster(aes(x = lon, y = lat), fill = mapcolour) +
  theme_opts


## Printing the final plot -----
fig.name <- 'fig___RAM-vs-S4T'
fig.width <- 8; fig.height <- 8;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.3
h <- 0.3

print(g_bars,  vp = viewport(width = 1, height = 1, x = 0.0, y = 0.0, just = c(0,0)))
print(gMapTro, vp = viewport(width = w, height = h, x = 0.24, y = 0.44, just = c(0,0)))
print(gMapAri, vp = viewport(width = w, height = h, x = 0.70, y = 0.44, just = c(0,0)))
print(gMapTem, vp = viewport(width = w, height = h, x = 0.24, y = 0.02, just = c(0,0)))
print(gMapBor, vp = viewport(width = w, height = h, x = 0.70, y = 0.02, just = c(0,0)))

dev.off()
