#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make the main figure summarizing the global effects on cloud cover
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(dplyr)
require(tidyr)
require(grid)
require(ggplot2)
require(sf)
require(here)


## Initial data preparation and parametrization ---- 

# some general plot parametrization
boxColors <- c('light' = 'grey95', 'dark' = 'grey15')
latLims <- c(-56,86)

# load vector data for background
world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)

# Load data to plot...
load(paste0(dat4fig_path, '/df_dCFC_MOD05_FOR_1dd.Rdata'))  #  <--- 'df_dCFC_MOD05_FOR_1dd'

# get ROIs of interest
source('code/figures/ancillary__definingROIs.R')
zn <- zn %>% 
  mutate(boxColor = ifelse(uid %in% c('crn', 'ama'), 
                           'dark', 'light'))

## Maps of 4 seasons ----

# define the seasons
seasons <- factor(x = c(rep('DJF',2), rep('MAM',3), 
                        rep('JJA',3), rep('SON',3), 'DJF'), 
                  levels = c('DJF','MAM','JJA','SON'), 
                  labels = c('December to February (DJF)',
                             'March to May (MAM)',
                             'June to August (JJA)',
                             'September to November (SON)'))
df.seasonal <- data.frame(month = month.abb, season = seasons)

# make the plot
g.map.seasonal <- ggplot(df_dCFC_MOD05_FOR_1dd %>% 
                       left_join(df.seasonal, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(dCFC_seas = mean(dCFC, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC_seas)) +
  geom_path(data = zn, aes(group = lbl, x = lon, y = lat, color = boxColor)) +
  facet_wrap(~season, nc = 1) +
  scale_colour_manual(values = boxColors) +
  scale_fill_gradientn(colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  ggtitle('Seasonal patterns of cloud fractional cover change',
    subtitle = 'Resulting from potential afforestation') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'none',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.text = element_text(size = rel(1.1)),
        axis.title = element_blank(),
        title = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.2))) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


## Lat-Month summary plot (with legend) ----

# Labeller to add labels like 'sf' does on the maps
# works, but could have problem if degree symbol not properly available
geo_labeller <- function(x) {
  lbls <- c('1' = '°N', '-1' = '°S', '0' = '°')
  lbl <- paste0(abs(x), lbls[as.character(sign(x))])
  return(lbl)
}
# # Alt version with bquotes and all, but DOES NOT WORK
# geo_labeller <- function(x) {
#   lbls <- c('1' = 'N', '-1' = 'S', '0' = '')
#   lbl <- bquote(.(abs(x))*degree*lbls[as.character(sign(x))])
#   return(lbl)
# }

# the plot
g.lat.month <- ggplot(df_dCFC_MOD05_FOR_1dd %>%
                        mutate(lat_bin = cut(lat, breaks = seq(-90,90,4), 
                                             labels = seq(-88,88,4))) %>%
                        group_by(lat_bin, month) %>%
                        summarise(dCFC_latmonth = mean(dCFC, na.rm = T))) +
  geom_raster(aes(x = month, y = as.numeric(levels(lat_bin))[lat_bin], 
                  fill = dCFC_latmonth)) +
  scale_fill_gradientn('Change in cloud fractional cover',
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
    scale_y_continuous(labels = geo_labeller) + 
  coord_cartesian(ylim = c(-41.99,65.99), expand = F) +
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'top',
        legend.key.width = unit(2.4, "cm"),
        axis.text = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.2)),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




## Sub-plots of bar graphs for selected zones ---- 

# function to make the subplots
mk.tmp.plot <- function(zn.dum, mon = NULL, ylims = NULL){
  
  df.dum  <- df_dCFC_MOD05_FOR_1dd %>%
    filter(lat > min(zn.dum$lat), lat < max(zn.dum$lat), 
           lon > min(zn.dum$lon), lon < max(zn.dum$lon)) %>%
    group_by(month) %>%
    summarize(mean_dCFC = mean(dCFC),
              stdE_dCFC = sd(dCFC)/sqrt(length(dCFC))) %>%
    mutate(sign = factor(sign(mean_dCFC), levels = c(-1,0,1) )) 
  
  if(!is.null(mon)){ df.dum$sign[df.dum$month == mon] <- 0 }
  
  g.tmp <- ggplot(df.dum) + 
    geom_errorbar(aes(x = month, colour = sign,
                      ymin = mean_dCFC - stdE_dCFC,
                      ymax = mean_dCFC + stdE_dCFC)) +
    geom_bar(aes(x = month, y = mean_dCFC, fill = sign), stat = 'identity') +
    geom_hline(yintercept = 0) +
    scale_y_continuous('Change in CFrC') + 
    scale_x_discrete('', labels = strtrim(month.abb, 1)) +
    scale_fill_manual(values = c('-1' = col.pal[3], '1' = col.pal[length(col.pal) - 2], '0' = 'Grey30')) +
    scale_colour_manual(values = c('-1'= col.pal[2], '1' = col.pal[length(col.pal) - 1], '0' = 'Grey20')) +
    coord_cartesian(ylim = ylims) +
    theme_minimal()+
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.ticks.y = element_line(size = 0.5),
          #axis.text.x = element_blank(),
          axis.line.x = element_line(size = 0.5),
          axis.ticks.x = element_line(size = 0.5)) + 
    ggtitle(unique(zn.dum$lbl))
  
}

# subplots
g.eur <- mk.tmp.plot(zn.eur, mon = NULL, ylims = dcfcLims)
g.nam <- mk.tmp.plot(zn.nam, mon = NULL, ylims = dcfcLims)
g.ind <- mk.tmp.plot(zn.ind, mon = NULL, ylims = dcfcLims)
g.aus <- mk.tmp.plot(zn.aus, mon = NULL, ylims = dcfcLims + 0.02)
g.rus <- mk.tmp.plot(zn.rus, mon = NULL, ylims = dcfcLims - 0.02)
g.ama <- mk.tmp.plot(zn.ama, mon = NULL, ylims = dcfcLims)
g.afr <- mk.tmp.plot(zn.afr, mon = NULL, ylims = dcfcLims + 0.02)
g.crn <- mk.tmp.plot(zn.crn, mon = NULL, ylims = dcfcLims)
g.chi <- mk.tmp.plot(zn.chi, mon = NULL, ylims = dcfcLims + 0.02 )


## Printing the entire figure ----
fig.name <- 'fig___map-delta-CFC'
fig.width <- 14; fig.height <- 13; #fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

hm <- 1.0; wm <- 0.5
hs <- 0.45; ws <- 1 - wm
w <- ws/3; h <- (1 - hs - 0.02)/3; s <- wm

print(g.map.seasonal, vp = viewport(width = wm, height = hm, x = 0, y = 0, just = c(0,0)))
print(g.lat.month,    vp = viewport(width = ws, height = hs, x = wm, y = 1 - hs, just = c(0,0)))

print(g.ama, vp = viewport(width = w, height = h, y = 0 * h, x = s + 0 * w, just = c(0,0)))
print(g.crn, vp = viewport(width = w, height = h, y = 1 * h, x = s + 0 * w, just = c(0,0)))
print(g.nam, vp = viewport(width = w, height = h, y = 2 * h, x = s + 0 * w, just = c(0,0)))
print(g.afr, vp = viewport(width = w, height = h, y = 0 * h, x = s + 1 * w, just = c(0,0)))
print(g.ind, vp = viewport(width = w, height = h, y = 1 * h, x = s + 1 * w, just = c(0,0)))
print(g.eur, vp = viewport(width = w, height = h, y = 2 * h, x = s + 1 * w, just = c(0,0)))
print(g.aus, vp = viewport(width = w, height = h, y = 0 * h, x = s + 2 * w, just = c(0,0)))
print(g.chi, vp = viewport(width = w, height = h, y = 1 * h, x = s + 2 * w, just = c(0,0)))
print(g.rus, vp = viewport(width = w, height = h, y = 2 * h, x = s + 2 * w, just = c(0,0)))


grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.02, "npc"), y = unit(0.71, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("c")), x = unit(0.02, "npc"), y = unit(0.48, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("d")), x = unit(0.02, "npc"), y = unit(0.25, "npc"), gp = gpar(fontsize = 18))

grid.text(expression(bold("e")), x = unit(0.56, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize = 18))

grid.text(expression(bold("f")), x = unit(0.52, "npc"), y = unit(0.52, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("g")), x = unit(0.52, "npc"), y = unit(0.34, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("h")), x = unit(0.52, "npc"), y = unit(0.16, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("i")), x = unit(0.69, "npc"), y = unit(0.52, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("j")), x = unit(0.69, "npc"), y = unit(0.34, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("k")), x = unit(0.69, "npc"), y = unit(0.16, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("l")), x = unit(0.85, "npc"), y = unit(0.52, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("m")), x = unit(0.85, "npc"), y = unit(0.34, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("n")), x = unit(0.85, "npc"), y = unit(0.16, "npc"), gp = gpar(fontsize = 18))

dev.off()

