#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure showing the results with the SYNOP points
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)
require(raster)


## Initial data preparation and parametrization ---- 

# set projection stuff
world <- sf::st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)

# set some specific graphical parameters 
pointSize <- 1.5
xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)

# set the parameters to select the target points 
i.thr.dist.max <- 100  # 60 80 100
i.thr.dist.min <- 30  # 20 30 40
i.thr.dfor.min <- 0   # 0.0 0.1 0.2
i.thr.nyrs.min <- 7   # 5 7 9

# set time from SYNOP to compare with MODIS overpass (Aqua platform)
modisTime <- 14

# Get the SYNOP data ready
load(paste0(dat4fig_path, '/df_SYNOP_agr_4polarplots_eur.Rdata'))  #  <--- "df_SYNOP_agr" and "df_SYNOP_loc"

pts_df <- df_SYNOP_loc %>% 
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

pts_df_sub <- pts_df %>%
  filter(thr.dist.max == i.thr.dist.max,
         thr.dist.min == i.thr.dist.min,
         thr.nyrs.min == i.thr.nyrs.min,
         thr.dfor.min == i.thr.dfor.min)

pts_buffer <- pts_df_sub %>%
  sf::st_buffer(dist = 50000) %>%
  st_union()


## SYNOP point location map -----

g.synop.map <- ggplot(pts_df_sub) +
  geom_sf(data = europe_laea, fill = landColor, size = 0) +
  geom_sf(data = pts_buffer , fill = 'grey70') + 
  geom_sf(aes(colour =  n), size = pointSize) + 
  scale_colour_viridis_c('Number of valid observations per pair') +
  coord_sf(xlim = xLims, ylim = yLims, expand = F) +
  ggtitle('Location of suitable SYNOP station pairs') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        #legend.position = c(0.5,0.12),
        legend.text = element_text(size = rel(0.8)),
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.direction = "horizontal",
        panel.grid = element_line(color = seaColor),
        #axis.text = element_text(size = rel(1.1)),
        axis.title = element_blank()) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))


## SYNOP wheel -----

big.title <- 'Effect of afforestation on cloud fractional cover based on SYNOP'
sub.title <-  paste0(' MinDist: ', i.thr.dist.min, 'km',
                     ' | MaxDist: ', i.thr.dist.max, 'km', 
                     ' | MinYears: ', i.thr.nyrs.min, 'yrs')
# ' | MinDfor: ', 100 * i.thr.dfor.min, '%')

g.synop.wheel <- ggplot(df_SYNOP_agr %>%
                          filter(thr.dist.max == i.thr.dist.max,
                                 thr.dist.min == i.thr.dist.min,
                                 thr.nyrs.min == i.thr.nyrs.min,
                                 thr.dfor.min == i.thr.dfor.min), 
                        aes(x = hour, y = month)) +
  geom_tile(aes(fill = dCFC)) +
  geom_tile(aes(alpha = val.signif), fill = 'grey80') +
  # geom_point(aes(alpha = val.signif)) +
  # # THESE BELOW TO ADD AN AXIS AT MIDNIGHT
  # geom_path(data = data.frame(y = c(0.5,12.5), x = c(0.5,0.5)), 
  #           aes(x = x, y = y), colour = 'grey50', size = 0.5) + 
  # geom_point(data = data.frame(y = 1:12, x = rep(0.5, times = 12)), 
  #           aes(x = x, y = y), colour = 'grey50', size = 1, shape = 3) + 
  geom_path(data = data.frame(y = c(0.5,12.5,12.5,0.5,0.5), 
                              x = modisTime + c(0.5,0.5,1.5,1.5,1.5)), 
            aes(x = x, y = y), colour = 'grey20', size = 0.7) + 
  # annotate("segment", x=15, y=13.5, xend=15.5, yend=18,
  #          col="grey20", arrow=arrow(length=unit(0.2, "cm"))) +
  scale_alpha_manual(values = c(1,0), guide = F) +
  # scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_gradientn('Change in cloud fractional cover',
                       colors = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_polar() +
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.ticks.y = element_line(colour = 'grey50', 
                                    arrow = arrow(length = unit(0.15, "cm"),
                                                  type = 'open')),
        axis.ticks.length.y = unit(0.4, "cm"),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


## Comparison with MODIS...  -----

# select the right SYNOP parametrisations
df.SYNOP <- df_SYNOP_agr %>%
  filter(thr.dist.max == i.thr.dist.max,
         thr.dist.min == i.thr.dist.min,
         thr.nyrs.min == i.thr.nyrs.min,
         thr.dfor.min == i.thr.dfor.min, 
         hour %in% c(paste(modisTime,'00',sep = ':')))

# load MODIS data
load(paste0(dat4fig_path, "/df_dCFC_MOD02_FOR.Rdata")) #  <--- "df_dCFC_MOD02_FOR"
load(paste0(dat4fig_path, "/df_dCFC_MOD05_FOR.Rdata")) #  <--- "df_dCFC_MOD05_FOR"

# get data from the MOD02 CFrC dataset
df.MOD02 <- df_dCFC_MOD02_FOR %>%
  mutate(month = factor(month, ordered = T)) %>%
  filter(lon > -10, lon < 50,
         lat > 30, lat < 65) %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj) %>%
  st_intersection(y = pts_buffer) %>%   # This is the limiting step... 
  as.data.frame() %>%
  dplyr::group_by(month) %>%
  summarise(MOD02.dCFC.mu = mean(dCFC, na.rm = T),
            MOD02.dCFC.sd = sd(dCFC, na.rm = T),
            MOD02.dCFC.se = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC))))


# get data from the MOD02 CFrC dataset
df.MOD05 <- df_dCFC_MOD05_FOR %>%
  mutate(month = factor(month, ordered = T)) %>%
  filter(lon > -10, lon < 50,
         lat > 30, lat < 65) %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj) %>%
  st_intersection(y = pts_buffer) %>%   # This is the limiting step... 
  as.data.frame() %>%
  dplyr::group_by(month) %>%
  summarise(MOD05.dCFC.mu = mean(dCFC, na.rm = T),
            MOD05.dCFC.sd = sd(dCFC, na.rm = T),
            MOD05.dCFC.se = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC))))


# combine it all together
df.combo <- df.SYNOP %>%
  rename(SYNOP.dCFC.mu = dCFC, SYNOP.dCFC.se = se.fit) %>%
  dplyr::select(SYNOP.dCFC.mu, SYNOP.dCFC.se, val.signif, month) %>%
  left_join(df.MOD02, by = 'month') %>%
  left_join(df.MOD05, by = 'month')

df.combo2 <- left_join(
  df.combo %>% 
    dplyr::select(month, MOD02.dCFC.mu, MOD05.dCFC.mu, SYNOP.dCFC.mu) %>%
    pivot_longer(cols = c('MOD02.dCFC.mu', 'MOD05.dCFC.mu', 'SYNOP.dCFC.mu'), 
                 names_to = 'source', 
                 names_pattern = "(.*).dCFC.mu",
                 values_to = 'dCFC.mu'),
  df.combo %>% 
    dplyr::select(month, MOD02.dCFC.se, MOD05.dCFC.se, SYNOP.dCFC.se) %>%
    pivot_longer(cols = c('MOD02.dCFC.se', 'MOD05.dCFC.se', 'SYNOP.dCFC.se'), 
                 names_to = 'source',
                 names_pattern = "(.*).dCFC.se",
                 values_to = 'dCFC.se'),
  by = c('month', 'source'))

# making the barplot 
g.confr.bars <- ggplot(df.combo2) +
  geom_errorbar(aes(ymin = dCFC.mu - dCFC.se, x = month, 
                    ymax = dCFC.mu + dCFC.se, group = source),
                stat = 'identity', position = "dodge", color = 'grey30', size = 0.4) +
  geom_bar(aes(x = month, y = dCFC.mu, fill = source), colour = 'grey30',
           stat = 'identity', position = "dodge", size = 0.4) + 
  scale_y_continuous('Change in cloud fraction cover') +
  scale_fill_manual(values = c('MOD02'='chartreuse4', 'MOD05'='olivedrab2', 
                               'SYNOP'='cornflowerblue'),
                    labels = c('MOD02'='Satellite\n(refined)', 
                               'MOD05'='Satellite\n(original)', 'SYNOP'='Ground\nstation')) +
  geom_hline(aes(yintercept = 0), colour = 'grey30', size = 1.2) +
  theme_minimal()+
  theme(legend.position = c(0.62,0.10),
        #legend.position = c(0.9,0.85),
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        legend.spacing.x = unit(2.0, 'mm'),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5, colour = 'Grey20'),
        axis.ticks = element_line(size = 0.5, colour = 'Grey20'),
        axis.title = element_text(size = rel(1.1)), 
        axis.title.x = element_blank()) +
  ggtitle('Comparison with satellite estimations')



## Printing the final plot -----
fig.name <- 'fig___synop-delta-CFC'
fig.width <- 12; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g.synop.map,   vp = viewport(width = 0.40, height = 0.6, x = 0.00, y = 0.4, just = c(0,0)))
print(g.synop.wheel, vp = viewport(width = 0.59, height = 1.0, x = 0.41, y = 0.0, just = c(0,0)))
print(g.confr.bars,  vp = viewport(width = 0.40, height = 0.4, x = 0.00, y = 0.0, just = c(0,0)))

grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.95, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.42, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("c")), x = unit(0.02, "npc"), y = unit(0.38, "npc"), gp = gpar(fontsize = 18))

grid.lines(x = c(0.55, 0.53, 0.41), 
           y = c(0.25, 0.22, 0.22), 
           arrow = arrow(angle = 40, length = unit(0.2, "cm"), 
                         ends = "last", type = "open"))

dev.off()

