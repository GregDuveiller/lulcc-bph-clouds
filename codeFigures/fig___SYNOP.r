
require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)


# set projection stuff -----
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
#vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'

world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)


# set some general graphical parameters -----
col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'

zlims <- c(-0.07, 0.07)
pointSize <- 1.5

xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)


# set the parameters to select the target points -----

i.thr.dist.max <- 100  # 60 80 100
i.thr.dist.min <- 30  # 20 30 40
i.thr.dfor.min <- 0   # 0.0 0.1 0.2
i.thr.nyrs.min <- 7   # 5 7 9


# Get the SYNOP data ready -----

load('dataFigures/df_SYNOP_agr_4polarplots_eur.RData')

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


# Location map -----

g.synop.map <- ggplot(pts_df_sub) +
  geom_sf(data = europe_laea, fill = landColor, size = 0) +
  geom_sf(data = pts_buffer , fill = 'grey70') + 
  geom_sf(aes(colour =  n), size = pointSize) + 
  scale_colour_viridis_c('Number of valid observations per pair') +
  coord_sf(xlim = xLims, ylim = yLims, expand = F) +
  labs(tag = 'a') + 
  ggtitle('Location of suitable SYNOP station pairs') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = c(0.5,0.12),
        legend.text = element_text(size = rel(0.8)),
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.direction = "horizontal",
        panel.grid = element_line(color = seaColor),
        #axis.text = element_text(size = rel(1.1)),
        axis.title = element_blank()) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))


# SYNOP wheel -----

big.title <- 'Effect of change in forest cover on cloud cover based on SYNOP'
sub.title <-  paste0(' MinDist: ', i.thr.dist.min, 'km',
                     ' | MaxDist: ', i.thr.dist.max, 'km', 
                     ' | MinYears: ', i.thr.nyrs.min, 'yrs',
                     ' | MinDfor: ', 100 * i.thr.dfor.min, '%')

g.synop.wheel <- ggplot(df_SYNOP_agr %>%
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
  labs(tag = 'b') + 
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# comparison with MODIS...  -----

df.SYNOP <- df_SYNOP_agr %>%
  filter(thr.dist.max == i.thr.dist.max,
         thr.dist.min == i.thr.dist.min,
         thr.nyrs.min == i.thr.nyrs.min,
         thr.dfor.min == i.thr.dfor.min, 
         hour %in% c('13:00'))


# load MODIS data
load("dataFigures/df_dCFC_MOD02_FOR.Rdata") # df_dCFC_MOD02_FOR

pts_MODIS_laea <- df_dCFC_MOD02_FOR %>%
  filter(lon > -10, lon < 50,
         lat > 30, lat < 65) %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj) 

df.MODIS <- pts_MODIS_laea %>%
  st_intersection(y = pts_buffer) %>%   # This is the limiting step... 
  as.data.frame() %>%
  dplyr::group_by(month) %>%
  summarise(MODIS.dCFC.mu = mean(dCFC, na.rm = T),
            MODIS.dCFC.sd = sd(dCFC, na.rm = T),
            MODIS.dCFC.se = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC))))


df.combo <- df.SYNOP %>%
  #transmute(month = factor(month, levels = month.abb), SYNOP.dCFC.mu = dCFC) %>%
  rename(SYNOP.dCFC.mu = dCFC, SYNOP.dCFC.se = se.fit) %>%
  dplyr::select(SYNOP.dCFC.mu, SYNOP.dCFC.se, val.signif, month) %>%
  left_join(df.MODIS, by = 'month')

df.combo2 <- left_join(
  df.combo %>% 
    dplyr::select(month, MODIS.dCFC.mu, SYNOP.dCFC.mu) %>%
    pivot_longer(cols = c('MODIS.dCFC.mu','SYNOP.dCFC.mu'), 
                 names_to = 'source', 
                 names_pattern = "(.*).dCFC.mu",
                 values_to = 'dCFC.mu'),
  df.combo %>% 
    dplyr::select(month, MODIS.dCFC.se, SYNOP.dCFC.se) %>%
    pivot_longer(cols = c('MODIS.dCFC.se','SYNOP.dCFC.se'), 
                 names_to = 'source',
                 names_pattern = "(.*).dCFC.se",
                 values_to = 'dCFC.se'),
  by = c('month', 'source'))



# possible plot 1: based on barplot 
g.confr.bars <- ggplot(df.combo2) +
  geom_bar(aes(x = month, y = dCFC.mu, fill = source),
           stat = 'identity', position = "dodge") + 
  geom_errorbar(aes(ymin = dCFC.mu - dCFC.se, x = month, 
                    ymax = dCFC.mu + dCFC.se, color = source),
           stat = 'identity', position = "dodge") +
  scale_y_continuous('Change in cloud fraction cover') +
  geom_hline(aes(yintercept = 0), colour = 'grey30') +
  labs(tag = 'c') + 
  theme(legend.position = c(0.9,0.9),
        axis.title = element_text(size = rel(1.1)), 
        axis.title.x = element_blank()) +
  ggtitle('Comparison with MODIS')


# possible plot 2: based on scatterplot
g.confr.scat <- ggplot(df.combo) +
  geom_point(aes(x = MODIS.dCFC.mu, y = SYNOP.dCFC.mu, colour = month)) + 
  geom_linerange(aes(x = MODIS.dCFC.mu, colour = month,
                     ymin = SYNOP.dCFC.mu - 2*SYNOP.dCFC.se, 
                     ymax = SYNOP.dCFC.mu + 2*SYNOP.dCFC.se)) + 
  geom_errorbarh(aes(y = SYNOP.dCFC.mu, colour = month,
                     xmin = MODIS.dCFC.mu - 2*MODIS.dCFC.se, 
                     xmax = MODIS.dCFC.mu + 2*MODIS.dCFC.se)) + 
  geom_abline()  +
  scale_y_continuous('Delta CFC from SYNOP') +
  scale_x_continuous('Delta CFC from MODIS') + 
  coord_equal(xlim = c(-0.02, 0.11), ylim = c(-0.02, 0.11)) +
  labs(tag = 'c') +
  theme(legend.position = c(0.9,0.5),
        axis.title = element_text(size = rel(1.1))) +
  ggtitle('Comparison with MODIS')


# possible plot 3: based on ranges... like simple boxplots 
g.confr.boxs <- ggplot(df.combo2) +
  geom_point(aes(x = month, y = dCFC.mu, colour = source),
           stat = 'identity') + 
  geom_linerange(aes(ymin = dCFC.mu - 2 * dCFC.se, x = month, 
                    ymax = dCFC.mu + 2 * dCFC.se, colour = source),
                stat = 'identity') +
  geom_hline(aes(yintercept = 0), colour = 'grey30') + 
  labs(tag = 'c') + 
  theme(legend.position = c(0.9,0.9),
        axis.title = element_text(size = rel(1.1)), 
        axis.title.x = element_blank()) +
  ggtitle('Comparison with MODIS')



# printing the final plot -----
fig.name <- 'fig___synop-delta-CFC'
fig.width <- 12; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g.synop.map,   vp = viewport(width = 0.4, height = 0.5, x = 0.0, y = 0.5, just = c(0,0)))
print(g.synop.wheel, vp = viewport(width = 0.6, height = 1.0, x = 0.4, y = 0.0, just = c(0,0)))
print(g.confr.bars,  vp = viewport(width = 0.4, height = 0.5, x = 0.0, y = 0.0, just = c(0,0)))
dev.off()

