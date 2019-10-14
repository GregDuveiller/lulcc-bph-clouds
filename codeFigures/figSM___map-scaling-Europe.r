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
    mutate(pix.size = '0.05 dd', grd.size = '35 km', win.size = '7 pix',
           lbl.long = 'a) PixSize:0.05dd GrdSize:35km WinSize:7pix',
           lbl.short = 'a'),
  df_dCFC_MOD02_FOR %>% 
    mutate(pix.size = '0.02 dd', grd.size = '14 km', win.size = '7 pix',
           lbl.long = 'b) PixSize:0.02dd GrdSize:14km WinSize:7pix',
           lbl.short = 'b'),
  df_dCFC_MODo2_FOR %>% 
    mutate(pix.size = '0.02 dd', grd.size = '35 km', win.size = '17 pix',
           lbl.long = 'c) PixSize:0.02dd GrdSize:35km WinSize:17pix',
           lbl.short = 'c'),
  df_dCFC_MOD02_FOR_agr %>% 
    mutate(pix.size = '0.02 dd', grd.size = '35 km', win.size = '7 pix',
           lbl.long = 'd) PixSize:0.02dd GrdSize:35km WinSize:7pix',
           lbl.short = 'd'))


xLims <- c(-10,45)
yLims <- c(35,65)

clr.Lims <- c(-0.08,0.08)

landColor <- 'grey60'
seaColor <- 'grey20'

iMonth <- 'Aug'

df_sub <- df_all %>%
  filter(month == iMonth)

g.map <- ggplot(df_sub) + 
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC)) +
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  facet_wrap(~lbl.long, nc = 2) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims)+
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


ggsave(filename = paste0('figSM___ProductComp-Europe-FOR-', iMonth, '.', fig.fmt),
       width = 6, height = 7, path = fig.path)



## other attempt/// 
df_sum <- df_all %>%
  filter(grd.size == "35 km") %>%
  group_by(lbl.short, month) %>%
  summarise(median = median(dCFC, na.rm = T),
            pctl25 = quantile(dCFC, probs = 0.25, na.rm = T),
            pctl75 = quantile(dCFC, probs = 0.75, na.rm = T)) 

g.scale <- ggplot(df_sum) + 
  geom_point(aes(x = month, y = median, colour = lbl.short), 
             position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(x = month, ymin = pctl25, ymax = pctl75, 
                    colour = lbl.short), position = position_dodge(width = 0.9)) +
  scale_y_continuous('Change in cloud fraction cover') +
  theme(legend.position = 'bottom')


df_sum2 <- df_sum %>%
  pivot_wider(id_cols = c('lbl.short', 'month'), 
              names_from = lbl.short, 
              values_from = c('median', 'pctl25', 'pctl75')) %>%
  rename(win.size.effect = median_c,
         pix.size.effect = median_d) %>%
  pivot_longer(cols = c('win.size.effect', 'pix.size.effect'),
               names_to = "cases", values_to = "dCFC") %>%
  rename(ref_case = median_a)

g.scat <- ggplot(df_sum2) +
  geom_point(aes(x = ref_case, y = dCFC, 
                 fill = month), shape = 21, size = 2) +
  geom_abline(colour = 'grey30') +
  facet_wrap(~cases, nc = 2) + 
  theme(legend.position = 'bottom')



# ## TO BE REFINED IF REALLY NEEDED... 
# ## Current problem: lat-lons are not identical in each dataset so no join 
# 
# #### extra fig on seasonal comparison of scales...
# 
# df_all <- inner_join(by = c('lat', 'lon', 'month'),
#                     inner_join(by = c('lat', 'lon', 'month'),
#                               df_dCFC_MOD05_FOR %>% rename(MOD05 = dCFC) %>%
#                                 mutate(lat = round(lat, digits = 5),
#                                        lon = round(lon, digits = 5)),
#                               df_dCFC_MODo2_FOR %>% rename(MODo2 = dCFC) %>%
#                                 mutate(lat = round(lat, digits = 5),
#                                        lon = round(lon, digits = 5))),
#                     df_dCFC_MOD02_FOR %>% rename(MOD02 = dCFC) %>%
#                       mutate(lat = round(lat, digits = 5),
#                              lon = round(lon, digits = 5)))
# 
# 
# g.scat <- ggplot(df_all) + 
#   geom_point(aes(x = MOD05, y = MOD02)) +
#   # geom_bin2d(aes(x = `0.05 dd and 7 pix`, y = `0.02 dd and 17 pix`)) +
#   facet_wrap(~month, nc = 3) 
# 
# ggsave(filename = paste0('figSM___ProductComp-Europe-FOR-', iMonth, '.', fig.fmt),
#        width = 6, height = 7, path = fig.path)
# 
# 
# 
