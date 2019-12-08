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
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  facet_wrap(~lbl.long, nc = 2) +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  ggtitle(paste('Evaluation of the scale effect for', 
                month.name[which(month.abb == iMonth)], 'over Europe')) +
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

g.time <- ggplot(df_sum) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  geom_errorbar(aes(x = month, 
                    ymin = pctl25, ymax = pctl75, 
                    colour = lbl.short), 
                position = position_dodge(width = 0.5)) +
  geom_point(aes(x = month, y = median, colour = lbl.short), 
             fill = 'white', shape = 21, size = 2,
             position = position_dodge(width = 0.5)) + 
  scale_colour_discrete(labels = c('a'='Pixel size 0.05dd, 7 pixel window',
                                   'c'='Pixel size 0.02dd, 17 pixel window',
                                   'd'='Pixel size 0.02dd, 7 pixel window')) + 
  scale_y_continuous('Change in cloud fraction cover') +
  scale_x_discrete('') + 
  ggtitle('Evaluation of the scale effect over Europe across time') + 
  theme(legend.position = c(0.95,0.05),
        legend.justification = c(1,0),
        legend.title = element_blank())

df_sumplus <- df_sum %>%
  pivot_longer(cols = c('median', 'pctl25', 'pctl75'),
               names_to = 'stat', values_to = 'value') %>%
  pivot_wider(id_cols = c('lbl.short', 'month', 'stat'), 
              names_from = lbl.short, 
              values_from = 'value') %>%
  rename(ref.case = a,
         win.size.effect = c,
         pix.size.effect = d) %>%
  pivot_longer(cols = c('win.size.effect', 'pix.size.effect'),
               names_to = "cases", values_to = 'dCFC') %>%
  pivot_wider(id_cols = c('month', 'stat', 'cases'),
              names_from = 'stat', values_from = c('dCFC', 'ref.case'))

labeller_explicit <- labeller(
  cases = c(win.size.effect = 'Effect of window size\n[Pixel size 0.02dd, 17 pixel window]',
            pix.size.effect = 'Effect of resolution\n[Pixel size 0.02dd, 7 pixel window]'))

g.scat <- ggplot(df_sumplus) +
  geom_abline(colour = 'grey30') +
  geom_errorbar(aes(x = ref.case_median, ymin = dCFC_pctl25, ymax = dCFC_pctl75, 
                    colour = month)) +
  geom_errorbarh(aes(y = dCFC_median, xmin = ref.case_pctl25, 
                     xmax = ref.case_pctl75, colour = month)) +
  geom_point(aes(x = ref.case_median, y = dCFC_median, 
                 colour = month), fill = 'white', shape = 21, size = 2) +
  facet_wrap(~cases, nc = 2, labeller = labeller_explicit) + 
  scale_x_continuous('Original [Pixel size 0.05dd, 7 pixel window]') + 
  scale_y_continuous('Change in cloud fraction cover') +
  scale_colour_viridis_d('') +
  coord_equal(ylim = c(-0.08, 0.04), xlim = c(-0.08, 0.04)) +
  theme(legend.position = 'bottom',
        strip.text = element_text(rel(1.2))) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))



# printing the final plot -----
fig.name <- 'figSM___scale-effect'
fig.width <- 6; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g.time, vp = viewport(width = 1, height = 0.5, x = 0.0, y = 0.5, just = c(0,0)))
print(g.scat, vp = viewport(width = 1, height = 0.5, x = 0.0, y = 0.0, just = c(0,0)))
dev.off()




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
