require(ggplot2)
require(dplyr)
require(grid)


load('dataFigures/df_multiDeltaDF.Rda') # df_all

seasons <- factor(x = c(rep('DJF',2), rep('MAM',3), 
                        rep('JJA',3), rep('SON',3), 'DJF'), 
                  levels = c('DJF','MAM','JJA','SON'), 
                  labels = c('December to February (DJF)',
                             'March to May (MAM)',
                             'June to August (JJA)',
                             'September to November (SON)'))

df.seasonal <- data.frame(month = factor(month.abb, levels = month.abb),
                          season = seasons)


df_all <- df_all %>% left_join(df.seasonal, by = "month")


col.cross <- 'grey30'
clr.Lims <- c(-0.05, 0.05)
pts.size <- 0.4



# ggplot(df_all %>%
#          filter(delta_albedo >= -0.1)) +
#   geom_point(aes(x = delta_LE, y = delta_HG, colour = dCFC), size = pts.size) +
#   geom_hline(yintercept = 0, color = col.cross) + 
#   geom_vline(xintercept = 0, color = col.cross) + 
#   scale_colour_gradientn('Change in cloud cover fraction following afforestation', 
#                          colours = RColorBrewer::brewer.pal(9,'RdBu'),
#                          limits = clr.Lims, oob = scales::squish) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2.4, "cm")) +
#   guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))


LE.Lims <- c(-20,60)
HG.Lims <- c(-40,40)
Rn.Lims <- c(-30,50)

n = 15

g_LEvsHG <- ggplot(df_all %>%
         #filter(season == 'June to August (JJA)')) +
         filter(delta_albedo >= -0.1)) +
  stat_summary_2d(aes(x = delta_LE, y = delta_HG, z = dCFC), binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  coord_cartesian(xlim = LE.Lims, ylim = HG.Lims) + 
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))





g_LEvsRn <- ggplot(df_all %>%
         filter(delta_albedo >= -0.1)) +
  stat_summary_2d(aes(x = delta_LE, y = delta_Rn, z = dCFC), 
                  binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  coord_cartesian(xlim = LE.Lims, ylim = Rn.Lims) + 
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


g_HGvsRn <- ggplot(df_all %>%
         filter(delta_albedo >= -0.1)) +
  stat_summary_2d(aes(x = delta_HG, y = delta_Rn, z = dCFC), 
                  binwidth = 2, drop = T,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  coord_cartesian(xlim = HG.Lims, ylim = Rn.Lims) + 
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




# 
# ggsave(filename = paste0('fig___multiple-delta-plot', '.', fig.fmt), 
#        plot = g.xdelta, width = 9, height = 6, path = fig.path)

# printing the final plot -----
fig.name <- 'fig___multiple-delta-plot'
fig.width <- 12; fig.height <- 6;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g_LEvsHG, vp = viewport(width = 0.33, height = 1.0, x = 0.00, y = 0.0, just = c(0,0)))
print(g_LEvsRn, vp = viewport(width = 0.33, height = 1.0, x = 0.33, y = 0.0, just = c(0,0)))
print(g_HGvsRn, vp = viewport(width = 0.33, height = 1.0, x = 0.66, y = 0.0, just = c(0,0)))
dev.off()



# 
# my_breaks <- c(0,1,10,100,1000,10000,100000)
# 
# ggplot(df_all %>%
#          filter(delta_albedo >= -0.1)) +
#   stat_summary_2d(aes(x = delta_LE, y = delta_HG, z = dCFC), binwidth = 2,
#                   fun = function(z){length(z)}) +
#   geom_hline(yintercept = 0, color = col.cross) + 
#   geom_vline(xintercept = 0, color = col.cross) + 
#   scale_fill_viridis_c('Number of samples', trans = "log",
#                        breaks = my_breaks, labels = my_breaks) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2.4, "cm")) +
#   guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
# 
# 
# 
# ggplot(df_all %>%
#          filter(delta_albedo < -0.1)) +
#   stat_summary_2d(aes(x = delta_Rn, y = delta_LSTday, z = dCFC), 
#                   binwidth = c(5, 0.5),
#                   fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
#   geom_hline(yintercept = 0, color = col.cross) + 
#   geom_vline(xintercept = 0, color = col.cross) + 
#   scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
#                        colours = RColorBrewer::brewer.pal(9,'RdBu'),
#                        limits = clr.Lims * 1.5, oob = scales::squish) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2.4, "cm")) +
#   guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
# 
# n = 0
# 
# ggplot(df_all %>%
#          filter(delta_albedo < -0.1)) +
#   stat_summary_2d(aes(x = delta_albedo, y = delta_Rn, z = dCFC), 
#                   binwidth = c(0.01, 2),
#                   fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
#   geom_hline(yintercept = 0, color = col.cross) + 
#   geom_vline(xintercept = 0, color = col.cross) + 
#   scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
#                        colours = RColorBrewer::brewer.pal(9,'RdBu'),
#                        limits = clr.Lims*2, oob = scales::squish) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2.4, "cm")) +
#   guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
# 
# 
# 
# 
# 
# ggplot(df_all %>%
#          filter(delta_albedo < -0.1)) +
#   stat_summary_2d(aes(x = delta_albedo, y = delta_Rn, z = dCFC), 
#                   binwidth = c(0.01, 2),
#                   fun = function(z){length(z)}) +
#   geom_hline(yintercept = 0, color = col.cross) + 
#   geom_vline(xintercept = 0, color = col.cross) + 
#   scale_fill_viridis_c('Number of samples', trans = "log",
#                        breaks = my_breaks, labels = my_breaks) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2.4, "cm")) +
#   guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
# 
# 
