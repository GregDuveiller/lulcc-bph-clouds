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
clr.Lims <- c(-0.08, 0.08)
pts.size <- 0.4

# HG vs LE 
ggplot(df_all) +
  geom_point(aes(x = delta_LE, y = delta_HG, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims*1.5, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)


n = 5

ggplot(df_all %>%
         filter(delta_albedo >= -0.1)) +
  stat_summary_2d(aes(x = delta_LE, y = delta_HG, z = dCFC), binwidth = 5,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_grid(PFT ~ season) +
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
                        colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims, oob = scales::squish) +
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


ggplot(df_all %>%
         filter(delta_albedo < -0.1)) +
  stat_summary_2d(aes(x = delta_Rn, y = delta_LSTday, z = dCFC), 
                  binwidth = c(5, 0.5),
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_grid(PFT ~ season) +
  scale_fill_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))






# LSTday vs LSTnight
ggplot(df_all) +
  geom_point(aes(x = delta_LSTday, y = delta_LSTnight, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims*1.5, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)


# alb vs SW
ggplot(df_all) +
  geom_point(aes(x = delta_Rn, y = delta_albedo, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims*1.5, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)





# HG vs LE ... 'without snow'...
ggplot(df_all %>% 
         filter(delta_albedo >= -0.1)) +
  geom_point(aes(x = delta_LE, y = delta_HG, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims*1.5, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)
#facet_grid(month~region)

# Rn vs T ... 'on snow'
ggplot(df_all %>% 
         filter(delta_albedo < -0.1)) +
  geom_point(aes(x = delta_LE + delta_HG, y = delta_LSTday, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims*2, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)




# HG vs LE in modified axis...
ggplot(df_all %>% 
         filter(delta_albedo >= -0.1)) +
  geom_point(aes(x = delta_LE - delta_HG, y = delta_HG + delta_LE, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)




# HG vs LE in modified axis and extra var...
g.xdelta <- ggplot(df_all ) +
  geom_point(aes(x = delta_LE - delta_HG, y = delta_LSTday, color = dCFC), 
             size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_grid(PFT~region) + 
  scale_color_gradientn('Change in cloud cover fraction\nfollowing afforestation of different forest types', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(color = guide_colourbar(title.position = "top", title.hjust = 0.5))


ggsave(filename = paste0('fig___multiple-delta-plot', '.', fig.fmt), 
       plot = g.xdelta, width = 9, height = 6, path = fig.path)
