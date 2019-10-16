require(ggplot2)

load('dataFigures/df_multiDeltaDF.Rda') # df_all

clr.Lims <- c(-0.08, 0.08)
pts.size <- 0.4

# HG vs LE in axis...
ggplot(df_all) +
  geom_point(aes(x = delta_LE, y = delta_HG, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = clr.Lims, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)


# HG vs LE in modified axis...
ggplot(df_all ) +
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
