require(ggplot2)
require(dplyr)
require(grid)


load('dataFigures/df_multiDeltaDF.Rda') # df_all

col.pal <- RColorBrewer::brewer.pal(9,'RdBu')
col.pal <- rev(c('#2B3677', '#327FBB', '#A2D5FF', '#F7F7F7', '#FFD181' ,'#EA965A', '#9C4D0C'))

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
pts.size <- 0.4


LE.Lims <- c(-20,55)
HG.Lims <- c(-40,35)
Rn.Lims <- c(-25,50)

LE.axis.title <- bquote('Change in latent heat ('~Delta~LE~') [' ~ Wm^-2 ~ ']')
HG.axis.title <- bquote('Change in sensible and ground heat ('~Delta~(H+G)~') [' ~ Wm^-2 ~ ']') 
Rn.axis.title <- bquote('Change in net radiation ('~Delta~R[n]~') [' ~ Wm^-2 ~ ']')
  
n = 15
ALB_thr <- -0.15

g_LEvsRn_LAC <- ggplot(df_all %>%
                     filter(delta_albedo >= ALB_thr)) +
  stat_summary_2d(aes(x = delta_LE, y = delta_Rn, z = dCFC), 
                  binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud fractional cover following afforestation', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_cartesian(xlim = LE.Lims, ylim = Rn.Lims) + 
  xlab(LE.axis.title) + 
  ylab(Rn.axis.title) +
  labs(title = bquote(Delta~alpha>=.(ALB_thr))) + 
  # labs(title = 'Effect of afforestation on cloud cover',
  #      subtitle = 'Selection for bins with n > 15 and delta_albedo > -0.1') +
  theme(legend.position = 'none',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



g_HGvsRn_LAC <- ggplot(df_all %>%
                     filter(delta_albedo >= ALB_thr)) +
  stat_summary_2d(aes(x = delta_HG, y = delta_Rn, z = dCFC), 
                  binwidth = 2, drop = T,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud fractional cover following afforestation', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_cartesian(xlim = HG.Lims, ylim = Rn.Lims) + 
  xlab(HG.axis.title) + 
  ylab(Rn.axis.title) +
  labs(title = bquote(Delta~alpha>=.(ALB_thr))) + 
  theme(legend.position = 'none',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

g_LEvsHG_LAC <- ggplot(df_all %>%
                     filter(delta_albedo >= ALB_thr)) +
  stat_summary_2d(aes(y = delta_LE, x = delta_HG, z = dCFC), binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud fractional cover following afforestation', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_cartesian(ylim = LE.Lims, xlim = HG.Lims) + 
  ylab(LE.axis.title) + 
  xlab(HG.axis.title) +
  labs(title = bquote(Delta~alpha>=.(ALB_thr))) + 
  theme(legend.position = 'none') +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

g_LEvsHG_HAC <- ggplot(df_all %>%
                     filter(delta_albedo < ALB_thr)) +
  stat_summary_2d(aes(y = delta_LE, x = delta_HG, z = dCFC), binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
 # geom_text(label = bquote(delta~"albedo"~<.(ALB_thr)), x = min(HG.Lims+25),
 #           y = min(LE.Lims)) +
  scale_fill_gradientn('Change in cloud fractional cover following afforestation', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_cartesian(ylim = LE.Lims, xlim = LE.Lims) + 
  ylab(LE.axis.title) + 
  xlab(HG.axis.title) +
  labs(title = bquote(Delta~alpha<.(ALB_thr))) + 
  theme(legend.position = c(0.5,0.85),
        legend.background = element_rect(colour = 'grey40', fill = 'white', linetype='solid'),
        legend.direction = "horizontal", 
        legend.key.width = unit(1.8, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                frame.colour = 'black',
                                ticks.colour = 'black'))



# printing the final plot -----
fig.name <- 'fig___multiple-delta-plot'
fig.width <- 9; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g_LEvsRn_LAC, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just = c(0,0)))
print(g_HGvsRn_LAC, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just = c(0,0)))
print(g_LEvsHG_HAC, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just = c(0,0)))
print(g_LEvsHG_LAC, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just = c(0,0)))


grid.text(expression(bold("a")), x = unit(0.03, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.53, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("c")), x = unit(0.03, "npc"), y = unit(0.46, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("d")), x = unit(0.53, "npc"), y = unit(0.46, "npc"), gp = gpar(fontsize = 18))


dev.off()



#### ---- 


# 

#   my_breaks <- c(0,1,10,100,1000,10000,100000)
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
#                        limits = dcfcLims * 1.5, oob = scales::squish) +
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
#                        limits = dcfcLims*2, oob = scales::squish) +
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
