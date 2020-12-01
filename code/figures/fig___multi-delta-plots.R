#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure "multi-delta-plots"
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(ggplot2)
require(dplyr)
require(grid)


# preparing the data for the plots ----

# load the necessary data
load(paste0(dat4fig_path, "/df_multiDeltaDF.Rda")) # <-- "df_all"

# set some specific graphical parameters 
col.cross <- 'grey30'
pts.size <- 0.4
n <- 15
ALB_thr <- -0.15

# setup labels for seasons in data frame
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

# setup axis ranges
LE.Lims <- c(-20,55)
HG.Lims <- c(-40,35)
Rn.Lims <- c(-25,50)

# setup titles
LE.axis.title <- bquote('Change in latent heat ('~Delta~LE~') [' ~ Wm^-2 ~ ']')
HG.axis.title <- bquote('Change in sensible and ground heat ('~Delta~(H+G)~') [' ~ Wm^-2 ~ ']') 
Rn.axis.title <- bquote('Change in net radiation ('~Delta~R[n]~') [' ~ Wm^-2 ~ ']')
  

# plot subpanel 1: LEvsRN ----
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
  theme(legend.position = 'none',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# plot subpanel 2: HGvsRN ----
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

# plot subpanel 3: LEvsHG ----
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

# plot subpanel 4: LEvsHG (high albedo) ----
g_LEvsHG_HAC <- ggplot(df_all %>%
                     filter(delta_albedo < ALB_thr)) +
  stat_summary_2d(aes(y = delta_LE, x = delta_HG, z = dCFC), binwidth = 2,
                  fun = function(z){ifelse(length(z) > n, median(z), NA)}) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_fill_gradientn('Change in cloud fractional cover\n following afforestation', 
                       colours = col.pal,
                       limits = dcfcLims, oob = scales::squish) +
  coord_cartesian(ylim = LE.Lims, xlim = LE.Lims) + 
  ylab(LE.axis.title) + 
  xlab(HG.axis.title) +
  labs(title = bquote(Delta~alpha<.(ALB_thr))) + 
  theme(legend.position = c(0.5,0.85),
        legend.background = element_rect(colour = 'grey40', fill = 'white', linetype = 'solid'),
        legend.margin = margin(2, 10, 2, 10, "mm"),
        legend.direction = "horizontal", 
        legend.key.width = unit(1.6, "cm")) +
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
print(g_LEvsHG_LAC, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just = c(0,0)))
print(g_LEvsHG_HAC, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just = c(0,0)))

grid.text(expression(bold("a")), x = unit(0.03, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.53, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("c")), x = unit(0.03, "npc"), y = unit(0.46, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("d")), x = unit(0.53, "npc"), y = unit(0.46, "npc"), gp = gpar(fontsize = 18))

dev.off()
