
require(ggrepel)
require(patchwork)



load('dataFigures/df_multiDeltaDF.Rda') # df_all

col.cross <- 'grey10'
col.bar <- 'grey60'
col.arrow <- 'grey20'

clr.Lims <- c(-0.05, 0.05)
pts.size <- 3


# LE.Lims <- c(-20,55)
# HG.Lims <- c(-40,35)
# Rn.Lims <- c(-25,50)

# LE.axis.title <- bquote('Change in latent heat ('~Delta~LE~') [' ~ Wm^-2 ~ ']')
# HG.axis.title <- bquote('Change in sensible and ground heat ('~Delta~(H+G)~') [' ~ Wm^-2 ~ ']') 
# Rn.axis.title <- bquote('Change in net radiation ('~Delta~R[n]~') [' ~ Wm^-2 ~ ']')

LE.axis.title <- bquote(Delta~LE~'[' ~ Wm^-2 ~ ']')
HG.axis.title <- bquote(Delta~(H+G)~'[' ~ Wm^-2 ~ ']') 
Rn.axis.title <- bquote(Delta~R[n]~'[' ~ Wm^-2 ~ ']')


# get ROIs
source('codeFigures/ancillary__definingROIs.r')



for(izn in unique(zn$uid)){
# plot.my.plot <- function(izn){
  
  zn.x <- zn %>% filter(uid == izn)


dfz <- df_all %>%
  filter(lon > min(zn.x$lon), lon < max(zn.x$lon),
         lat > min(zn.x$lat), lat < max(zn.x$lat)) %>%
  group_by(month) %>%
  summarize(dCFC_mu = mean(dCFC, na.rm = T),
            dHG_mu = mean(delta_HG, na.rm = T),
            dHG_sd = sd(delta_HG, na.rm = T),
            dLE_mu = mean(delta_LE, na.rm = T),
            dLE_sd = sd(delta_LE, na.rm = T),
            dRn_mu = mean(delta_Rn, na.rm = T),
            dRn_sd = sd(delta_Rn, na.rm = T))


g_LEvsRn <- ggplot(dfz, aes(x = dLE_mu, y = dRn_mu)) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  geom_errorbarh(aes(xmin = dLE_mu - dLE_sd, xmax = dLE_mu + dLE_sd), colour = col.bar) +
  geom_errorbar(aes(ymin = dRn_mu - dRn_sd, ymax = dRn_mu + dRn_sd), colour = col.bar) +
  geom_path(size = 1 , colour = col.arrow,
            arrow = arrow(angle = 30, length = unit(0.35, "cm"),
                          ends = "last", type = "open")) +
  geom_label_repel(aes(label = month), box.padding   = 0.85) + 
  geom_point(aes(fill = dCFC_mu), size = 3, shape = 21, colour = col.bar) +
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  # coord_cartesian(xlim = HG.Lims, ylim = Rn.Lims) + 
  xlab(LE.axis.title) + 
  ylab(Rn.axis.title) +
  labs(title = paste('Potential afforestation in',unique(zn.x$lbl))) + 
  theme(legend.position = 'none',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


g_HGvsRn <- ggplot(dfz, aes(x = dHG_mu, y = dRn_mu)) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  geom_errorbarh(aes(xmin = dHG_mu - dHG_sd, xmax = dHG_mu + dHG_sd), colour = col.bar) +
  geom_errorbar(aes(ymin = dRn_mu - dRn_sd, ymax = dRn_mu + dRn_sd), colour = col.bar) +
  geom_path(size = 1 , colour = col.arrow,
            arrow = arrow(angle = 30, length = unit(0.35, "cm"),
                          ends = "last", type = "open")) +
  geom_label_repel(aes(label = month), box.padding   = 0.85) + 
  geom_point(aes(fill = dCFC_mu), size = 3, shape = 21, colour = col.bar) +
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  # coord_cartesian(xlim = HG.Lims, ylim = Rn.Lims) + 
  xlab(HG.axis.title) + 
  ylab(Rn.axis.title) +
  #labs(title = bquote(Delta~alpha>=.(ALB_thr))) + 
  theme(legend.position = 'none',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




g_HGvsLE <- ggplot(dfz, aes(x = dHG_mu, y = dLE_mu)) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  geom_errorbarh(aes(xmin = dHG_mu - dHG_sd, xmax = dHG_mu + dHG_sd), colour = col.bar) +
  geom_errorbar(aes(ymin = dLE_mu - dLE_sd, ymax = dLE_mu + dLE_sd), colour = col.bar) +
  geom_path(size = 1 , colour = col.arrow,
            arrow = arrow(angle = 30, length = unit(0.35, "cm"),
                          ends = "last", type = "open")) +
  geom_label_repel(aes(label = month), box.padding   = 0.85) + 
  geom_point(aes(fill = dCFC_mu), size = 3, shape = 21, colour = col.bar) +
  scale_fill_gradientn('Change in cloud cover fraction following afforestation', 
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = clr.Lims, oob = scales::squish) +
  # coord_cartesian(xlim = HG.Lims, ylim = Rn.Lims) + 
  xlab(HG.axis.title) + 
  ylab(LE.axis.title) +
  #labs(title = bquote(Delta~alpha>=.(ALB_thr))) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.2, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



# printing the plot -----

dir.create(paste0(fig.path, '/xtras'), recursive = T, showWarnings = F)

fig.name <- paste0('figXtra___multiple-delta-plot___',unique(zn.x$uid))
fig.width <- 5; fig.height <- 15;  # fig.fmt <- 'png'
fig.fullfname <-  paste0(fig.path, '/xtras/', fig.name, '.', fig.fmt)
#fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g_LEvsRn / g_HGvsRn / g_HGvsLE) 

dev.off()

}
