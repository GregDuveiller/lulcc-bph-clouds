### clouds...
# try to see dCFC in the space between delta_LE and delta_H from NCOMM


require(ncdf4)
require(dplyr)
require(raster)
require(ggplot2)





#### 1-D relationships with cloud cover ####

col.cross <- 'grey40'

ggplot(df_all) +
       #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_HG, seq(-40,40,10)), y = dCFC)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))

ggplot(df_all) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_LSTnight, seq(-4,4,1)), y = dCFC)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))

ggplot(df_all) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_albedo, seq(-0.2,0.2,0.02)), y = dCFC)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))




ggplot(df_all) +
  geom_point(aes(x = delta_LE - delta_HG, y = delta_HG + delta_LE, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) + 
  facet_wrap(~region, nc = 2)


df_all_cat <- df_all %>%
  group_by(region) %>%
  mutate(cat_LE_HG = cut(delta_LE - delta_HG, seq(-50,50,5))) 


ggplot(df_all_cat) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cat_LE_HG, y = dCFC)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~region, nc = 2) +
  coord_cartesian(ylim = c(-0.2, 0.2))




ggplot(df_all) +
 # geom_point(aes(x = delta_HG, y = dCFC), alpha = 0.5, colour = 'cornflowerblue') +
  geom_bin2d(aes(x = delta_HG, y = dCFC)) + 
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  scale_fill_viridis_c(limits = c(10,250), oob = scales::squish) +
  #stat_smooth(method = 'lm') + 
  #facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))




ggplot(df_all) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_HG, seq(-40,40,10)), y = dCFC)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))






df_try <- df_all %>%
  tidyr::gather(key = 'variable', value = 'value', 
                c('delta_LE', 'delta_HG'))
                #c('delta_LE', 'delta_HG', 'delta_SW', 'delta_LSTday', 'delta_LSTnight', 'delta_albedo'))

ggplot(df_try %>% 
         filter(month %in% c('May','Jun','Jul','Aug','Sep','Oct')) #%>% 
       #filter(lat > 35, lat < 65, lon > -10, lon < 40)
) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(value, breaks = c(-100,-25,-20,-15,-10,-5,5,10,15,20,25,100)),
                   y = dCFC, fill = variable), outlier.shape = NA) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  facet_wrap(~month, nc = 2) +
  coord_cartesian(ylim = c(-0.2, 0.2))


ggplot(df_try %>% 
         filter(variable == 'delta_HG') %>%
         filter(month %in% c('May','Jun','Jul','Aug','Sep','Oct')) #%>% 
         #filter(lat > 35, lat < 65, lon > -10, lon < 40)
       ) +
  geom_quantile(aes(x = value, y = dCFC), quantiles = c(0.05,0.25,0.5,0.75,0.95)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month, nc = 2) +
  coord_cartesian(ylim = c(-0.2, 0.2))



#### 2-D spaces (to be expanded) ####

col.cross <- 'grey40'
sel.months <- c('Sep','Jul')
lim.colors <- c(-0.12,0.12)
pts.size <- 0.5

g1 <- ggplot(df_all %>% filter(month %in% sel.months)) +
  geom_point(aes(x = delta_LE, y = delta_HG, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_wrap(~month, nr = length(sel.months), scales = "free_y") +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) +
  theme(legend.position = 'none') 


g2 <- ggplot(df_all %>% filter(month %in% sel.months)) +
  geom_point(aes(x = delta_LE, y = delta_SW, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_wrap(~month, nr = length(sel.months), scales = "free_y") +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) +
  theme(legend.position = 'none') 

g3 <- ggplot(df_all %>% filter(month %in% sel.months)) +
  geom_point(aes(x = delta_HG, y = delta_SW, color = dCFC), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_wrap(~month, nr = length(sel.months), scales = "free_y") +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) 


require(grid)

fname <- 'FigTest_drivers'
figW <- 10; figH <- 3*length(sel.months); fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g1, vp = viewport(width = 0.3, height = 1, x = 0.0, y = 0, just = c(0,0)))
print(g2, vp = viewport(width = 0.3, height = 1, x = 0.3, y = 0, just = c(0,0)))
print(g3, vp = viewport(width = 0.4, height = 1, x = 0.6, y = 0, just = c(0,0)))

dev.off()


####
