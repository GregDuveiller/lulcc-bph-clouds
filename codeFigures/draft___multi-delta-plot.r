### clouds...
# try to see delta_cfc in the space between delta_LE and delta_H from NCOMM


require(ncdf4)
require(dplyr)
require(raster)
require(ggplot2)



# input delta_LE and delta_H

dpath1 <- '/ESS_Datasets/USERS/Duveiller/Workspace/BiophysLUCeffectsfromRS/dataCuration'

nc_lvar <- 4 # to specify that we use the 4th dimension (iTr) to select what to put in the raster bricks
nc_level <- 2 # to specify the level of the variable nc_lvar to select, in this case the second transition

nc_LE <- nc_open(filename = paste0(dpath1, '/LE_IGBPgen.nc'))


rs_LE_delta <- brick(x = paste0(dpath1, '/LE_IGBPgen.nc'),
                     varname = 'Delta_LE', lvar = nc_lvar, level = nc_level)
rs_LE_sigma <- brick(x = paste0(dpath1, '/LE_IGBPgen.nc'),
                     varname = 'SD_Delta_LE', lvar = nc_lvar, level = nc_level)

df_LE_delta <- as.data.frame(rs_LE_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LE = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LE))


rs_HG_delta <- brick(x = paste0(dpath1, '/HG_IGBPgen.nc'),
                     varname = 'Delta_HG', lvar = nc_lvar, level = nc_level)
rs_HG_sigma <- brick(x = paste0(dpath1, '/HG_IGBPgen.nc'),
                     varname = 'SD_Delta_HG', lvar = nc_lvar, level = nc_level)

df_HG_delta <- as.data.frame(rs_HG_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>% 
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_HG = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_HG))


rs_SW_delta <- brick(x = paste0(dpath1, '/SWreflected_IGBPgen.nc'),
                     varname = 'Delta_SWreflected', lvar = nc_lvar, level = nc_level)
rs_SW_sigma <- brick(x = paste0(dpath1, '/SWreflected_IGBPgen.nc'),
                     varname = 'SD_Delta_SWreflected', lvar = nc_lvar, level = nc_level)

df_SW_delta <- as.data.frame(rs_SW_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_SW = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_SW))


rs_albedo_delta <- brick(x = paste0(dpath1, '/albedo_IGBPgen.nc'),
                     varname = 'Delta_albedo', lvar = nc_lvar, level = nc_level)
rs_albedo_sigma <- brick(x = paste0(dpath1, '/albedo_IGBPgen.nc'),
                     varname = 'SD_Delta_albedo', lvar = nc_lvar, level = nc_level)

df_albedo_delta <- as.data.frame(rs_albedo_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_albedo = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_albedo))



rs_LSTday_delta <- brick(x = paste0(dpath1, '/LSTday_IGBPgen.nc'),
                         varname = 'Delta_LSTday', lvar = nc_lvar, level = nc_level)
rs_LSTday_sigma <- brick(x = paste0(dpath1, '/LSTday_IGBPgen.nc'),
                         varname = 'SD_Delta_LSTday', lvar = nc_lvar, level = nc_level)

df_LSTday_delta <- as.data.frame(rs_LSTday_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTday = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTday))


rs_LSTnight_delta <- brick(x = paste0(dpath1, '/LSTnight_IGBPgen.nc'),
                         varname = 'Delta_LSTnight', lvar = nc_lvar, level = nc_level)
rs_LSTnight_sigma <- brick(x = paste0(dpath1, '/LSTnight_IGBPgen.nc'),
                         varname = 'SD_Delta_LSTnight', lvar = nc_lvar, level = nc_level)

df_LSTnight_delta <- as.data.frame(rs_LSTnight_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(month = factor(month.abb[Z], levels = month.abb),
                delta_LSTnight = -value) %>% # Note change in sign to flip transition
  dplyr::select(-Z, -value) %>%
  dplyr::filter(!is.na(delta_LSTnight))



# need to clean this up and load cfc dataframe from scratch
load('dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata') # df_dCFC_MOD05_FOR_1dd
df_CFC_delta <- df_dCFC_MOD05_FOR_1dd


df_all <- df_CFC_delta %>%
  inner_join(df_LE_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_HG_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_SW_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_LSTday_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_LSTnight_delta, by = c('lon', 'lat', 'month')) %>%
  inner_join(df_albedo_delta, by = c('lon', 'lat', 'month'))



#### 1-D relationships with cloud cover ####

col.cross <- 'grey40'

ggplot(df_all) +
       #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_HG, seq(-40,40,10)), y = delta_cfc)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))

ggplot(df_all) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_LSTnight, seq(-4,4,1)), y = delta_cfc)) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  #stat_smooth(method = 'lm') + 
  facet_wrap(~month) +
  coord_cartesian(ylim = c(-0.2, 0.2))

ggplot(df_all) +
  #geom_point(alpha = 0.5, colour = 'cornflowerblue') +
  geom_boxplot(aes(x = cut(delta_albedo, seq(-0.2,0.2,0.02)), y = delta_cfc)) +
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
                   y = delta_cfc, fill = variable), outlier.shape = NA) +
  geom_hline(yintercept = 0, colour = col.cross) +
  geom_vline(xintercept = 0, colour = col.cross) +
  facet_wrap(~month, nc = 2) +
  coord_cartesian(ylim = c(-0.2, 0.2))


ggplot(df_try %>% 
         filter(variable == 'delta_HG') %>%
         filter(month %in% c('May','Jun','Jul','Aug','Sep','Oct')) #%>% 
         #filter(lat > 35, lat < 65, lon > -10, lon < 40)
       ) +
  geom_quantile(aes(x = value, y = delta_cfc), quantiles = c(0.05,0.25,0.5,0.75,0.95)) +
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
  geom_point(aes(x = delta_LE, y = delta_HG, color = delta_cfc), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_wrap(~month, nr = length(sel.months), scales = "free_y") +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) +
  theme(legend.position = 'none') 


g2 <- ggplot(df_all %>% filter(month %in% sel.months)) +
  geom_point(aes(x = delta_LE, y = delta_SW, color = delta_cfc), size = pts.size) +
  geom_hline(yintercept = 0, color = col.cross) + 
  geom_vline(xintercept = 0, color = col.cross) + 
  facet_wrap(~month, nr = length(sel.months), scales = "free_y") +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                        limits = lim.colors, oob = scales::squish) +
  theme(legend.position = 'none') 

g3 <- ggplot(df_all %>% filter(month %in% sel.months)) +
  geom_point(aes(x = delta_HG, y = delta_SW, color = delta_cfc), size = pts.size) +
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
