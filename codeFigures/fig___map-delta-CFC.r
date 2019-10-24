# Maps of global CFC

# Still to do...
# - adjust font sizes if needed



require(dplyr)
require(tidyr)
require(grid)
require(ggplot2)
require(raster)
require(ncdf4)
require(sf)

require(here)



world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)


# Load data...
load('dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata') # df_dCFC_MOD05_FOR_1dd.Rdata



col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'
latLims <- c(-56,86)
ylims <- c(-0.07, 0.07)

mk.zone <- function(lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = lbl, 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}

zn.eur <- mk.zone('West/Central Europe',-10,20,38,62)
zn.nam <- mk.zone('North America',-120,-60,40,60)
zn.crn <- mk.zone('US corn belt',-95,-82,36,44)
zn.ind <- mk.zone('Indian subcontinent',65,90,5,30)
zn.aus <- mk.zone('Eastern Australia',140,155,-45,-18)
zn.rus <- mk.zone('Russia/East Europe',20,110,45,65)
zn.ama <- mk.zone('Southern Amazon',-70,-45,-15,-5)
zn.afr <- mk.zone('Southern Africa',10,42,-30,-5)
zn.chi <- mk.zone('Eastern China',105,125,20,37)

zn <- bind_rows(zn.nam, zn.crn, zn.eur, zn.rus, zn.ama, zn.afr, zn.ind, zn.aus, zn.chi)

mk.tmp.plot <- function(zn.dum, mon = NULL, ylims = NULL){
  
  df.dum  <- df_dCFC_MOD05_FOR_1dd %>%
    filter(lat > min(zn.dum$lat), lat < max(zn.dum$lat), 
           lon > min(zn.dum$lon), lon < max(zn.dum$lon)) %>%
    group_by(month) %>%
    summarize(mean_dCFC = mean(dCFC),
              stdE_dCFC = sd(dCFC)/sqrt(length(dCFC))) %>%
    mutate(sign = factor(sign(mean_dCFC), levels = c(-1,0,1) )) 
  
  if(!is.null(mon)){ df.dum$sign[df.dum$month == mon] <- 0 }
  
  g.tmp <- ggplot(df.dum) + 
    geom_bar(aes(x = month, y = mean_dCFC, fill = sign, colour = sign), stat = 'identity') +
    geom_errorbar(aes(x = month, colour = sign,
                      ymin = mean_dCFC - stdE_dCFC,
                      ymax = mean_dCFC + stdE_dCFC))+
    geom_hline(yintercept = 0) +
    scale_y_continuous('Change in CFC') + 
    scale_x_discrete('') +
    scale_fill_manual(values = c('-1' = col.pal[2], '1' = col.pal[8], '0' = 'Grey30')) +
    scale_colour_manual(values = c('-1'= col.pal[1], '1' = col.pal[9], '0' = 'Grey20')) +
    coord_cartesian(ylim = ylims) +
    theme_minimal()+
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_blank()) + 
    ggtitle(unique(zn.dum$lbl))
  
}







# Maps of 4 seasons

seasons <- factor(x = c(rep('DJF',2), rep('MAM',3), 
                        rep('JJA',3), rep('SON',3), 'DJF'), 
                  levels = c('DJF','MAM','JJA','SON'), 
                  labels = c('December to February (DJF)',
                             'March to May (MAM)',
                             'June to August (JJA)',
                             'September to November (SON)'))

df.seasonal <- data.frame(month = month.abb, season = seasons)

g.map.seasonal <- ggplot(df_dCFC_MOD05_FOR_1dd %>% 
                       left_join(df.seasonal, by = 'month') %>%
                       group_by(lat, lon, season) %>%
                       summarise(dCFC_seas = mean(dCFC, na.rm = T))) +
  geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = dCFC_seas)) +
  geom_path(data = zn, aes(group = lbl, x = lon, y = lat), color = 'white') +
  facet_wrap(~season, nc = 1) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = ylims, oob = scales::squish) +
  coord_sf(expand = F, ylim = latLims)+
  ggtitle('Seasonal patterns of cloud change cover (CFC)\nresulting from afforestation') + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'none',
        legend.key.width = unit(2.4, "cm"),
        panel.grid = element_line(color = seaColor),
        axis.text = element_text(size = rel(1.1)),
        axis.title = element_blank(),
        title = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.2))) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# Lat-Month summary plot (with legend)

# # Alt version with bquotes and all, but DOES NOT WORK
# geo_labeller <- function(x) {
#   lbls <- c('1' = 'N', '-1' = 'S', '0' = '')
#   lbl <- bquote(.(abs(x))*degree*lbls[as.character(sign(x))])
#   return(lbl)
# }

# works, but could have problem if fonts not availables properly
geo_labeller <- function(x) {
  lbls <- c('1' = '°N', '-1' = '°S', '0' = '°')
  lbl <- paste0(abs(x), lbls[as.character(sign(x))])
  return(lbl)
}

g.lat.month <- ggplot(df_dCFC_MOD05_FOR_1dd %>% 
                        group_by(lat, month) %>%
                        summarise(dCFC_latmonth = mean(dCFC, na.rm = T))) +
  geom_raster(aes(x = month, y = lat, fill = dCFC_latmonth)) +
  scale_fill_gradientn('Change in cloud fraction cover (CFC)',
                       colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = ylims, oob = scales::squish) +
    scale_y_continuous(labels = geo_labeller) + 
  theme(panel.background = element_rect(fill = seaColor),
        legend.position = 'top',
        legend.key.width = unit(2.4, "cm"),
        axis.text = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.2)),
        panel.grid = element_line(color = seaColor),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



# sub-plots of bar graphs for selected zones
g.eur <- mk.tmp.plot(zn.eur, mon = NULL, ylims = ylims)
g.nam <- mk.tmp.plot(zn.nam, mon = NULL, ylims = ylims)
g.ind <- mk.tmp.plot(zn.ind, mon = NULL, ylims = ylims)
g.aus <- mk.tmp.plot(zn.aus, mon = NULL, ylims = ylims)
g.rus <- mk.tmp.plot(zn.rus, mon = NULL, ylims = c(-0.10, 0.04))
g.ama <- mk.tmp.plot(zn.ama, mon = NULL, ylims = ylims)
g.afr <- mk.tmp.plot(zn.afr, mon = NULL, ylims = ylims)
g.crn <- mk.tmp.plot(zn.crn, mon = NULL, ylims = ylims)
g.chi <- mk.tmp.plot(zn.chi, mon = NULL, ylims = c(-0.05, 0.09))





# printing the plot
fig.name <- 'fig___map-delta-CFC'
fig.width <- 14; fig.height <- 13; fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

hm <- 1.0; wm <- 0.5
hs <- 0.45; ws <- 1 - wm
w <- ws/3; h <- (1 - hs - 0.02)/3; s <- wm

print(g.map.seasonal, vp = viewport(width = wm, height = hm, x = 0, y = 0, just = c(0,0)))
print(g.lat.month,    vp = viewport(width = ws, height = hs, x = wm, y = 1 - hs, just = c(0,0)))

print(g.ama, vp = viewport(width = w, height = h, y = 0 * h, x = s + 0 * w, just = c(0,0)))
print(g.crn, vp = viewport(width = w, height = h, y = 1 * h, x = s + 0 * w, just = c(0,0)))
print(g.nam, vp = viewport(width = w, height = h, y = 2 * h, x = s + 0 * w, just = c(0,0)))
print(g.afr, vp = viewport(width = w, height = h, y = 0 * h, x = s + 1 * w, just = c(0,0)))
print(g.ind, vp = viewport(width = w, height = h, y = 1 * h, x = s + 1 * w, just = c(0,0)))
print(g.eur, vp = viewport(width = w, height = h, y = 2 * h, x = s + 1 * w, just = c(0,0)))
print(g.aus, vp = viewport(width = w, height = h, y = 0 * h, x = s + 2 * w, just = c(0,0)))
print(g.chi, vp = viewport(width = w, height = h, y = 1 * h, x = s + 2 * w, just = c(0,0)))
print(g.rus, vp = viewport(width = w, height = h, y = 2 * h, x = s + 2 * w, just = c(0,0)))

dev.off()


# ## ALTERNATIVE VERSION> >>> not ready
# 
# ncp <- 1/4
# fig.name <- 'fig___map-delta-CFC'
# fig.width <- 14; fig.height <- 16; fig.fmt <- 'png'
# fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
# if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
# if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
# print(g.map.seasonal, vp = viewport(width = ncp*2, height = 0.8, x = 0.25, y = 0.2, just = c(0,0)))
# print(g.lat.month,    vp = viewport(width = ncp*2, height = 0.2, x = 0.25, y = 0.0, just = c(0,0)))
# 
# 
# print(g.nam, vp = viewport(width = ncp, height = 0.25, y = 0*ncp, x = 0.75, just = c(0,0)))
# print(g.crn, vp = viewport(width = ncp, height = 0.25, y = 1*ncp, x = 0.75, just = c(0,0)))
# print(g.eur, vp = viewport(width = ncp, height = 0.25, y = 2*ncp, x = 0.75, just = c(0,0)))
# print(g.rus, vp = viewport(width = ncp, height = 0.25, y = 3*ncp, x = 0.75, just = c(0,0)))
# print(g.ama, vp = viewport(width = ncp, height = 0.25, y = 0*ncp, x = 0.00, just = c(0,0)))
# print(g.afr, vp = viewport(width = ncp, height = 0.25, y = 1*ncp, x = 0.00, just = c(0,0)))
# print(g.ind, vp = viewport(width = ncp, height = 0.25, y = 2*ncp, x = 0.00, just = c(0,0)))
# print(g.aus, vp = viewport(width = ncp, height = 0.25, y = 3*ncp, x = 0.00, just = c(0,0)))
# 
# dev.off()
# 
# 
