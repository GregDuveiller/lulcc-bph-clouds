require(tidyr)
require(dplyr)
require(ggplot2)
require(purrr)

load('dataResults/Results_from_Andrej/FOR_Greg.RData') # paired
load('dataResults/Results_from_Andrej/forGregNew.RData') # paired.new

# some cleaning up..
df1_ <- paired.new %>% 
  distinct(month, hour, area.h, area.l, diff, dist, N, .keep_all = T) %>% # this shouldn't be needed
  mutate(dCFC = diff/100,
         dfor = hl,
         for.cvr.pt1 = area.h,
         for.cvr.pt2 = area.l) 

# shuffle directions of the effect... 
extract = c()
for(i in 1:10)
{
  df1 = df1_
set.seed(i)
n.rows <- dim(df1)[1]
ID.rows <- 1:n.rows %in% sample.int(n = n.rows, size = n.rows/2, replace = F) 

df1[ID.rows,]$for.cvr.pt1 <- df1[ID.rows,]$area.l
df1[ID.rows,]$for.cvr.pt2 <- df1[ID.rows,]$area.h
df1[ID.rows,]$dfor <- -df1[ID.rows,]$hl
df1[ID.rows,]$dCFC <- -df1[ID.rows,]$dCFC

df <- df1 %>% select(-area.h, -area.l) %>%
  mutate(month = recode_factor(month, .ordered = T,
                               `1`="Jan", `2`="Feb", `3`="Mar", `4`="Apr",
                               `5`="May", `6`="Jun", `7`="Jul", `8`="Aug",
                               `9`="Sep", `10`="Oct", `11`="Nov", `12`="Dec"),
         hour = recode_factor(hour, .ordered = T,  `0`="0:00",
                              `1`="1:00", `2`="2:00", `3`="3:00", `4`="4:00",
                              `5`="5:00", `6`="6:00", `7`="7:00", `8`="8:00",
                              `9`="9:00", `10`="10:00", `11`="11:00", `12`="12:00",
                              `13`="13:00", `14`="14:00", `15`="15:00", `16`="16:00",
                              `17`="17:00", `18`="18:00", `19`="19:00", `20`="20:00",
                              `21`="21:00", `22`="22:00", `23`="23:00"))

# filter per region
regio <- 'EUR'
mk.zone <- function(lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = lbl, 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}
zn <- mk.zone(regio,-10,20,42,58)

df <- df %>% 
  filter(lat > min(zn$lat), lat < max(zn$lat), 
         lon > min(zn$lon), lon < max(zn$lon))

# determination of 2d curve
df$x = df$for.cvr.pt1 - df$for.cvr.pt2
df$y = df$for.cvr.pt1 + df$for.cvr.pt2
print(df[1:10,])
months = unique(df$month)
hours = unique(df$hour)
for(month in months)
  for(hour in hours)
  {
    test = lm(dCFC~0+x+I(x*y), data=df[df$month==month&df$hour==hour,])
    extract = rbind(extract, data.frame(i=i, month=month, hour=hour, x=1, y=0, value=test$coefficients[1]+test$coefficients[2], se=predict.lm(test, newdata=data.frame(x=1, y=1), se.fit = TRUE)$se.fit))
  }
}

g1=ggplot(extract, aes(x=month,y=hour,fill=value))+
  geom_tile()+
  scale_fill_gradient2()+
  geom_point(aes(size=se), shape=2)

# set path for these dedicated figures
fpath <- 'synopgraphs' 
dir.create(path = fpath, showWarnings = F, recursive = T)

# define function to plot...
plot.SYNOP.pairs <- function(df, sorting.var, display.var = 6, 
                             thr.dist.max = 60, thr.dist.min = 30, 
                             thr.nyrs.min = 5, thr.dfor.min = 0){
  
  #thr.dist.max = 60 ; thr.dist.min = 30; thr.nyrs.min = 5; thr.dfor.min = 0
  
  if(sorting.var == "hour"){
    month2show <- month.abb[display.var]
    ref.value <- paste0('m', ifelse(display.var < 10, '0', ''), display.var)
    hour2show <- paste0(seq(0, 23, 2), ':00')
    sorting.var.quo = quo(hour)
  }
  if(sorting.var == "month"){
    hour2show <- paste0(display.var, ':00')
    ref.value <- paste0('h', ifelse(display.var < 10, '0', ''), display.var)
    month2show <- month.abb
    sorting.var.quo = quo(month)
  }
  
  df.sub <- df %>% 
    filter(hour %in% hour2show, month %in% month2show) %>%
    filter(dist <= thr.dist.max, 
           dist >= thr.dist.min,
           nyrs >= thr.nyrs.min,
           abs(dfor) >= thr.dfor.min) %>%
    mutate(hour = factor(hour, levels = hour2show, ordered = T)) %>%
    mutate(sorting.var = !!sorting.var.quo)
  
  
  n.grid = 101
  df.mesh <- data.frame(for.cvr.pt1 = rep(seq(0,1, length.out = n.grid), 
                                          times = n.grid), 
                        for.cvr.pt2 = rep(seq(0,1, length.out = n.grid), 
                                          each = n.grid)) %>%
    mutate(dfor = for.cvr.pt1 - for.cvr.pt2)
  
  # fit 2-d surface... (rough way)
  df.fit <- df.sub %>%
    split(.$sorting.var) %>%
    map(~ lm(dCFC ~ for.cvr.pt1 + for.cvr.pt2 - 1, data = .)) 
  # the lm above should be improved by either using a more elastics 2-D surface,
  # including a constraint to the 1:1 line (y - x = 0), or by simplifying using 
  # only a 1-D fit perpendicular to the 1:1 line
  
  df.fit <- df.sub %>%
    split(.$sorting.var) %>%
    map(~ lm(dCFC ~ for.cvr.pt1 + for.cvr.pt2 - 1, data = .)) 
  
  # fit 1-d line ... (simple way)
  df.fit <- df.sub %>%
    split(.$sorting.var) %>%
    map(~ lm(dCFC ~ dfor - 1, data = .)) 
  
  
  df.pred.mesh <- df.fit %>%
    map_dfr(~ predict(object = ., newdata = df.mesh)) %>%
    bind_cols(df.mesh) %>%
    gather(key = sorting.var, value = dCFC, -for.cvr.pt1, -for.cvr.pt2, -dfor) %>%
    mutate(sorting.var = factor(sorting.var, levels = levels(df.sub$sorting.var), ordered = T))
  
  df.pred.val <- df.fit %>%
    map_dfr(~ predict(object = ., se.fit = T, 
                      newdata = data.frame(for.cvr.pt1 = 1, for.cvr.pt2 = 0, dfor = 1))) %>%
    mutate(r.squared = map(df.fit, summary) %>% map_dbl("r.squared"),
           sorting.var =  factor(names(df.fit), levels = levels(df.sub$sorting.var), ordered = T),
           val.label = paste0(round(fit, digits = 3), '%+-%',
                              round(2 * se.fit, digits = 3), ' %->% phantom(1)'),
           val.signif = abs(fit) > 2 * se.fit) 
  
  
  big.title <- 'Effect of change in forest cover on cloud cover based on SYNOP'
  sub.title <-  paste0(regio, ' | ', ref.value, 
                       ' | MinDist: ', thr.dist.min, 'km',
                       ' | MaxDist: ', thr.dist.max, 'km', 
                       ' | MinYears: ', thr.nyrs.min, 'yrs',
                       ' | MinDfor: ', 100 * thr.dfor.min, '%')
  
  lim.colors <- c(-0.08, 0.08)
  
  g <- ggplot(df.sub, aes(x = for.cvr.pt1, y = for.cvr.pt2)) +
    geom_raster(data = df.pred.mesh, aes(fill = dCFC)) +
    geom_point(aes(fill = dCFC), shape = 21,  size = 2) +
    geom_text(data = df.pred.val %>% filter(val.signif == F), x = 1, y = 0,
              aes(label = val.label), colour = "grey50", parse = TRUE, hjust = 1) +
    geom_text(data = df.pred.val %>% filter(val.signif == T), x = 1, y = 0,
              aes(label = val.label), colour = "black", parse = TRUE, hjust = 1) +
    geom_point(data = df.pred.val, x = 1, y = 0, shape = 3, size = 3, 
               aes(colour = val.signif)) +
    geom_abline(colour = "grey30") +
    facet_wrap(~sorting.var, nc = 4) +
    scale_colour_manual(values = c("grey50", "black"), guide = 'none') +
    scale_fill_gradientn('Change in cloud fraction cover',
                         colours = RColorBrewer::brewer.pal(9,'RdBu'),
                         limits = lim.colors, oob = scales::squish) + 
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    xlab('Forest cover at SYNOP site 1') +
    ylab('Forest cover at SYNOP site 2') +
    ggtitle(big.title, subtitle = sub.title) + 
    theme(legend.position = 'bottom',
          legend.key.width = unit(2.4, "cm")) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
  
  ggsave(filename = paste0('SYNOP-pairs-in-',regio,'-by-', sorting.var, '-for-', ref.value,
                           '_', thr.dist.min,'km_',thr.dist.max, 'km_', 
                           thr.nyrs.min,'yrs_',thr.dfor.min,'dif', '.png'),
         plot = g, path = fpath, width = 9, height = 9)
  
}

for(iMonth in 1:12){
  plot.SYNOP.pairs(df, sorting.var = 'hour', display.var = iMonth)
}

for(iHour in c(6,9,12,15,18)){
  plot.SYNOP.pairs(df, sorting.var = 'month', display.var = iHour)
}


for(iDfor in seq(0,.20,.05)){
  plot.SYNOP.pairs(df, sorting.var = 'hour', display.var = 6, thr.dfor.min = iDfor)
  plot.SYNOP.pairs(df, sorting.var = 'month', display.var = 14, thr.dfor.min = iDfor )
}


