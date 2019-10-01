require(dplyr)
require(tidyr)


## Prepraring the data ----

load('dataResults/Results_from_Andrej/forGregNew.RData') # paired.new

# some cleaning up..
df1 <- paired.new %>% 
  distinct(month, hour, area.h, area.l, diff, dist, N, .keep_all = T) %>% # this shouldn't be needed
  mutate(dCFC = diff/100,
         dfor = hl,
         for.cvr.pt1 = area.h,
         for.cvr.pt2 = area.l) 

# shuffle directions of the effect... 
set.seed(1982)
n.rows <- dim(df1)[1]
ID.rows <- 1:n.rows %in% sample.int(n = n.rows, size = n.rows/2, replace = F) 

df1[ID.rows,]$for.cvr.pt1 <- df1[ID.rows,]$area.l
df1[ID.rows,]$for.cvr.pt2 <- df1[ID.rows,]$area.h
df1[ID.rows,]$dfor <- -df1[ID.rows,]$hl
df1[ID.rows,]$dCFC <- -df1[ID.rows,]$dCFC

df <- df1 %>% 
  dplyr::select(-area.h, -area.l) %>%
  dplyr::mutate(month = recode_factor(month, .ordered = T,
                                      `1`="Jan", `2`="Feb", `3`="Mar", `4`="Apr",
                                      `5`="May", `6`="Jun", `7`="Jul", `8`="Aug",
                                      `9`="Sep", `10`="Oct", `11`="Nov", `12`="Dec"),
                hour = recode_factor(hour, .ordered = T,  `0`="00:00",
                                     `1`="01:00", `2`="02:00", `3`="03:00", `4`="04:00",
                                     `5`="05:00", `6`="06:00", `7`="07:00", `8`="08:00",
                                     `9`="09:00", `10`="10:00", `11`="11:00", `12`="12:00",
                                     `13`="13:00", `14`="14:00", `15`="15:00", `16`="16:00",
                                     `17`="17:00", `18`="18:00", `19`="19:00", `20`="20:00",
                                     `21`="21:00", `22`="22:00", `23`="23:00"))

# region to filter
mk.zone <- function(lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = lbl, 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}

zn.eur <- mk.zone('eur',-10,20,42,58)
zn.nam <- mk.zone('nam',-120,-60,40,60)
zn.crn <- mk.zone('crn',-95,-82,36,44)
zn.ind <- mk.zone('ind',70,90,5,30)
zn.aus <- mk.zone('aus',140,155,-45,-18)
zn.rus <- mk.zone('rus',20,110,45,65)
zn.ama <- mk.zone('ama',-70,-45,-15,-5)
zn.afr <- mk.zone('afr',10,42,-25,-5)






## The function ----

# define function to plot...
get.SYNOP.values <- function(df, zn = NULL, min.num.samples = 5,
                             thr.dist.max = 60, thr.dist.min = 30, 
                             thr.nyrs.min = 5, thr.dfor.min = 0){
  
  
  # filter by region if needed
  if(!is.null(zn)){ 
    df.loc <- df %>% 
      filter(lat > min(zn$lat), lat < max(zn$lat), 
             lon > min(zn$lon), lon < max(zn$lon))
    regio.name <- levels(zn$lbl)
  } else {
    df.loc <- df
    regio.name <- 'ALL'
  }
  
  
  # pre-allocate output
  df.output.dum <- data.frame(
    regio = regio.name,
    month = rep(unique(df$month), times = 24),
    hour = rep(unique(df$hour), each = 12), 
    thr.dist.max = thr.dist.max, 
    thr.dist.min = thr.dist.min, 
    thr.nyrs.min = thr.nyrs.min, 
    thr.dfor.min = thr.dfor.min)
  
  
  
  
  
  # main filtering 
  df.sub <- df.loc %>% 
    #filter(month == iMonth) %>% # should tweak to make it not per month
    filter(dist <= thr.dist.max, 
           dist >= thr.dist.min,
           nyrs >= thr.nyrs.min,
           abs(dfor) >= thr.dfor.min) %>%
    mutate(cases = paste(month, hour))
  
  # count occurences
  df.count <- df.sub %>% 
    count(cases) 
  
  
  #if(length(df.count$n) == 0){ return(df.output) }
  
  # subselect those where no points are available for fit... 
  df.select <- df.sub %>%
    right_join(df.count %>% filter(n > min.num.samples), by = 'cases')
  
  if(dim(df.select)[1] == 0){ 
    df.output <- df.output.dum %>%
      mutate(dCFC = NA, val.signif = F)
    return(df.output) 
  }
  
  ## The fitting ----
  df.fit <- df.sub %>%
    split(.$cases) %>%
    map(~ lm(dCFC ~ dfor - 1, data = .)) 
  
  # # This pred on mesh is not necessary here, but kept by legacy in case we plot
  # df.pred.mesh <- df.fit %>%
  #   map_dfr(~ predict(object = ., newdata = df.mesh)) %>%
  #   bind_cols(df.mesh) %>%
  #   gather(key = cases, value = dCFC, -for.cvr.pt1, -for.cvr.pt2, -dfor) 
  
  
  
  df.pred.val <- df.fit %>%
    map_dfr(~ predict(object = ., se.fit = T, 
                      newdata = data.frame(for.cvr.pt1 = 1, for.cvr.pt2 = 0, dfor = 1))) %>%
    mutate(cases = names(df.fit),
           r.squared = map(df.fit, summary) %>% map_dbl("r.squared"),
           #hour =  factor(names(df.fit), levels = levels(df.sub$hour), ordered = T),
           val.label = paste0(round(fit, digits = 3), '%+-%',
                              round(2 * se.fit, digits = 3), ' %->% phantom(1)'),
           val.signif = abs(fit) > 2 * se.fit) 
  
  
  
  ## exporting with relevant info ----
  df.pred.val <- df.pred.val %>% 
    rename(dCFC = fit) %>%
    mutate(month = factor(substring(cases, 1, 3), 
                          levels = levels(df.sub$month), ordered = T),
           hour = factor(substring(cases, 5, 9), 
                         levels = levels(df.sub$hour), ordered = T))
  
  df.output <- left_join(df.output.dum, df.pred.val, 
                         by = c("month", "hour"))
  
  return(df.output)
}



df.all <- data.frame(NULL)
out.df <- get.SYNOP.values(df, zn = zn.eur)
df.all <- rbind(df.all, out.df)
out.df <- get.SYNOP.values(df, zn = zn.nam)
df.all <- rbind(df.all, out.df)
out.df <- get.SYNOP.values(df, zn = zn.rus)
df.all <- rbind(df.all, out.df)
out.df <- get.SYNOP.values(df, zn = zn.ama)
df.all <- rbind(df.all, out.df)
out.df <- get.SYNOP.values(df, zn = zn.ind)
df.all <- rbind(df.all, out.df)




require(ggplot2)
ggplot(df.all, aes(x = hour, y = month)) +
  geom_tile(aes(fill = dCFC)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_gradientn('Change in cloud fraction cover',
                       colors = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = c(-0.08,0.08), oob = scales::squish) +
  coord_polar() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))





require(ggplot2)
ggplot(df.all, aes(x = hour, y = month)) +
  geom_tile(aes(fill = dCFC)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_gradientn('Change in cloud fraction cover',
                       colors = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = c(-0.08,0.08), oob = scales::squish) +
  coord_polar() +
  facet_wrap(~regio) +
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
