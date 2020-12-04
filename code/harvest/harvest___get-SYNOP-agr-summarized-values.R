#!/usr/local/bin/Rscript
################################################################################
# Purpose: Harvest the SYNOP data to make the plots for a given region 
# License: GPL v3
# Authors: Gregory Duveiller - Dec. 2020
################################################################################


require(dplyr)
require(tidyr)
require(purrr)


## Preparing the data ----
load(paste0(results_path, '/', 'groundstation_analysis', '/final_synop_paired_data.RData')) # paired.new

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

## define the target region  ----
mk.zone <- function(lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = factor(lbl), 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}

zn.eur <- mk.zone('eur', -10, 50, 30, 65)
regio.name <- levels(zn.eur$lbl)


## Define the functions that will do the job  ----

# Function for aggregating values per time and month ...
get.SYNOP.agr.values <- function(df, zn = NULL, min.num.samples = 5,
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
  df.fit <- df.select %>%
    split(.$cases) %>%
    purrr::map(~ lm(dCFC ~ dfor - 1, data = .)) 
 
  
  df.pred.val <- df.fit %>%
    map_dfr(~ predict(object = ., se.fit = T, 
                      newdata = data.frame(for.cvr.pt1 = 1, for.cvr.pt2 = 0, dfor = 1))) %>%
    mutate(cases = names(df.fit),
           r.squared = map(df.fit, summary) %>% map_dbl("r.squared"),
           val.label = paste0(round(fit, digits = 3), '%+-%',
                              round(2 * se.fit, digits = 3), ' %->% phantom(1)'),
           val.signif = abs(fit) > 2 * se.fit) 
  
  
  
  ## exporting with relevant info ----
  df.pred.val <- df.pred.val %>% 
    left_join(df.count, by = 'cases') %>%
    rename(dCFC = fit) %>%
    mutate(month = factor(substring(cases, 1, 3), 
                          levels = levels(df.sub$month), ordered = T),
           hour = factor(substring(cases, 5, 9), 
                         levels = levels(df.sub$hour), ordered = T))
  
  
  
  df.output <- left_join(df.output.dum, df.pred.val, 
                         by = c("month", "hour"))
  
  # this could be done more elegantly I suppose
  df.output$val.signif[is.na(df.output$val.signif)] <- FALSE
  
  return(df.output)
}

# (ancillary) Function to retain the unique points retained  ...
get.SYNOP.unique.loc <- function(df, zn = NULL, min.num.samples = 5,
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
  
  
  
  # main filtering 
  df.sub <- df.loc %>% 
    filter(dist <= thr.dist.max, 
           dist >= thr.dist.min,
           nyrs >= thr.nyrs.min,
           abs(dfor) >= thr.dfor.min) %>%
    mutate(cases = paste(month, hour)) %>% 
    group_by(lat, lon) %>%
    count() %>%
    mutate(thr.dist.max = thr.dist.max, 
           thr.dist.min = thr.dist.min, 
           thr.nyrs.min = thr.nyrs.min, 
           thr.dfor.min = thr.dfor.min) %>%
    ungroup()
  
  return(df.sub)
}




# aggregate all the values 
df_SYNOP_agr <- data.frame(NULL)
df_SYNOP_loc <- data.frame(NULL)

for(i.thr.dist.max in seq(60,100,20)){
  for(i.thr.dist.min in seq(20,40,10)){
    for(i.thr.nyrs.min in seq(5,9,2)){
      for(i.thr.dfor.min in seq(0,0.2,0.1)){
        out.df <- get.SYNOP.agr.values(df, zn = zn.eur, 
                                       thr.dfor.min = i.thr.dfor.min,
                                       thr.nyrs.min = i.thr.nyrs.min,
                                       thr.dist.max = i.thr.dist.max,
                                       thr.dist.min = i.thr.dist.min)
        df_SYNOP_agr <- rbind(df_SYNOP_agr, out.df)
        
        out.df <- get.SYNOP.unique.loc(df, zn = zn.eur, 
                                       thr.dfor.min = i.thr.dfor.min,
                                       thr.nyrs.min = i.thr.nyrs.min,
                                       thr.dist.max = i.thr.dist.max,
                                       thr.dist.min = i.thr.dist.min)
        df_SYNOP_loc <- rbind(df_SYNOP_loc, out.df)
        
      }}}
}


df_filename <- paste0('df_SYNOP_agr_4polarplots_', regio.name)
save('df_SYNOP_agr', 'df_SYNOP_loc',
     file = paste0(harvest_path, '/', df_filename, '.RData'))
