#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Make figure showing proportions of positive/negative cloud effects
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


require(dplyr)
require(tidyr)
require(ggplot2)


# load all necessary data
load(paste0(dat4fig_path, '/df_dCFC_MOD05_FOR_1dd.Rdata'))
load(paste0(dat4fig_path, '/df_dCFC_MOD05_EFO_1dd.Rdata'))
load(paste0(dat4fig_path, '/df_dCFC_MOD05_DFO_1dd.Rdata'))
load(paste0(dat4fig_path, '/df_dCFC_MOD05_FOR.Rdata'))
load(paste0(dat4fig_path, '/df_dCFC_MOD05_EFO.Rdata'))
load(paste0(dat4fig_path, '/df_dCFC_MOD05_DFO.Rdata'))

# structure it in a df
df.avg <- bind_rows(
  df_dCFC_MOD05_FOR %>% 
    summarize(type = 'FOR', n = length(dCFC),
              avg = mean(dCFC), sd = sd(dCFC), se = sd/sqrt(n)),
  df_dCFC_MOD05_EFO %>% 
    summarize(type = 'EFO', n = length(dCFC),
              avg = mean(dCFC), sd = sd(dCFC), se = sd/sqrt(n)),
  df_dCFC_MOD05_DFO %>% 
    summarize(type = 'DFO', n = length(dCFC),
              avg = mean(dCFC), sd = sd(dCFC), se = sd/sqrt(n)))

thr <- 0.001 

month6s <- month.abb[c(7:12,1:6)]
names(month6s) = month.abb 



df.surf.month <- bind_rows(
  df_dCFC_MOD05_FOR %>% 
    mutate(monthS = factor(ifelse(lat >= 0, month.abb[month], month6s[month]),
                           levels = month.abb, ordered = T)) %>%
    group_by(monthS) %>% 
    summarize(type = 'All forest types combined',
              pos = sum((dCFC > (0 + thr)), na.rm = T),
              neg = sum((dCFC < (0 - thr)), na.rm = T),
              tot = sum(!is.na(dCFC))),
  df_dCFC_MOD05_EFO %>% 
    mutate(monthS = factor(ifelse(lat >= 0, month.abb[month], month6s[month]),
                           levels = month.abb, ordered = T)) %>%
    group_by(monthS) %>% 
    summarize(type = 'Only evergreen forests',
              pos = sum((dCFC > (0 + thr)), na.rm = T),
              neg = sum((dCFC < (0 - thr)), na.rm = T),
              tot = sum(!is.na(dCFC))),
  df_dCFC_MOD05_DFO %>% 
    mutate(monthS = factor(ifelse(lat >= 0, month.abb[month], month6s[month]),
                           levels = month.abb, ordered = T)) %>%
    group_by(monthS) %>% 
    summarize(type = 'Only deciduous forests',
              pos = sum((dCFC > (0 + thr)), na.rm = T),
              neg = sum((dCFC < (0 - thr)), na.rm = T),
              tot = sum(!is.na(dCFC))))

df.surf <- df.surf.month %>%
  mutate(pct.pos = 100 * pos/tot,
         pct.neg = 100 * neg/tot) %>%
  pivot_longer(cols = c('pct.pos', 'pct.neg'), 
               names_to = 'sign', values_to = 'pct') %>%
  group_by(type, sign) %>%
  mutate(avg.pct = mean(pct))


# mean(filter(df.surf, type == 'FOR', sign == 'pct.pos', month %in% month.abb[5:9])$pct)


lbls.pct <- c('pct.pos' = paste('Pixels where change in cloud fraction cover is POSITIVE (above', thr,')'), 
              'pct.neg' = paste('Pixels where change in cloud fraction cover is NEGATIVE (below', -thr,')'))


g_bars <- ggplot(df.surf) + 
  geom_bar(aes(x = monthS, y = pct, fill = sign), 
           stat = 'identity', position = 'dodge') + 
  geom_hline(aes(yintercept = avg.pct, colour = sign)) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~type, nc = 1) +
  scale_x_discrete('') + 
  scale_y_continuous('Percentage of sampled pixels', expand = c(0,0)) +
  scale_fill_manual('', labels = lbls.pct, guide = guide_legend(nrow = 2), 
                    values = c('pct.neg' = col.pal[3], 'pct.pos' = col.pal[length(col.pal)-2])) +
  scale_colour_manual('', guide = 'none', 
                    values = c('pct.neg' = col.pal[2], 'pct.pos' = col.pal[length(col.pal)-1])) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        axis.line = element_line(size = 0.5, colour = 'Grey20'),
        axis.ticks = element_line(size = 0.5, colour = 'Grey20'),
        axis.title = element_text(size = rel(1.1))) +
  ggtitle('Extent of pixels showing a given effect on CFrC',
          subtitle = 'Considering potential afforestation over short vegetation')


## printing the final plot -----
fig.name <- 'figSM___ExtentForestWithSign'
fig.width <- 6; fig.height <- 8;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname,
       plot = g_bars, width = fig.width, height = fig.height)
  
