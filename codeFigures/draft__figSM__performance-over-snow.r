library(ggplot2)
library(dplyr)
library(tidyr)

# get data
load('dataFigures/df.groundDataRS.snowbias.Rda')


## plot with various snow classes ----

tbl.grd <- as.data.frame(satsite) %>%
  filter(!is.na(synop + aqua + SD)) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  mutate(month = factor(format(time, '%b'), levels = month.abb)) %>%
  mutate(SD_class = cut(SD, breaks = c(-1,0,5,30,400) , right = T, 
                        labels = c('No Snow', '(0,5]', '(5,30]', '> 30'))) %>%
  group_by(SD_class, month, Kod) %>% 
  summarize(dCFC_aqua_synop = mean((aqua/100) - (synop/100), na.rm = T),
            num.obs = length(aqua)) %>%
  right_join(tbl.s4t, by = c('Kod', 'month'))

tbl4plot.raw <- tbl.grd %>% filter(month %in% month.abb[1:4])
tbl4plot.sum <- tbl4plot.raw %>% group_by(SD_class, month) %>%
  summarize(mu.dCFC_af = mean(dCFC_afforestation, na.rm = T),
            mu.dCFC_as = mean(dCFC_aqua_synop, na.rm = T))

g1 <- ggplot(tbl4plot.raw) +
  geom_hline(yintercept = 0, colour = 'grey70') + 
  geom_vline(xintercept = 0, colour = 'grey70') + 
  geom_point(aes(x = dCFC_afforestation, y = dCFC_aqua_synop)) + 
  geom_point(data = tbl4plot.sum,
             aes(x = mu.dCFC_af, y = mu.dCFC_as),
             shape = 21, fill = 'cornflowerblue', size = 3) + 
  facet_grid(month~SD_class) +
  labs(title = 'The effect of snow based on ground data') +
  xlab('Change in CFC due to afforestation (from satellite)') +
  ylab('Bias in CFC at stations (Satellite - SYNOP)')

ggsave(filename = 'textFigures/xtras/figXtra_bias_snowClasses.png',
       plot = g1, width = 9, height = 8)



## plot with binary snow/no_snow separation ----

tbl.grd2 <- as.data.frame(satsite) %>%
  filter(!is.na(synop + aqua + SD)) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  mutate(month = factor(format(time, '%b'), levels = month.abb)) %>%
  mutate(SnowFlag = cut(SD, breaks = c(-1,0,400) , right = T, 
                        labels = c('NoSnow', 'Snow'))) %>%
  group_by(SnowFlag, month, Kod) %>% 
  summarize(dCFC_aqua_synop = mean((aqua/100) - (synop/100), na.rm = T),
            num.obs = length(aqua)) %>%
  right_join(tbl.s4t, by = c('Kod', 'month'))

tbl4plot.raw <- tbl.grd2 %>% filter(month %in% month.abb[1:4])
tbl4plot.sum <- tbl4plot.raw %>% group_by(SnowFlag, month) %>%
  summarize(mu.dCFC_af = mean(dCFC_afforestation, na.rm = T),
            mu.dCFC_as = mean(dCFC_aqua_synop, na.rm = T))

g2 <- ggplot(tbl4plot.raw) +
  geom_hline(yintercept = 0, colour = 'grey70') + 
  geom_vline(xintercept = 0, colour = 'grey70') + 
  geom_point(aes(x = dCFC_afforestation, y = dCFC_aqua_synop)) + 
  geom_point(data = tbl4plot.sum,
             aes(x = mu.dCFC_af, y = mu.dCFC_as),
             shape = 21, fill = 'cornflowerblue', size = 3) + 
  facet_grid(month~SnowFlag) +
  labs(title = 'The effect of snow based on ground data') +
  xlab('Change in CFC due to afforestation (from satellite)') +
  ylab('Bias in CFC at stations (Satellite - SYNOP)')

ggsave(filename = 'textFigures/xtras/figXtra_bias_snowBinary.png',
       plot = g2, width = 6, height = 8)


## Dev [some tests... of which I am not convinced]... ----

# focusing only on the negative dCFC_aff values... 
tbl.grd3 <- as.data.frame(satsite) %>%
  filter(!is.na(synop + aqua + SD)) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  mutate(month = factor(format(time, '%b'), levels = month.abb)) %>%
  right_join(tbl.s4t, by = c('Kod', 'month')) %>%
  filter(dCFC_afforestation < -0.01) %>%
  mutate(SnowFlag = cut(SD, breaks = c(-1,0,400) , right = T, 
                        labels = c('NoSnow', 'Snow'))) %>%
  group_by(SnowFlag, month, Kod) %>% 
  summarize(dCFC_aqua_synop = mean((aqua/100) - (synop/100), na.rm = T),
            num.obs = length(aqua)) %>%
  right_join(tbl.s4t, by = c('Kod', 'month'))



tbl4plot.raw <- tbl.grd3 %>% filter(month %in% month.abb[1:4])

ggplot(tbl4plot.raw) +
  geom_boxplot(aes(x = SnowFlag, y = dCFC_aqua_synop),
               outlier.shape = NA) + 
  facet_grid(month~.) 



# Isolation of snow bias effect  ----

tbl.grd3 <- tbl.grd2 %>% 
  filter(month %in% month.abb[1:4]) %>% 
  group_by(SnowFlag, month) %>%
  summarize(mu.dCFC_as = mean(dCFC_aqua_synop, na.rm = T)) %>%
  pivot_wider(names_from = SnowFlag, values_from = mu.dCFC_as) %>%
  transmute(month = month, SnowBias = Snow - NoSnow) 

tbl.grd4 <- tbl.grd2 %>% 
  filter(month %in% month.abb[1:4]) %>%
  left_join(y = tbl.grd3, by = 'month')

tbl4plot.sum <- tbl4plot.raw %>% group_by(SD_class, month) %>%
  summarize(mu.dCFC_af = mean(dCFC_afforestation, na.rm = T),
            mu.dCFC_as = mean(dCFC_aqua_synop, na.rm = T))


ggplot(tbl.grd4) +
  geom_hline(yintercept = 0, colour = 'grey70') + 
  geom_vline(xintercept = 0, colour = 'grey70') + 
  geom_point(aes(x = dCFC_afforestation, y = dCFC_aqua_synop), color = 'grey50') + 
  geom_point(aes(x = dCFC_afforestation, y = dCFC_aqua_synop - SnowBias)) + 
  # geom_point(data = tbl4plot.sum,
  #            aes(x = mu.dCFC_af, y = mu.dCFC_as),
  #            shape = 21, fill = 'cornflowerblue', size = 3) + 
  facet_grid(month~SnowFlag)
  





# ## FROM JED
# 
# # define threshold of SD when COMET performance is stable
# satsite[, SD_class_interval := cut2(SD, cuts=c(0, seq(1,29,5), seq(30,80,10)), levels.mean = F, oneval=FALSE)]
# satsite[, SD_class_mean := cut2(SD, cuts=c(0, seq(1,29,5), seq(30,80,10)), levels.mean = T, oneval=FALSE)]
# 
# x <- satsite[!is.na(synop+aqua+SD), list(MBE=mean(aqua-synop, na.rm=T)),
#                   by=list(SD_class_mean, SD_class_interval, Kod)]
# x[, sat:= "CCI_aqua"]
# setorder(x, SD_class_mean)   
# x <- x[!is.na(SD_class_mean),]
# 
# p <- ggplot(x, aes(x=SD_class_interval, y=MBE)) + 
#   geom_boxplot(outlier.shape = NA) +
#   labs(x="Snow depth (cm)", y="MBE (% CFC)") + 
#   coord_cartesian(ylim = c(-30, 20))
# print(p)
