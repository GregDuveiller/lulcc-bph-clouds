library(data.table)
library(ggplot2)
library(dplyr)

load("dataResults/Results_from_Jed/satsite.Rda")

## to get data from MODIS delta results

# select the points
tbl.pts <- as.data.frame(satsite) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  group_by(Kod) %>%
  summarize(x = signif(unique(lon), digits = 4), 
            y = signif(unique(lat), digits = 4))

# extract data from MODIS S4T values
dat0 <- raster::extract(x = brick('dataFigures/rstr_dCFC_MOD05_FOR.nc'), 
                        y = dplyr::select(tbl.pts, -Kod)) 
colnames(dat0) <- month.abb
dat1 <- cbind(dplyr::select(tbl.pts, Kod), dat0)
dat2 <- dat1[apply(is.na(dat0), 1, sum) < 5,]
dat3 <- pivot_longer(data = dat2, cols = month.abb, 
                     names_to = 'month', values_to = 'dCFC')

tbl.s4t <- dat3 %>%
  transmute(Kod = Kod, 
            month = factor(month, levels = month.abb),
            dCFC_afforestation = dCFC)


tbl.grd <- as.data.frame(satsite) %>%
  filter(!is.na(synop + aqua + SD)) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  mutate(month = factor(format(time, '%b'), levels = month.abb)) %>%
  mutate(SD_class = cut(SD, breaks = c(-1,0,1,5,30,400) , right = T, 
                        labels = c('No Snow', '< 1', '(1,5]', '(5,30]', '< 30'))) %>%
  group_by(SD_class, month, Kod) %>% 
  summarize(dCFC_aqua_synop = mean((aqua/100) - (synop/100), na.rm = T),
            num.obs = length(aqua)) %>%
  right_join(tbl.s4t, by = c('Kod', 'month'))


ggplot(tbl.grd %>% filter(month == 'Apr')) +
  geom_boxplot(aes(x = SD_class, y = dCFC_aqua_synop), outlier.shape = 1)


ggplot(tbl.grd %>% filter(month %in% month.abb[1:4])) +
  geom_point(aes(x = dCFC_afforestation, y = dCFC_aqua_synop)) + 
  facet_grid(month~SD_class)








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
