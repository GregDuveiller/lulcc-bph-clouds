library(data.table)
library(ggplot2)
library(dplyr)

load("dataResults/Results_from_Jed/satsite.Rda")

tbl <- as.data.frame(satsite)

tbl.mbe <- tbl %>%
  filter(!is.na(synop+aqua+SD)) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  mutate(month = factor(format(time, '%b'), levels = month.abb)) %>%
  mutate(SD_class_interval = cut(SD, breaks = c(0, seq(1,29,5), seq(30,80,10), 355), 
                                 include.lowest = T)) %>%
  group_by(SD_class_interval, month) %>% 
  summarise(MBE = mean(aqua - synop, na.rm = T))

p <- ggplot(tbl.mbe, aes(x = SD_class_interval, y = MBE)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~month) + 
  labs(x="Snow depth (cm)", y="MBE (% CFC)") 






tbl.mbe <- tbl %>%
  filter(!is.na(synop+aqua+SD)) %>%
  mutate(SD_class_interval = cut(SD, breaks = c(0, seq(1,29,5), seq(30,80,10), 355), 
                                 include.lowest = T))

ggplot(tbl.mbe, aes(x = SD_class_interval, y = aqua - synop)) + 
  geom_boxplot(outlier.shape = NA) +
  labs(x="Snow depth (cm)", y="MBE (% CFC)") + 
  coord_cartesian(ylim = c(-30, 20))



# define threshold of SD when COMET performance is stable
satsite[, SD_class_interval := cut2(SD, cuts=c(0, seq(1,29,5), seq(30,80,10)), levels.mean = F, oneval=FALSE)]
satsite[, SD_class_mean := cut2(SD, cuts=c(0, seq(1,29,5), seq(30,80,10)), levels.mean = T, oneval=FALSE)]

x <- satsite[!is.na(synop+aqua+SD), list(MBE=mean(aqua-synop, na.rm=T)),
                  by=list(SD_class_mean, SD_class_interval, Kod)]
x[, sat:= "CCI_aqua"]
setorder(x, SD_class_mean)   
x <- x[!is.na(SD_class_mean),]

p <- ggplot(x, aes(x=SD_class_interval, y=MBE)) + 
  geom_boxplot(outlier.shape = NA) +
  labs(x="Snow depth (cm)", y="MBE (% CFC)") + 
  coord_cartesian(ylim = c(-30, 20))
print(p)
