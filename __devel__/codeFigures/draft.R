

load('dataFigures/satsite.Rda')

df2plot <- satsite %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31'),
         !is.na(aqua)) %>%
  mutate(month = format(time, '%b')) 
  

ggplot(df2plot) +
  geom_boxplot(aes(x = cut(SD,breaks = c(0,seq(1,26,5),seq(30,80,10),355)), 
                   y = aqua - synop)) +
  facet_wrap(~month)
