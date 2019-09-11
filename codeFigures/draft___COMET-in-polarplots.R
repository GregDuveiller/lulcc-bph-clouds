library(dplyr)
library(ggplot2)
library(tidyr)


load('dataFigures/df_dCFC_COMET_FOR_1dd.Rdata') # df_dCFC_COMET_FOR_1dd.Rdata

df <- df_dCFC_COMET_EFO_1dd %>% 
  filter(lon >= -10, lon <= 20, lat >= 42, lat <= 58) %>%
  group_by(hour, month) %>%
  summarize(dCFC.mu = mean(dCFC, na.rm = T),
                   dCFC.ttest_pval = t.test(dCFC)$p.value,
                   dCFC.stddev = sd(dCFC, na.rm = T),
                   dCFC.stderr = sd(dCFC, na.rm = T)/sqrt(sum(!is.na(dCFC)))) %>%
  mutate(month = factor(month.abb[month], levels = month.abb))


lim.colors <- c(-0.1, 0.1)

ggplot(df) +
  geom_tile(aes(x = factor(month), y = factor(hour), fill = dCFC.mu)) +
  geom_point(aes(x = factor(month), y = factor(hour), 
                 alpha = factor(dCFC.ttest_pval < 0.05))) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors/5, oob = scales::squish)+
  coord_polar()

ggplot(df) +
  geom_tile(aes(y = factor(month), x = factor(hour), fill = dCFC.mu)) +
  geom_point(aes(y = factor(month), x = factor(hour), 
                 alpha = factor(dCFC.ttest_pval < 0.05))) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1), guide = 'none') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,'RdBu'),
                       limits = lim.colors/5, oob = scales::squish)+
  coord_polar()
