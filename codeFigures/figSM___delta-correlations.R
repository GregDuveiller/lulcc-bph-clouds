require(ggplot2)
require(dplyr)
require(grid)


load('dataFigures/df_multiDeltaDF.Rda') # df_all

seasons <- factor(x = c(rep('DJF',2), rep('MAM',3), 
                        rep('JJA',3), rep('SON',3), 'DJF'), 
                  levels = c('DJF','MAM','JJA','SON'), 
                  labels = c('December to February (DJF)',
                             'March to May (MAM)',
                             'June to August (JJA)',
                             'September to November (SON)'))

df.seasonal <- data.frame(month = factor(month.abb, levels = month.abb),
                          season = seasons)


df_all <- df_all %>% left_join(df.seasonal, by = "month")

df_cor <- df_all %>% 
  group_by(season, PFT, region) %>% 
  summarise(HG = cor(dCFC,delta_HG),
            LE = cor(dCFC,delta_LE),
            Rn = cor(dCFC,delta_Rn),
            albedo = cor(dCFC,delta_albedo),
            LSTday = cor(dCFC,delta_LSTday),
            LSTnight = cor(dCFC,delta_LSTnight)) %>%
  pivot_longer(cols = c('HG','LE','Rn','albedo', 'LSTday', 'LSTnight'),
               names_to = 'variable', values_to = 'correlation')
  

ggplot(df_cor) +
  geom_bar(aes(x = PFT, y = correlation, fill = variable),
           stat = 'identity', position = 'dodge') +
  facet_grid(season~region)
