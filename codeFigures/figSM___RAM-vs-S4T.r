### XTRA FIGURE summarizing results with alt-methodology ----

# load necessary packages
require(dplyr)
require(grid)
require(ggplot2)
require(here)


# NOTE: Could be improved by adding maps of where climate zones are...


## Initial data preparation and parametrization ---- 

load('dataFigures/df_RAM-vs-S4T_CZ5.RData') # df_CZ5
iPFT <- 'TOT'
# NOTE: Should check if the df above actually used the right combo for TOT...
# (did that harvester run after the tweaks in the DFO + EFO weighted combo)

colour.meth1 <- c('#95559B', '#B892BB')
colour.meth2 <- c('#D89800', '#ECD8A6')

method.lbls <- c('S4T' = 'Space-for-time\nsubstitution', 'RAM' = 'Measure over\nreal changes')

## Make the bar plot ----
g_bars <- ggplot(df_CZ5 %>% 
                   filter(PFT == iPFT)) +
  geom_bar(aes(x = month, y = dCFC_CZ5, fill = method, colour = method), 
           stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(x = month, color = method,
                    ymin = dCFC_CZ5 - dCFC_CZ5_STD_err, 
                    ymax = dCFC_CZ5 + dCFC_CZ5_STD_err),
                position = 'dodge', show.legend = F) +
  geom_hline(yintercept = 0, colour = 'grey40', size = 0.5) +
  facet_wrap(~region, nc = 1) + 
  scale_fill_manual('Method used:', labels = method.lbls, 
                    values = c('S4T' = colour.meth1[2], 'RAM' = colour.meth2[2])) +
  scale_color_manual('Method used:', labels = method.lbls, 
                     values = c('S4T' = colour.meth1[1], 'RAM' = colour.meth2[1])) +
  scale_y_continuous('Change in cloud fractional cover') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        axis.line = element_line(size = 0.5, colour = 'Grey20'),
        axis.ticks = element_line(size = 0.5, colour = 'Grey20'),
        axis.title = element_text(size = rel(1.1))) + 
  ggtitle('Change in cloud fractional cover following afforestation',
          subtitle = 'Averages over major Koppen-Geiger climate zones with different methods')




## printing the final plot -----
fig.name <- 'figSM___RAM-vs-S4T'
fig.width <- 6; fig.height <- 8;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.fmt, '/', fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname,
       plot = g_bars, width = fig.width, height = fig.height)
