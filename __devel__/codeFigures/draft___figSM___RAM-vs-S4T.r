require(ggplot2)
require(dplyr)
require(grid)


load('dataFigures/df_RAM-vs-S4T_CZ5.RData') # df_CZ5

iPFT <- 'DFO'


# some plot
g_bars <- ggplot(df_CZ5 %>% 
                   filter(PFT == iPFT)) +
  geom_bar(aes(x = month, y = dCFC_CZ5, fill = method), 
           stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(x = month, color = method,
                    ymin = dCFC_CZ5 - dCFC_CZ5_STD_err, 
                    ymax = dCFC_CZ5 + dCFC_CZ5_STD_err),
                position = 'dodge') +
  geom_hline(yintercept = 0, colour = 'grey40', size = 0.5) +
  facet_grid(region~PFT) + 
  scale_fill_discrete('Method used:') +
  scale_y_continuous('Change in cloud cover fraction') +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  ggtitle('Cloud fraction cover difference following afforestation\n as estimated using different methods')

# ggsave(paste0('S4TvsRAM_bars',data.tag,'.png'),
#        path = 'tempFigures/', plot = g_bars, width = 7, height = 8)



g_scat <- ggplot(df_CZ5 %>%
                   filter(PFT == iPFT) %>%
                   dplyr::select(-Number_of_bins) %>%
                   tidyr::pivot_wider(names_from = 'method',
                                      values_from = c('dCFC_CZ5','dCFC_CZ5_STD_err'))) +
  geom_errorbar(aes(x = dCFC_CZ5_S4T, 
                    ymin = dCFC_CZ5_RAM - dCFC_CZ5_STD_err_RAM,
                    ymax = dCFC_CZ5_RAM + dCFC_CZ5_STD_err_RAM,
                    colour = month)) +
  geom_errorbarh(aes(y = dCFC_CZ5_RAM, 
                     xmin = dCFC_CZ5_S4T - dCFC_CZ5_STD_err_S4T,
                     xmax = dCFC_CZ5_S4T + dCFC_CZ5_STD_err_S4T,
                     colour = month)) +
  geom_point(aes(x = dCFC_CZ5_S4T, y = dCFC_CZ5_RAM, colour = month, 
                 shape = region), 
             fill = 'white', size = 2) +
  geom_abline(colour = 'grey40', size = 0.5) + 
  coord_equal(ylim = c(-0.07,0.05), xlim = c(-0.07,0.05)) + 
  scale_color_viridis_d('Month:', option = 'D') +
  scale_shape_manual('Climate zone:', 
                     values = c('Tropical' = 21, 'Arid' = 22, 
                                'Temperate' = 23, 'Boreal' = 24)) +
  theme(legend.position = 'bottom') +
  ggtitle('Cloud fraction cover difference following afforestation\nas estimated using different methods') +
  guides(colour = guide_legend(nrow = 4, byrow = TRUE,
                               title.position = "top", title.hjust = 0.5),
         shape = guide_legend(nrow = 4, byrow = TRUE,
                              title.position = "top", title.hjust = 0.5))

# 
# ggsave(paste0('S4TvsRAM_scatter',data.tag,'.png'),
#        path = 'tempFigures/', plot = g_scatter, width = 6, height = 6)
# 



# printing the final plot -----
fig.name <- 'figSM___RAM-vs-S4T'
fig.width <- 12; fig.height <- 9;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

print(g_bars, vp = viewport(width = 0.5, height = 1.0, x = 0.0, y = 0.0, just = c(0,0)))
print(g_scat, vp = viewport(width = 0.5, height = 1.0, x = 0.5, y = 0.0, just = c(0,0)))
dev.off()
