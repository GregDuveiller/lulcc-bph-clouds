
require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)



# set some general graphical parameters -----
col.pal <-  RColorBrewer::brewer.pal(9,'RdBu')
landColor <- 'grey60'
seaColor <- 'grey20'

zlims <- c(-0.07, 0.07)
zlims.num <- c(0, 800)
  
  
xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)


# set the parameters to select the target points -----

i.thr.dist.max <- 100  # 60 80 100
i.thr.dist.min <- 30  # 20 30 40
i.thr.dfor.min <- 0   # 0.0 0.1 0.2
i.thr.nyrs.min <- 7   # 5 7 9


# Get the SYNOP data ready -----

load('dataFigures/df_SYNOP_agr_4polarplots_eur.RData') # df_SYNOP_agr

# rename for convenience
df_SYNOP_agr <- df_SYNOP_agr %>%
  rename(MinDist = thr.dist.min,
         MaxDist = thr.dist.max,
         MinYears = thr.nyrs.min,
         MinDfor = thr.dfor.min)


# SYNOP wheel dCFC for MinDist~MaxDist -----

big.title <- 'Effect of change in forest cover on cloud cover based on SYNOP'
sub.title <-  paste0(' MinYears: ', i.thr.nyrs.min, 'yrs',
                     ' | MinDfor: ', 100 * i.thr.dfor.min, '%')



g.synop.wheels.distvar <- ggplot(df_SYNOP_agr %>%
                                   filter(MinYears == i.thr.nyrs.min,
                                          MinDfor == i.thr.dfor.min), 
                                 aes(x = hour, y = month)) +
  geom_tile(aes(fill = dCFC)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_gradientn('Change in cloud fraction cover',
                       colors = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = zlims, oob = scales::squish) +
  coord_polar() +
  facet_grid(MinDist~MaxDist, labeller = label_both) +
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



# printing the plot 
fig.name <- 'figSM___synop-distvar-delta-CFC'
fig.width <- 10; fig.height <- 11;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname, 
       plot = g.synop.wheels.distvar, width = fig.width, 
       height = fig.height)


# SYNOP wheel NUM for MinDist~MaxDist -----

big.title <- 'Available observations from SYNOP pairs'
sub.title <-  paste0(' MinYears: ', i.thr.nyrs.min, 'yrs',
                     ' | MinDfor: ', 100 * i.thr.dfor.min, '%')



g.synop.wheels.distvar <- ggplot(df_SYNOP_agr %>%
                                   filter(MinYears == i.thr.nyrs.min,
                                          MinDfor == i.thr.dfor.min), 
                                 aes(x = hour, y = month)) +
  geom_tile(aes(fill = n)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_viridis_c('Available SYNOP measures', limits = zlims.num,
                       oob = scales::squish) +
  coord_polar() +
  facet_grid(MinDist~MaxDist, labeller = label_both) +
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))



# printing the plot 
fig.name <- 'figSM___synop-distvar-NUM'
fig.width <- 10; fig.height <- 11;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname, 
       plot = g.synop.wheels.distvar, width = fig.width, 
       height = fig.height)




# SYNOP wheel dCFC for MinDfor~MinDfor -----

big.title <- 'Effect of change in forest cover on cloud cover based on SYNOP'
sub.title <-  paste0(' MinDist: ', i.thr.dist.min, 'km',
                     ' | MaxDist: ', i.thr.dist.max, 'km')

g.synop.wheels.yrdfor <- ggplot(df_SYNOP_agr %>%
                                   filter(MinDist == i.thr.dist.min,
                                          MaxDist == i.thr.dist.max), 
                                 aes(x = hour, y = month)) +
  geom_tile(aes(fill = dCFC)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_gradientn('Change in cloud fraction cover',
                       colors = RColorBrewer::brewer.pal(9, 'RdBu'),
                       limits = zlims, oob = scales::squish) +
  coord_polar() +
  facet_grid(MinDfor~MinYears, labeller = label_both) +
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# printing the plot 
fig.name <- 'figSM___synop-yrdfor-delta-CFC'
fig.width <- 10; fig.height <- 11;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname, 
       plot = g.synop.wheels.yrdfor, width = fig.width, 
       height = fig.height)


# SYNOP wheel NUM for MinDfor~MinDfor -----

big.title <- 'Available observations from SYNOP pairs'
sub.title <-  paste0(' MinDist: ', i.thr.dist.min, 'km',
                     ' | MaxDist: ', i.thr.dist.max, 'km')

g.synop.wheels.yrdfor <- ggplot(df_SYNOP_agr %>%
                                  filter(MinDist == i.thr.dist.min,
                                         MaxDist == i.thr.dist.max), 
                                aes(x = hour, y = month)) +
  geom_tile(aes(fill = n)) +
  geom_point(aes(alpha = val.signif)) +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_fill_viridis_c('Available SYNOP measures', limits = zlims.num,
                       oob = scales::squish) +
  coord_polar() +
  facet_grid(MinDfor~MinYears, labeller = label_both) +
  ggtitle(label = big.title, subtitle = sub.title) + 
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.y = element_text()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


# printing the plot 
fig.name <- 'figSM___synop-yrdfor-NUM'
fig.width <- 10; fig.height <- 11;  # fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, fig.name, '.', fig.fmt)

ggsave(filename = fig.fullfname, 
       plot = g.synop.wheels.yrdfor, width = fig.width, 
       height = fig.height)


