##### MASTER SCRIPT FOR MAKING PLOTS -----


library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(scales)
library(RColorBrewer)
library(here)




# in Work UNIX
if(Sys.info()['sysname'] == 'Linux') {
  vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
}
# in Home OSX:
if(Sys.info()['sysname'] == 'Darwin'){
  vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'
}


# Set format for figures 
fig.fmt <- 'png' 
fig.fmt <- 'pdf'
fig.path <- 'textFigures/'
dir.create(paste0('textFigures/', fig.fmt), showWarnings = F, recursive = T)
col.pal <- rev(c('#2B3677', '#327FBB', '#A2D5FF', '#F7F7F7', '#FFD181' ,'#EA965A', '#9C4D0C'))
dcfcLims <- c(-0.06,0.06)


## Global picture: map of 4 seasons
source('codeFigures/fig___map-delta-CFC.r')

## SYNOP confrontation over Europe
source('codeFigures/fig___SYNOP.r')

## Exploring effects of different PFTs across Europe
source('codeFigures/fig___map-PFT-effect.r')

## Delta-3 plots
source('codeFigures/fig___multi-delta-plots.r')

## Global picture: map of 4 seasons in relative terms
source('codeFigures/figSM___map-delta-CFC-relative.R')

## Zoom of selected regions
source('codeFigures/figSM___map-delta-CFC-zoom.R')

## Extent of forest with positive CFC change
source('codeFigures/figSM___extent-forest-with-sign.R')

## Maps of scale effect over Europe
source('codeFigures/figSM___map-scaling-Europe.r')

## Confronting methodologies
source('codeFigures/figSM___RAM-vs-S4T.r')

# ## Extra SYNOP confrontations
# source('codeFigures/figSM___SYNOP.r')
