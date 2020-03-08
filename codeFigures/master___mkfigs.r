##### MASTER SCRIPT FOR MAKING PLOTS -----


library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(scales)
library(RColorBrewer)
library(here)

## Set paths ----

fig.path <- 'textFigures/'
dir.create(fig.path, recursive = T, showWarnings = F)

fig.fmt <- 'png'
#fig.fmt <- 'pdf'

## Set some common graphical parameters ---- 

# TO BE SEEN IF NEEDED >>>


# in Work UNIX
if(Sys.info()['sysname'] == 'Linux') {
  vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
}
# in Home OSX:
if(Sys.info()['sysname'] == 'Darwin'){
  vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'
}


# Set format for figures 
fig.fmt <- 'png' # fig.fmt <- 'pdf'
dir.create(paste0('textFigures/', fig.fmt), showWarnings = F, recursive = T)
col.pal <- rev(c('#2B3677', '#327FBB', '#A2D5FF', '#F7F7F7', '#FFD181' ,'#EA965A', '#9C4D0C'))


## Global picture: map of 4 seasons
source('codeFigures/fig___map-delta-CFC.r')

## SYNOP confrontation over Europe
source('codeFigures/fig___SYNOP.r')

## Exploring effects of different PFTs across Europe
source('codeFigures/fig___map-PFT-effect.r')

## Delta-3 plots
source('codeFigures/fig___multi-delta-plots.r')

## Maps of scale effect over Europe
source('codeFigures/figSM___map-scaling-Europe.r')

## Confronting methodologies
source('codeFigures/figSM___RAM-vs-S4T.r')

## Extra SYNOP confrontations
source('codeFigures/figSM___SYNOP.r')
