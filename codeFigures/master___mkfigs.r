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


## Global picture: map of 4 seasons
source('codeFigures/fig___map-delta-CFC.r')


## SYNOP confrontation over Europe
source('codeFigures/fig___SYNOP.r')
source('codeFigures/figSM___SYNOP.r')

## Maps of scale effect over Europe
source('codeFigures/figSM___map-scaling-Europe.r')

## Confronting methodologies
source('codeFigures/figSM___RAM-vs-S4T.r')

## Exploring effects of different PFTs across Europe
source('codeFigures/fig___map-PFT-effect.r')

## Delta-3 plots
source('codeFigures/fig___multi-delta-plots.r')
