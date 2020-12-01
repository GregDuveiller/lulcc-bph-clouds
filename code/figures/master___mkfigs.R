#!/usr/local/bin/Rscript
################################################################################
# Purpose:  Master script for making all plots of the paper 
# License:  GPL v3
# Authors:  Gregory Duveiller - Dec. 2020
################################################################################


library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(scales)
library(RColorBrewer)
library(here)


# define link where to find the 'harvested' data
dat4fig_path <- 'data/final_data/__Zenodo__' 
dat4fig_path <- '__devel__/dataFigures' 

# Set format for figures 
fig.fmt <- 'png'  # fig.fmt <- 'pdf'
fig.path <- 'docs/article_Figures'
dir.create(paste0(fig.path, '/', fig.fmt), showWarnings = F, recursive = T)

# set-up common colour palette
col.pal <- rev(c('#2B3677', '#327FBB', '#A2D5FF', '#F7F7F7', '#FFD181' ,'#EA965A', '#9C4D0C'))
dcfcLims <- c(-0.06,0.06)
landColor <- 'grey70'
seaColor <- 'grey20'


## Global picture: map of 4 seasons
source('code/figures/fig___map-delta-CFC.R')

## Confronting methodologies
source('code/figures/fig___RAM-vs-S4T.R')

## SYNOP confrontation over Europe
source('code/figures/fig___SYNOP.R')

## Exploring effects of different PFTs across Europe
source('code/figures/fig___map-PFT-effect.R')

## Delta-3 plots
source('code/figures/fig___multi-delta-plots.R')

## Global picture: map of 4 seasons in relative terms
source('code/figures/figSM___map-delta-CFC-relative.R')

## Zoom of selected regions
source('code/figures/figSM___map-delta-CFC-zoom.R')

## Extent of forest with positive CFC change
source('code/figures/figSM___extent-forest-with-sign.R')

## Maps of scale effect over Europe
source('codeFigures/figSM___map-scaling-Europe.r')


