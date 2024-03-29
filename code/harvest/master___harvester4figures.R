#!/usr/local/bin/Rscript
################################################################################
# Purpose: Master script for harvesting all data necessary to make the figures 
# License: GPL v3
# Authors: Gregory Duveiller - Dec. 2020
################################################################################

results_path <- 'data/final_data/study_results' 
harvest_path <- 'data/final_data/data_for_figures' 
dir.create(harvest_path, recursive = T, showWarnings = F)

source('code/harvest/harvest___get-S4T-results-in-df.R')
source('code/harvest/harvest___get-SYNOP-agr-summarized-values.R')
source('code/harvest/harvest___get-RAM-results-w-S4T.R')
source('code/harvest/harvest___get-S4T-for-other-biophyvars.R')
source('code/harvest/harvest___get-CFC-in-df.R')
