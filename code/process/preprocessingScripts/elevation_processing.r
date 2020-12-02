#!/usr/bin/Rscript
# Title         : Elevation processing
# Description   : Compute global or regional elevation dataset and statistics from SRTM and GTOPO DEM data
# Date          : Mar 2018
# Version       : 0.1
# Licence       : GPL v3
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>, Gregory Duveiller
# ########################################################
#### SCRIPT to make Topodata at 5km... as mean and sd outputs globally

# load required libraries
require(raster)
require(ncdf4)

# set input and output folders
input_path <- normalizePath(path="/ESS_EarthObs/DATA_PRODUCTS/Elevation/CGIAR_SRTM/zips", winslash = "/", mustWork = TRUE)
output_path <- normalizePath(path="/space/filipfe_data/cloud/elevation/global_0.02/", winslash = "/", mustWork = FALSE)

# create output folder
dir.create(output_path, showWarnings = F, recursive = T)

# create list of files to be processed
flist <- list.files(path = input_path, pattern = 'srtm_.+.tif$', full.names = T)

# create empty raster objects
rOUT_LR_mu <- raster(nrows=9000, ncols=18000, xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=0.02, vals=NULL)
rOUT_LR_sd <- raster(nrows=9000, ncols=18000, xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=0.02, vals=NULL)

# process files
Sys.time()
for(ifile in flist){
  print(paste0("Processing file: ", ifile))
  # import file
  rDEM <- raster(ifile)
  
  # aggregate pixel to target spatial resolution
  rDEM_LR_mu <- round(aggregate(rDEM, fact = 0.02/res(rDEM), fun = mean, na.rm=TRUE))
  rDEM_LR_sd <- round(aggregate(rDEM, fact = 0.02/res(rDEM), fun = sd, na.rm=TRUE))
  
  # add layer to global rasters
  rOUT_LR_mu[cellsFromExtent(rOUT_LR_mu, extent(rDEM_LR_mu))] <- rDEM_LR_mu
  rOUT_LR_sd[cellsFromExtent(rOUT_LR_sd, extent(rDEM_LR_sd))] <- rDEM_LR_sd
  
  rm(rDEM)
  gc()
}
Sys.time()

print("Writing output files... ")
writeRaster(x = rOUT_LR_mu, datatype='INT2S', filename = paste0(output_path, '/','srtm_global_0.02dd_avg.tif'),format="GTiff", overwrite=TRUE)
writeRaster(x = rOUT_LR_sd, datatype='INT2S', filename = paste0(output_path, '/','srtm_global_0.02dd_stdev.tif'),format="GTiff", overwrite=TRUE)
print("Done! ")

# (use GTOPO30 DEM for latitudes above 60 degrees north)

output_path <- normalizePath(path="/space/filipfe_data/cloud/elevation/europe_0.02/", winslash = "/", mustWork = FALSE)

# import data
input_path_GTOPO30 <- normalizePath(path="/ESS_EarthObs/DATA_PRODUCTS/Elevation/GTOPO30/gtopo30_56N_90N.nc", winslash = "/", mustWork = TRUE)
rDEM <- raster(input_path_GTOPO30, varname='topo')
values(rDEM) <- getValues(rDEM)

# create empty raster objects
rOUT_LR_GTOPO_rast <- raster(nrows=1700, ncols=18000, xmn=-180, xmx=180, ymn=56, ymx=90, resolution=0.02, vals=NULL)
#rOUT_LR_mu_GTOPO <- raster(nrows=1700, ncols=18000, xmn=-180, xmx=180, ymn=56, ymx=90, resolution=0.02, vals=NULL)
#rOUT_LR_sd_GTOPO <- raster(nrows=1700, ncols=18000, xmn=-180, xmx=180, ymn=56, ymx=90, resolution=0.02, vals=NULL)

# aggregate pixel to target spatial resolution
rDEM_LR_mu <- round(aggregate(rDEM, fact = ceiling(0.02/res(rDEM)), fun = mean))
gc()
rDEM_LR_sd <- round(aggregate(rDEM, fact = ceiling(0.02/res(rDEM)), fun = sd))
gc()

# # the following commands fails because fact value is not an integer multiple of rDEM resolution
# # add layer to global rasters
# rOUT_LR_mu_GTOP[cellsFromExtent(rOUT_LR_mu_GTOP, extent(rDEM_LR_mu))] <- rDEM_LR_mu
# gc()
# rOUT_LR_sd_GTOP[cellsFromExtent(rOUT_LR_sd_GTOP, extent(rDEM_LR_sd))] <- rDEM_LR_sd
# gc()

# resample to fit 0.02 resolution
rOUT_LR_mu_GTOPO <- resample(rDEM_LR_mu, rOUT_LR_GTOPO_rast, method="ngb")
rOUT_LR_sd_GTOPO <- resample(rDEM_LR_sd, rOUT_LR_GTOPO_rast, method="ngb")

print("Writing output files... ")
writeRaster(x = rOUT_LR_mu_GTOPO, datatype='INT2S', filename = paste0(output_path, '/', 'gtopo_global_0.05dd_MEAN.tif'),format="GTiff", overwrite=TRUE)
writeRaster(x = rOUT_LR_sd_GTOPO, datatype='INT2S', filename = paste0(output_path, '/', 'gtopo_global_0.05dd_STDV.tif'),format="GTiff", overwrite=TRUE)
print("Done! ")

# merge with SRTM global DEM
rOUT_LR_mu[cellsFromExtent(rOUT_LR_mu, c(-180,180,60,90))] <- rOUT_LR_mu_GTOPO[cellsFromExtent(rOUT_LR_mu_GTOPO, c(-180,180,60,90))]
rOUT_LR_sd[cellsFromExtent(rOUT_LR_sd, c(-180,180,60,90))] <- rOUT_LR_sd_GTOPO[cellsFromExtent(rOUT_LR_sd_GTOPO, c(-180,180,60,90))]

print("Writing output files... ")
writeRaster(x = rOUT_LR_mu, datatype='INT2S', filename = paste0(output_path, '/', 'DEM_global_0.02dd_avg.tif'),format="GTiff", overwrite=TRUE)
writeRaster(x = rOUT_LR_sd, datatype='INT2S', filename = paste0(output_path, '/', 'DEM_global_0.02dd_stdev.tif'),format="GTiff", overwrite=TRUE)
print("Done! ")

#########################
# clip data to Europe

# import reference grid for Europe
europe_reference_grid <- raster("/space/filipfe_data/cloud/aqua_europe/run02/stats/2010_JJA-ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_avg.nc")

# create empty raster
rDEM_Europe_mu <- europe_reference_grid
rDEM_Europe_sd <- europe_reference_grid

# fill in rasters with values
values(rDEM_Europe_mu) <- rOUT_LR_mu[cellsFromExtent(rOUT_LR_mu, extent(rDEM_Europe_mu))]
values(rDEM_Europe_sd) <- rOUT_LR_sd[cellsFromExtent(rOUT_LR_sd, extent(rDEM_Europe_sd))]

# export raster
print("Writing output files... ")
writeRaster(x = rDEM_Europe_mu, datatype='INT2S', filename = paste0(output_path, '/', 'DEM_Europe_0.02dd_avg.tif'),format="GTiff", overwrite=TRUE)
writeRaster(x = rDEM_Europe_sd, datatype='INT2S', filename = paste0(output_path, '/', 'DEM_Europe_0.02dd_stdev.tif'),format="GTiff", overwrite=TRUE)
print("Done! ")
