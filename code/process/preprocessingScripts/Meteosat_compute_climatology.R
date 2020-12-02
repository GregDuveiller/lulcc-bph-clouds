#!/usr/bin/Rscript
# Title         : Meteosat_compute_climatology.R
# Description   : Compute hour climatology from METEOSAT data
# Date          : Nov 2018
# Version       : 1.0
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# load required libraries
library(ncdf4)
library(raster)

# set input and output files
input_file <- "/space/filipfe/cloud/METEOSAT/input/COMET_MMDC_October_2000_2015.nc"
output_file <- "/space/filipfe/cloud/METEOSAT/stacks/COMET_CFC_MMDC_hour_climatology_land_2004_2014_10.nc"

# optionally set water mask
mask_water_pixels <- TRUE
mask_file <- "/space/filipfe/cloud/METEOSAT/s4t_input/WAT_mask_resize.nc"

# read data
nc_in <- nc_open(input_file, write=F, readunlim = T)

# extract time information
time <- nc_in$dim$time$vals
time_val <- as.POSIXct(time, origin="1970-01-01")

# define temporal range of interest (from 2004 to 2014)
#tRange <- 97:360
start_date <- as.integer(as.Date("2004-01-01", origin="1970-01-01"))*86400
end_date <- as.integer(as.Date("2015-01-01", origin="1970-01-01"))*86400
tRange <- which(time_val >= start_date & time_val < end_date)
#nYears <- as.integer(strftime(as.POSIXct(time[tRange[length(tRange)]], origin = "1970-01-01"), format = "%Y")) - as.integer(strftime(as.POSIXct(time[tRange[1]], origin = "1970-01-01"), format = "%Y")) + 1

# get day hour
tHour <- as.integer(strftime(time_val, format="%H"))

# create empty raster stack
rast_void <- raster(stack(input_file)[[1]])
values(rast_void) <- NA

rast <- stack(replicate(24, rast_void))
names(rast) <- validNames(paste("T", sprintf("%02i", 0:23), sep=""))

if(mask_water_pixels){
  water_mask <- raster(mask_file)
  water_pixel <- which(as.vector(getValues(water_mask)) == 0)
}

# compute hourly average
for(t in 0:23){
  message(paste("Processing time: ", t, sep=""))
  time_step <- which(tHour == t)[which(tHour == t) %in% tRange]
  n <- length(time_step)
  rStack <- stack(replicate(n, rast_void))
  #rStack <- stack(lapply(1:n, function(i) rast_void)) 
  for(l in time_step){
    p <- which(time_step == l)
    rStack[[p]] <- as.vector(rev(apply(ncvar_get(nc_in, varid = "CFC", start = c(1,1,l), count=c(-1,-1,1)), 2, rev)))
  }
  mean_rast <- raster::mean(rStack, na.rm=TRUE)
  
  if(mask_water_pixels){
    # mask out water pixels
    mean_rast[water_pixel] <- NA
  }
  
  rast[[t+1]] <- getValues(mean_rast)
  rm(rStack, mean_rast)
}

# multiply by scale_factor
rast <- rast * 100
names(rast) <- validNames(paste("T", sprintf("%02i", 0:23), sep=""))

# # compute anomalies
# meanDaily <- mean(rast, na.rm=TRUE)
# anomalyRast <- stack(replicate(24, rast_void))
# for(l in 1:nlayers(anomalyRast)){
#   anomalyRast[[l]] <- rast[[l]] - meanDaily
# }

# ################
# save data to NetCDF file


# get dimensions position
nc_lon_pos <- which(names(nc_in$dim) %in% c("lon","Lon","longitude","Longitude"))[1]
nc_lat_pos <- which(names(nc_in$dim) %in% c("lat","Lat","latitude","Latitude"))[1]
nc_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]

# get dimension values
nc_lon_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lon_pos])
nc_lat_dim <- sort(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lat_pos]), decreasing=TRUE)
nc_time_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_time_pos])
nc_time_dim <- nc_time_dim[tRange[1:24]]

# get dimension units
nc_lon_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "units")$value
nc_lat_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "units")$value
nc_time_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "units")$value

# get dimension longnames
nc_lon_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "long_name")$value
nc_lat_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "long_name")$value
nc_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "long_name")$value

# define fill value
nc_deflate <- 9
ncout_fill_value <- -32768

# define new dimensions
ncout_londim <- ncdf4::ncdim_def(name="lon", longname=nc_lon_lname, units=nc_lon_units, vals=as.double(nc_lon_dim), unlim=FALSE)
ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=nc_lat_lname, units=nc_lat_units, vals=as.double(sort(nc_lat_dim, decreasing=TRUE)), unlim=FALSE)
ncout_timedim <- ncdf4::ncdim_def(name="time", longname=nc_time_lname, units=nc_time_units, vals=as.double(nc_time_dim), unlim=TRUE)
# define variables
nc_var_crs <- ncdf4::ncvar_def(name="crs", longname="CRS definition", units="dl", prec="char", dim=list())
nc_var_sif <- ncdf4::ncvar_def(name="CFC", longname="Cloud Fractional Cover", units="dl", missval=ncout_fill_value, prec="short", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
# create list of variables for NetCDF-4 creation
nc_var_list <- list(nc_var_sif, nc_var_crs)

# create output NetCDF-4 file
ncout <- ncdf4::nc_create(output_file, vars=nc_var_list, force_v4=TRUE)

# write results to NetCDF file
ncdf4::ncvar_put(ncout, varid="CFC", vals=as.integer(raster::getValues(rast)), start=c(1,1,1), count=c(-1,-1,-1))

# put additional attributes into dimension and data variables
ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
ncdf4::ncatt_put(ncout,"lon","axis","X")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"lat","axis","Y")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"time","standard_name","time")
ncdf4::ncatt_put(ncout,"time","calendar","standard")
ncdf4::ncatt_put(ncout, nc_var_sif, "add_offset", 0.0, prec="float")
ncdf4::ncatt_put(ncout, nc_var_sif, "scale_factor", 0.0001, prec="float")
ncdf4::ncatt_put(ncout, nc_var_sif, "grid_mapping", "crs")

# get CRS information
raster::projection(rast) <- sp::CRS("+init=epsg:4326") ### fix
p4s <- sp::proj4string(rast)
epsg_code <- as.integer(rgdal::showEPSG(p4s))
wkt_def <- rgdal::showWKT(p4s)
crs_def <- as.character(p4s)

# set attributed for 'crs' variable
if(epsg_code == 4326){
  ncdf4::ncatt_put(ncout,"crs","grid_mapping_name","latitude_longitude")
  ncdf4::ncatt_put(ncout,"crs","longitude_of_prime_meridian", 0.0, prec="float")
  ncdf4::ncatt_put(ncout,"crs","semi_major_axis", 6378137, prec="float")
  ncdf4::ncatt_put(ncout,"crs","inverse_flattening", 298.257223563, prec="float")
}
ncdf4::ncatt_put(ncout,"crs","projection", crs_def)
ncdf4::ncatt_put(ncout,"crs","proj4_params", crs_def)
ncdf4::ncatt_put(ncout,"crs","EPSG_code", epsg_code, prec="short")
ncdf4::ncatt_put(ncout,"crs","spatial_ref", wkt_def)

# set global attributes to output NetCDF-4 file
ncdf4::ncatt_put(ncout,0,"title","CFC")
ncdf4::ncatt_put(ncout,0,"summary", "This file contains time-space aggregated Thematic Climate Data Records (TCDR) produced by geosatclim within the Satellite Application Facility on Climate Monitoring (CM SAF)")
ncdf4::ncatt_put(ncout,0,"cdm_data_type","Grid")
ncdf4::ncatt_put(ncout,0,"grid_type","GeoX GeoY")

nc_close(ncout)

rm(list=ls())
gc()

# clip Land Cover to CFC extent using NCO
# ncks -d lon,-65.,65. -d lat,-65.,65. /space/filipfe/Space4Time/cloud/input/s4t_global/ESACCI-LC-L4-LCCS-Map-300m-P1Y-aggregated-0.050000Deg-2004-v2.0.7_resize.nc /space/filipfe/cloud/METEOSAT/ESACCI-LC-L4-LCCS-Map-300m-P1Y-aggregated-0.050000Deg-2004-v2.0.7_METEOSAT_resize.nc
# ncks -d longitude,-65.,65. -d latitude,-65.,65. /space/filipfe/Space4Time/cloud/input/s4t_global/DEM_Global_0.05dd_elevation_mask.nc /space/filipfe/cloud/METEOSAT/S4T_mask_DEM_0.05dd_METEOSAT_resize.nc
