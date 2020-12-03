#!/usr/bin/Rscript
# Title         : Geostationary temporal composite
# Description   : Compute time difference from UTC time
# Date          : Dec 2018
# Version       : 0.1
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# set local time to be composited (0-24)
local_time <- 14

### N.B. Input NetCDF file should have only 24 position in time dimension
# read input NetCDF file
input_file <- "/space/filipfe/clouds/s4t_cloud_meteosat/dataResults/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc"
#input_file <- "/space/filipfe/clouds/s4t_cloud_meteosat/dataResults/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked.nc"
nc_in <- ncdf4::nc_open(filename=input_file, write = FALSE, readunlim = FALSE)

# get dimension position
nc_lon_pos <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
nc_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]

# get dimension values
nc_lon_dim <- as.vector(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lon_pos]))
nc_time_dim <- as.vector(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_time_pos]))

# create difference time grid
nc_resolution <- as.double((max(nc_lon_dim) - min(nc_lon_dim)) / (length(nc_lon_dim) - 1))
grid_Dt <- nc_lon_dim / 15.0

# set time
time_index <- local_time - grid_Dt
#hour_index <- round(time_index)
WtB <- abs(time_index - round(time_index))
WtA <- 1 - WtB
tA <- floor(time_index - round(time_index) + 0.5) + round(time_index)
tB <- tA + 1

###
# create composite for 14:00 using Space4Time results
nc_input <- nc_in
output_file <- "/space/filipfe/clouds/s4t_cloud_meteosat/localTime/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked_aggregated_0.35_stack.nc"
#output_file <- "/space/filipfe/clouds/s4t_cloud_meteosat/localTime/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked_stack.nc"
nc_output <- ncdf4::nc_open(filename=output_file, write=T, readunlim = T)

# Process data
for(t in 1:12){
  message(paste("Processing month: ", t, sep=""))
  for(class in 1:2){
    tr <- (24*(t-1))+(1)
    # import data
    data <- ncdf4::ncvar_get(nc_input, varid = "delta", start=c(1,1,class,tr), count=c(-1,-1,1,24))
    data[which(data == -999)] <- NA
    matrice <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
    for(r in 1:nrow(matrice)){
      matrice[r,] <- as.double(((data[r,,tA[r]]*WtA[r]) + (data[r,,tB[r]]*WtB[r])))
    }
    ncdf4::ncvar_put(nc_output, varid = "delta", vals = matrice, start=c(1,1,class,t), count=c(-1,-1,1,1))
  }
}

nc_close(nc_output)
