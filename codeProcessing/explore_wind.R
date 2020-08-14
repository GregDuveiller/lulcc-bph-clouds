library(ncdf4)
library(raster)



r_ua100 <- brick('dataInput/ERA5_wind.nc', varname = 'ua100')
r_va100 <- brick('dataInput/ERA5_wind.nc', varname = 'va100')
