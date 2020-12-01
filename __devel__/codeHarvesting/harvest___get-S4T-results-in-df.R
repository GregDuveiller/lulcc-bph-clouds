#### Prepare dataset for use for figures ####

library(raster)
library(ncdf4)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)



dir.create('dataFigures', recursive = T, showWarnings = F)

# dummy raster to aggregate the output data in
rs_dummy <- raster(nrows = 180, ncols = 360, xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   crs = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'), vals = NULL)


# get fractions of each forest type per pixel
lcvr.file <- 'dataInput/ESACCI-LC-L4-LCCS-Map-300m-P1Y-aggregated-0.050000Deg-2015-v2.0.7.nc'
DFO.frct <- raster(lcvr.file, varname = 'DFO')
EFO.frct <- raster(lcvr.file, varname = 'EFO')
# ensure NAs are 0 instead
DFO.frct[is.na(DFO.frct)] <- 0
EFO.frct[is.na(EFO.frct)] <- 0
# ensure we add to 1 exactly
FOR.frct <- round(DFO.frct + EFO.frct, digits = 5)


# filtering thresholds
thr.mnqtl <- 0.005
thr.mxqtl <- 0.995
thr.sigma <- 0.1
thr.numfr <- 0.2

# MOD05 S4T result ----

# locate NetCDF file
S4T_file2harvest <- 'dataResults/Results_from_Federico/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-PM-fv2.0_cfc_all_2004_2014_monthly_avg_climatology_topography_masked_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc'

# load data as raster bricks
rs_DFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 2)
rs_DFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 1)
rs_EFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 2)

# make a dummy for later use
rs_dum05 <- raster(rs_DFO_delta)

# MOD05 for deciduous forests (DFO) ----

# remove values with too much uncertainty
rs_DFO_delta[rs_DFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_DFO_delta[rs_DFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_DFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_DFO_delta[rs_DFO_delta < qtls[1]] <- NA
rs_DFO_delta[rs_DFO_delta > qtls[2]] <- NA

# aggregate tp 1dd
rs_DFO_delta_1dd <- raster::resample(x = rs_DFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MOD05_DFO <- as.data.frame(rs_DFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_DFO', file = 'dataFigures/df_dCFC_MOD05_DFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MOD05_DFO_1dd <- as.data.frame(rs_DFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_DFO_1dd', file = 'dataFigures/df_dCFC_MOD05_DFO_1dd.Rdata')


# MOD05 for evergreen forests (EFO) ----

# remove values with too much uncertainty
rs_EFO_delta[rs_EFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_EFO_delta[rs_EFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_EFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_EFO_delta[rs_EFO_delta < qtls[1]] <- NA
rs_EFO_delta[rs_EFO_delta > qtls[2]] <- NA

# aggregate to 1dd
rs_EFO_delta_1dd <- raster::resample(x = rs_EFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MOD05_EFO <- as.data.frame(rs_EFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_EFO', file = 'dataFigures/df_dCFC_MOD05_EFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MOD05_EFO_1dd <- as.data.frame(rs_EFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_EFO_1dd', file = 'dataFigures/df_dCFC_MOD05_EFO_1dd.Rdata')


# MOD05 for both forest types combined (FOR) ----


# get fraction of forest, and resample 
DFO.frcw <- resample(DFO.frct/FOR.frct, rs_DFO_delta)
EFO.frcw <- resample(EFO.frct/FOR.frct, rs_EFO_delta)

# Combine both forest types in post-processing (already filtered)
rs_FOR_delta <- mosaic(x = rs_DFO_delta * DFO.frcw, 
                       y = rs_EFO_delta * EFO.frcw, fun = sum)
msk <- is.na(rs_DFO_delta) & is.na(rs_EFO_delta)
rs_FOR_delta <- mask(rs_FOR_delta, msk, maskvalue = T, updatevalue = NA)

# reset names
names(rs_FOR_delta) <- names(rs_DFO_delta)

# aggregate
rs_FOR_delta_1dd <- raster::resample(x = rs_FOR_delta, y = rs_dummy, method = 'bilinear')

# export raster
writeRaster(x = rs_FOR_delta, filename = "dataFigures/rstr_dCFC_MOD05_FOR.nc", 
            format = "CDF", overwrite = TRUE)

# export df at finer resolution
df_dCFC_MOD05_FOR <- as.data.frame(rs_FOR_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_FOR', file = 'dataFigures/df_dCFC_MOD05_FOR.Rdata')

# export df at lower resolution
df_dCFC_MOD05_FOR_1dd <- as.data.frame(rs_FOR_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD05_FOR_1dd', file = 'dataFigures/df_dCFC_MOD05_FOR_1dd.Rdata')



# MODo2 S4T result ----
# (Variant with MODIS at 0.02 dd but over a larger window like MOD05)

# load S4T dataset
S4T_file2harvest <- 'dataResults/Results_from_Federico/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_masked_winsize17_0.02dd_s4t_subset_masked_aggregated_0.34deg.nc'

# load data as raster bricks
rs_DFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 2)
rs_DFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 1)
rs_EFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 2)


# MODo2 for deciduous forests (DFO) ----

# remove values with too much uncertainty
rs_DFO_delta[rs_DFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_DFO_delta[rs_DFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_DFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_DFO_delta[rs_DFO_delta < qtls[1]] <- NA
rs_DFO_delta[rs_DFO_delta > qtls[2]] <- NA

# aggregate tp 1dd
rs_DFO_delta_1dd <- raster::resample(x = rs_DFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MODo2_DFO <- as.data.frame(rs_DFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_DFO', file = 'dataFigures/df_dCFC_MODo2_DFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MODo2_DFO_1dd <- as.data.frame(rs_DFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_DFO_1dd', file = 'dataFigures/df_dCFC_MODo2_DFO_1dd.Rdata')


# MODo2 for evergreen forests (EFO) ----

# remove values with too much uncertainty
rs_EFO_delta[rs_EFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_EFO_delta[rs_EFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_EFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_EFO_delta[rs_EFO_delta < qtls[1]] <- NA
rs_EFO_delta[rs_EFO_delta > qtls[2]] <- NA

# aggregate tp 1dd
rs_EFO_delta_1dd <- raster::resample(x = rs_EFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MODo2_EFO <- as.data.frame(rs_EFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_EFO', file = 'dataFigures/df_dCFC_MODo2_EFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MODo2_EFO_1dd <- as.data.frame(rs_EFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_EFO_1dd', file = 'dataFigures/df_dCFC_MODo2_EFO_1dd.Rdata')


# MODo2 for both forest types combined (FOR) ----

# get fraction of forest, and resample 
DFO.frcw <- resample(DFO.frct/FOR.frct, rs_DFO_delta)
EFO.frcw <- resample(EFO.frct/FOR.frct, rs_EFO_delta)

# Combine both forest types in post-processing (using frcw from MOD05)
rs_FOR_delta <- mosaic(x = rs_DFO_delta * DFO.frcw, 
                       y = rs_EFO_delta * EFO.frcw, fun = sum)
msk <- is.na(rs_DFO_delta) & is.na(rs_EFO_delta)
rs_FOR_delta <- mask(rs_FOR_delta, msk, maskvalue = T, updatevalue = NA)

# reset names
names(rs_FOR_delta) <- names(rs_DFO_delta)

# aggregate
rs_FOR_delta_1dd <- raster::resample(x = rs_FOR_delta, y = rs_dummy, method = 'bilinear')

# export df at finer resolution
df_dCFC_MODo2_FOR <- as.data.frame(rs_FOR_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_FOR', file = 'dataFigures/df_dCFC_MODo2_FOR.Rdata')

# export df at lower resolution
df_dCFC_MODo2_FOR_1dd <- as.data.frame(rs_FOR_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MODo2_FOR_1dd', file = 'dataFigures/df_dCFC_MODo2_FOR_1dd.Rdata')






# COMET S4T result ----
S4T_file2harvest <- 'dataResults/Results_from_Federico/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc'
rs_DFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 2)
rs_DFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 1)
rs_EFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 2)


# COMET for deciduous forests (DFO) ----


# remove values with too much uncertainty
rs_DFO_delta[rs_DFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_DFO_delta[rs_DFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_DFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_DFO_delta[rs_DFO_delta < qtls[1]] <- NA
rs_DFO_delta[rs_DFO_delta > qtls[2]] <- NA

# aggregate to 1 degree for simpler figures/analyses 
rs_DFO_delta_1dd <- raster::resample(x = rs_DFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_COMET_DFO <- as.data.frame(rs_DFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = lubridate::as_datetime(Z),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-Z, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_COMET_DFO', file = 'dataFigures/df_dCFC_COMET_DFO.Rdata')

# filter and export df at low resolution
df_dCFC_COMET_DFO_1dd <- as.data.frame(rs_DFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = as_datetime(as.numeric(substring(layer, 2, 11))),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-layer, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))

save('df_dCFC_COMET_DFO_1dd', file = 'dataFigures/df_dCFC_COMET_DFO_1dd.Rdata')



# COMET for evergreen forests (EFO) ----


# remove values with too much uncertainty
rs_EFO_delta[rs_EFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_EFO_delta[rs_EFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_EFO_delta), probs = c(thr.mnqtl, thr.mxqtl), na.rm = T)
rs_EFO_delta[rs_EFO_delta < qtls[1]] <- NA
rs_EFO_delta[rs_EFO_delta > qtls[2]] <- NA

# aggregate to 1 degree for simpler figures/analyses 
rs_EFO_delta_1dd <- raster::resample(x = rs_EFO_delta, y = rs_dummy, method = 'bilinear')

# export df at high resolution
df_dCFC_COMET_EFO <- as.data.frame(rs_EFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = lubridate::as_datetime(Z),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-Z, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_COMET_EFO', file = 'dataFigures/df_dCFC_COMET_EFO.Rdata')

# filter and export df at low resolution
df_dCFC_COMET_EFO_1dd <- as.data.frame(rs_EFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = as_datetime(as.numeric(substring(layer, 2, 11))),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-layer, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_COMET_EFO_1dd', file = 'dataFigures/df_dCFC_COMET_EFO_1dd.Rdata')

# COMET for both forest types combined (FOR) ----

# get fraction of forest, and resample 
DFO.frcw <- resample(DFO.frct/FOR.frct, rs_DFO_delta)
EFO.frcw <- resample(EFO.frct/FOR.frct, rs_EFO_delta)

# Combine both forest types in post-processing (using frcw from MOD05)
rs_FOR_delta <- mosaic(x = rs_DFO_delta * DFO.frcw, 
                       y = rs_EFO_delta * EFO.frcw, fun = sum)
msk <- is.na(rs_DFO_delta) & is.na(rs_EFO_delta)
rs_FOR_delta <- mask(rs_FOR_delta, msk, maskvalue = T, updatevalue = NA)

# reset names
names(rs_FOR_delta) <- names(rs_DFO_delta)

# aggregate
rs_FOR_delta_1dd <- raster::resample(x = rs_FOR_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at finer resolution
df_dCFC_COMET_FOR <- as.data.frame(rs_FOR_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = as_datetime(as.numeric(substring(layer, 2, 11))),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-layer, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_COMET_FOR', file = 'dataFigures/df_dCFC_COMET_FOR.Rdata')

# filter and export df at lower resolution
df_dCFC_COMET_FOR_1dd <- as.data.frame(rs_FOR_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(date_tag = as_datetime(as.numeric(substring(layer, 2, 11))),
                month = factor(format(date_tag, '%b'), levels = month.abb),
                hour = factor(format(date_tag, '%H'))) %>%
  dplyr::select(-layer, -date_tag) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_COMET_FOR_1dd', file = 'dataFigures/df_dCFC_COMET_FOR_1dd.Rdata')





# MOD02 S4T result ----

# load S4T dataset
S4T_file2harvest <- 'dataResults/Results_from_Federico/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_masked_winsize7_0.02dd_s4t_subset_masked_aggregated_0.14deg.nc'

# load data as raster bricks
rs_DFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 1)
rs_EFO_delta <- brick(x = S4T_file2harvest, varname = 'delta', lvar = 3, level = 2)
rs_DFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 1)
rs_EFO_sigma <- brick(x = S4T_file2harvest, varname = 'uncertainty', lvar = 3, level = 2)
rs_DFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 1)
rs_EFO_numfr <- brick(x = S4T_file2harvest, varname = 'fraction', lvar = 3, level = 2)


# MOD02 for deciduous forests (DFO) ----

# remove values with too much uncertainty
rs_DFO_delta[rs_DFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_DFO_delta[rs_DFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_DFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_DFO_delta[rs_DFO_delta < qtls[1]] <- NA
rs_DFO_delta[rs_DFO_delta > qtls[2]] <- NA

# aggregate tp 1dd
rs_DFO_delta_1dd <- raster::resample(x = rs_DFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MOD02_DFO <- as.data.frame(rs_DFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_DFO', file = 'dataFigures/df_dCFC_MOD02_DFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MOD02_DFO_1dd <- as.data.frame(rs_DFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_DFO_1dd', file = 'dataFigures/df_dCFC_MOD02_DFO_1dd.Rdata')


# MOD02 for evergreen forests (EFO) ----

# remove values with too much uncertainty
rs_EFO_delta[rs_EFO_sigma > thr.sigma] <- NA

# remove values with too little spatial representativity
rs_EFO_delta[rs_EFO_numfr < thr.numfr] <- NA

# remove extreme values (which are probably unrealistic)
qtls <- quantile(as.vector(rs_EFO_delta),probs = c(thr.mnqtl,thr.mxqtl), na.rm = T)
rs_EFO_delta[rs_EFO_delta < qtls[1]] <- NA
rs_EFO_delta[rs_EFO_delta > qtls[2]] <- NA

# aggregate tp 1dd
rs_EFO_delta_1dd <- raster::resample(x = rs_EFO_delta, y = rs_dummy, method = 'bilinear')

# filter and export df at high resolution
df_dCFC_MOD02_EFO <- as.data.frame(rs_EFO_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(Z, '%b'), levels = month.abb)) %>%
  dplyr::select(-Z) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_EFO', file = 'dataFigures/df_dCFC_MOD02_EFO.Rdata')

# filter and export df at lower resolution
df_dCFC_MOD02_EFO_1dd <- as.data.frame(rs_EFO_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_EFO_1dd', file = 'dataFigures/df_dCFC_MOD02_EFO_1dd.Rdata')


# MOD02 for both forest types combined (FOR) ----

# get fraction of forest, and resample 
DFO.frcw <- resample(DFO.frct/FOR.frct, rs_DFO_delta)
EFO.frcw <- resample(EFO.frct/FOR.frct, rs_EFO_delta)

# Combine both forest types in post-processing
rs_FOR_delta <- mosaic(x = rs_DFO_delta * DFO.frcw, 
                       y = rs_EFO_delta * EFO.frcw, fun = sum)
msk <- is.na(rs_DFO_delta) & is.na(rs_EFO_delta)
rs_FOR_delta <- mask(rs_FOR_delta, msk, maskvalue = T, updatevalue = NA)

# reset names
names(rs_FOR_delta) <- names(rs_DFO_delta)

# aggregate to 1 dd
rs_FOR_delta_1dd <- raster::resample(x = rs_FOR_delta, y = rs_dummy, method = 'bilinear')
# aggregate to MOD05 resolution
rs_FOR_delta_agr <- raster::resample(x = rs_FOR_delta, y = rs_dum05, method = 'bilinear')

# export raster
writeRaster(x = rs_FOR_delta, filename = "dataFigures/rstr_dCFC_MOD02_FOR.nc", 
            format = "CDF", overwrite = TRUE)

# export df at finer resolution
df_dCFC_MOD02_FOR <- as.data.frame(rs_FOR_delta, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_FOR', file = 'dataFigures/df_dCFC_MOD02_FOR.Rdata')

# export df at intermeadiate resolution
df_dCFC_MOD02_FOR_agr <- as.data.frame(rs_FOR_delta_agr, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_FOR_agr', file = 'dataFigures/df_dCFC_MOD02_FOR_agr.Rdata')


# export df at lower resolution
df_dCFC_MOD02_FOR_1dd <- as.data.frame(rs_FOR_delta_1dd, xy = T, long = T) %>% 
  dplyr::rename(lon = x, lat = y, dCFC = value) %>%
  dplyr::mutate(month = factor(format(as.Date(layer, format = 'X%Y.%m.%d'), '%b'), 
                               levels = month.abb)) %>%
  dplyr::select(-layer) %>%
  dplyr::filter(!is.na(dCFC))
save('df_dCFC_MOD02_FOR_1dd', file = 'dataFigures/df_dCFC_MOD02_FOR_1dd.Rdata')


