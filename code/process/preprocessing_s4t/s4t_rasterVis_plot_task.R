rm(list=ls())
gc()
print("Processing MODIS vs AVHRR")
# load required libraries
library(rgdal)
library(raster)
library(ncdf4)
library(rasterVis)
library(KernSmooth)
#library(colorRamps)
library(pals)
library(sf)

# define path where to store maps
outmappath <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/plots", winslash = "/", mustWork = T)
# define input file names
input_a <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_0.02dd_s4t_masked.nc", winslash = "/", mustWork = T)
#input_b <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_0.05_s4t_masked.nc", winslash = "/", mustWork = T)
input_b <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3X_CLOUD-JRC-AVHRR_NOAA-PM-fv3.0_cfc_all_monthly_avg_climatology_fix_time_0.05_clip_Europe_winsize_5_s4t_masked.nc", winslash = "/", mustWork = T)

# input_a <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_0.02dd.nc", winslash = "/", mustWork = T)
# input_b <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_0.05.nc", winslash = "/", mustWork = T)
# input_c <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3X_CLOUD-JRC-AVHRR_NOAA-PM-fv3.0_cfc_all_monthly_avg_climatology_fix_time_0.05_clip_Europe.nc", winslash = "/", mustWork = T)
# define names
name_a <- "MODIS 0.02"
#name_b <- "MODIS 0.05"
name_b <- "AVHRR 0.05"

# import vector file for plot overlay
gdam_sf <- sf::st_read("/space/filipfe_data/cloud/space4time/AVHRR_MODIS_differences/gdam28_clean.shp")
gdam <- as(gdam_sf, "Spatial")

# define transition to plot
transit <- c(1,2)
# define transition names
tNames <- c("CaG_DFO","CaG_EFO")

# define months names
mNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")

# create colour palette
#myTheme=rasterTheme(region=coolwarm(100))
myTheme <- rasterTheme(region=brewer.rdbu(100))
#myTheme_diff <- rasterTheme(region=brewer.prgn(100))
#myTheme_diff <- rasterTheme(region=jet(100))
#myTheme_diff <- rasterTheme(region=warmcool(100))
myTheme_diff <- rasterTheme(region=brewer.rdbu(100))
breaks <- seq(-0.2,0.2, length.out = 100)
colKey <- list(space="bottom")
#MyColorkey <- list(at=seq(0, 15, 0.2), labels=list(at=c(0, 5, 7.5, 10, 15), labels=c(0, 5, "g/m^3", 10, ">15"), cex=1.5), space="bottom")
# define plot title
myMain <- list("Resolution comparison", cex=2.0)
# define layout
myLayout <- c(1,3)
# print all scale labels
myScales <- list(draw=TRUE, alternating=F)

# read NetCDF A
nc_a <- ncdf4::nc_open(input_a, readunlim = FALSE)
rast_a <- raster::raster(suppressWarnings(raster::stack(input_a)[[1]]))
rast_a_bkp <- rast_a
# # get values
# values(rast_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,1)))
# names(rast_a) <- name_a
# stack_a <- stack(replicate(3, rast_a))
# values(stack_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,3)))
# names(stack_a) <- c("January","February","March")

# read NetCDF B
nc_b <- ncdf4::nc_open(input_b, readunlim = FALSE)
rast_b <- raster::raster(suppressWarnings(raster::stack(input_b)[[1]]))
# # get values
# values(rast_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,1)))
# names(rast_b) <- name_b
# stack_b <- stack(replicate(3, rast_b))
# values(stack_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,3)))
# names(stack_b) <- c("January","February","March")

################
# create raster representation
# create a raster at 0.01 deg resolution for comparison
rast_a <- rast_a_bkp
rast_aR <- rast_a_bkp
if(sum(round(res(rast_a), digits=2) == round(res(rast_b), digits=2)) != 2){
  res(rast_aR) <- c(0.01,0.01)
}
# create empty raster to store difference
rast_d <- rast_aR
# create empty stacks
stack_a <- stack(replicate(3, rast_a))
stack_b <- stack(replicate(3, rast_b))
stack_d <- stack(replicate(3, rast_d))

# create list of month by year quarters
months_table <- data.frame(Q1=c(1,2,3),Q2=c(4,5,6),Q3=c(7,8,9),Q4=c(10,11,12))

for(tr in transit){
  print(paste("Processing transition ", tNames[tr], sep=""))
  
  for(q in 1:4){
    print(paste("Processing quarter: ", q, sep=""))
    mList <- months_table[,q]
    
    for(n in 1:3){
      m <- mList[n]
      print(paste("Processing month: ", m, sep=""))
      # import data
      values(rast_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,tr,m), count=c(-1,-1,1,1)))
      values(rast_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,tr,m), count=c(-1,-1,1,1)))
      # using vectors
      if(sum(round(res(rast_a), digits=2) == round(res(rast_b), digits=2)) == 2){ ### do not resample if raster have the same resolution
        val_aRes <- as.vector(getValues(rast_a))
        val_bRes <- as.vector(getValues(rast_b))
      } else {
        val_aRes <- as.vector(getValues(raster::resample(rast_a, rast_d, method="ngb")))
        val_bRes <- as.vector(getValues(raster::resample(rast_b, rast_d, method="ngb")))
      }
      # compute difference
      val_dRes <- (val_aRes - val_bRes)
      values(rast_d) <- as.vector(val_dRes)
      # store values in raster stack
      values(stack_a[[n]]) <- as.vector(getValues(rast_a))
      values(stack_b[[n]]) <- as.vector(getValues(rast_b))
      values(stack_d[[n]]) <- as.vector(getValues(rast_d))
    }
    # set layer names
    names(stack_a) <- mNames[mList]
    names(stack_b) <- mNames[mList]
    names(stack_d) <- mNames[mList]
    # save plot to file
    print("Generating plot")
    png_name <- normalizePath(paste(outmappath, "/", "Comparison_MODIS_AVHRR_Q", q, "_", tNames[tr], ".png", sep=""), winslash="/", mustWork=FALSE)
    png(filename=png_name, width=6400, height=5000, units="px", res=300)
    p1 <- levelplot(stack_a, at=breaks, maxpixels=6000000, par.settings=myTheme, main=name_a, xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
    p1 <- p1 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
    p2 <- levelplot(stack_b, at=breaks, maxpixels=6000000, par.settings=myTheme, main=name_b, xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
    p2 <- p2 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
    p3 <- levelplot(stack_d, at=breaks, maxpixels=6000000, par.settings=myTheme_diff, main="Difference", xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
    p3 <- p3 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
    print(p1, split=c(1, 1, 3, 1), more=TRUE)
    print(p2, split=c(2, 1, 3, 1), more=TRUE)
    print(p3, split=c(3, 1, 3, 1))
    dev.off()
    gc()
  }
}

rm(list=ls())
gc()
print("Processing CFC MODIS vs AVHRR")

# load required libraries
library(rgdal)
library(raster)
library(ncdf4)
library(rasterVis)
#library(colorRamps)
library(pals)
library(sf)

# define path where to store maps
outmappath <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/plots", winslash = "/", mustWork = T)
# define input file names
# input_a <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_0.02dd_s4t_masked.nc", winslash = "/", mustWork = T)
# input_b <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_0.05_s4t_masked.nc", winslash = "/", mustWork = T)
# input_c <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/results/ESACCI-L3X_CLOUD-JRC-AVHRR_NOAA-PM-fv3.0_cfc_all_monthly_avg_climatology_fix_time_0.05_clip_Europe_winsize_5_s4t_masked.nc", winslash = "/", mustWork = T)
input_a <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_0.02dd.nc", winslash = "/", mustWork = T)
input_b <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_0.05.nc", winslash = "/", mustWork = T)
input_c <- normalizePath("/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3X_CLOUD-JRC-AVHRR_NOAA-PM-fv3.0_cfc_all_monthly_avg_climatology_fix_time_0.05_clip_Europe.nc", winslash = "/", mustWork = T)
# define names
name_a <- "MODIS 0.02"
name_b <- "AVHRR 0.05"
name_c <- "AVHRR 0.05"

# import vector file for plot overlay
gdam_sf <- sf::st_read("/space/filipfe_data/cloud/space4time/AVHRR_MODIS_differences/gdam28_clean.shp")
gdam <- as(gdam_sf, "Spatial")

# define transition to plot
transit <- c(1,2)
# define transition names
tNames <- c("CaG_DFO","CaG_EFO")

# define months names
mNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")

# create colour palette
#myTheme=rasterTheme(region=coolwarm(100))
myTheme <- rasterTheme(region=jet(100))
myTheme_diff <- rasterTheme(region=brewer.rdbu(100))
breaks <- seq(0.0,1.0, length.out = 100)
breaks_diff <- seq(-0.2,0.2, length.out = 100)
colKey <- list(space="bottom")
#MyColorkey <- list(at=seq(0, 15, 0.2), labels=list(at=c(0, 5, 7.5, 10, 15), labels=c(0, 5, "g/m^3", 10, ">15"), cex=1.5), space="bottom")
# define plot title
myMain <- list("Resolution comparison", cex=2.0)
# define layout
myLayout <- c(1,3)
# print all scale labels
myScales <- list(draw=TRUE, alternating=F)

# read NetCDF A
nc_a <- ncdf4::nc_open(input_a, readunlim = FALSE)
rast_a <- raster::raster(suppressWarnings(raster::stack(input_a)[[1]]))
rast_a_bkp <- rast_a
# # get values
# values(rast_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,1)))
# names(rast_a) <- name_a
# stack_a <- stack(replicate(3, rast_a))
# values(stack_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,3)))
# names(stack_a) <- c("January","February","March")

# read NetCDF B
nc_b <- ncdf4::nc_open(input_b, readunlim = FALSE)
rast_b <- raster::raster(suppressWarnings(raster::stack(input_b)[[1]]))
# # get values
# values(rast_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,1)))
# names(rast_b) <- name_b
# stack_b <- stack(replicate(3, rast_b))
# values(stack_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,3)))
# names(stack_b) <- c("January","February","March")

# read NetCDF C
nc_c <- ncdf4::nc_open(input_c, readunlim = FALSE)
rast_c <- raster::raster(suppressWarnings(raster::stack(input_c)[[1]]))
# # get values
# values(rast_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,1)))
# names(rast_b) <- name_b
# stack_b <- stack(replicate(3, rast_b))
# values(stack_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,1,1), count=c(-1,-1,1,3)))
# names(stack_b) <- c("January","February","March")

# create a raster at 0.01 deg resolution
rast_aR <- rast_a
res(rast_aR) <- c(0.01,0.01)

################
# create raster representation
# create a raster at 0.01 deg resolution for comparison
rast_a <- rast_a_bkp
rast_aR <- rast_a_bkp
if(sum(round(res(rast_a), digits=2) == round(res(rast_b), digits=2)) != 2){
  res(rast_aR) <- c(0.01,0.01)
}
# create empty raster to store difference
rast_d <- rast_aR
# create empty stacks
stack_a <- stack(replicate(3, rast_a))
stack_b <- stack(replicate(3, rast_c))
stack_d <- stack(replicate(3, rast_d))

# create list of month by year quarters
months_table <- data.frame(Q1=c(1,2,3),Q2=c(4,5,6),Q3=c(7,8,9),Q4=c(10,11,12))

for(q in 1:4){
  print(paste("Processing quarter: ", q, sep=""))
  mList <- months_table[,q]
  
  for(n in 1:3){
    m <- mList[n]
    print(paste("Processing month: ", m, sep=""))
    # import data
    mat_a <- as.matrix(ncvar_get(nc_a, varid = "cmask", start=c(1,1,m), count=c(-1,-1,1)))
    mat_a <- apply(mat_a, 1, rev)
    values(rast_a) <- as.vector(t(mat_a))
    mat_c <- as.matrix(ncvar_get(nc_c, varid = "cfc_all", start=c(1,1,m), count=c(-1,-1,1)))
    mat_c <- apply(mat_c, 1, rev)
    values(rast_c) <- as.vector(t(mat_c))
    # values(rast_a) <- as.vector(ncvar_get(nc_a, varid = "delta", start=c(1,1,tr,m), count=c(-1,-1,1,1)))
    # values(rast_b) <- as.vector(ncvar_get(nc_b, varid = "delta", start=c(1,1,tr,m), count=c(-1,-1,1,1)))
    # using vectors
    if(sum(round(res(rast_a), digits=2) == round(res(rast_c), digits=2)) == 2){ ### do not resample if raster have the same resolution
      val_aRes <- as.vector(getValues(rast_a))
      val_bRes <- as.vector(getValues(rast_c))
    } else {
      val_aRes <- as.vector(getValues(raster::resample(rast_a, rast_d, method="ngb")))
      val_bRes <- as.vector(getValues(raster::resample(rast_c, rast_d, method="ngb")))
    }
    # compute difference
    val_dRes <- (val_aRes - val_bRes)
    values(rast_d) <- as.vector(val_dRes)
    # store values in raster stack
    # values(stack_a[[n]]) <- as.vector(getValues(rast_a))
    # values(stack_b[[n]]) <- as.vector(getValues(rast_b))
    # values(stack_d[[n]]) <- as.vector(getValues(rast_d))
    stack_a[[n]][] <- as.vector(getValues(rast_a))
    stack_b[[n]][] <- as.vector(getValues(rast_c))
    stack_d[[n]][] <- as.vector(getValues(rast_d))
    # stack_a <- setValues(stack_a, as.vector(getValues(rast_a)), layer=n)
    # stack_b <- setValues(stack_b, as.vector(getValues(rast_d)), layer=n)
    # stack_d <- setValues(stack_d, as.vector(getValues(rast_b)), layer=n)
  }
  # set layer names
  names(stack_a) <- mNames[mList]
  names(stack_b) <- mNames[mList]
  names(stack_d) <- mNames[mList]
  # save plot to file
  print("Generating plot")
  png_name <- normalizePath(paste(outmappath, "/", "Comparison_Q", q, "_CFC", ".png", sep=""), winslash="/", mustWork=FALSE)
  png(filename=png_name, width=6400, height=5000, units="px", res=300)
  p1 <- levelplot(stack_a, at=breaks, maxpixels=6000000, par.settings=myTheme, main=name_a, xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
  p1 <- p1 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
  p2 <- levelplot(stack_b, at=breaks, maxpixels=6000000, par.settings=myTheme, main=name_b, xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
  p2 <- p2 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
  p3 <- levelplot(stack_d, at=breaks_diff, maxpixels=6000000, par.settings=myTheme_diff, main="Difference", xlab="Longitude", ylab="Latitude", scales=myScales, margin=FALSE, layout=myLayout, colorkey=colKey)
  p3 <- p3 + layer(sp.polygons(gdam, lwd=0.3, col="black", fill="transparent"))
  print(p1, split=c(1, 1, 3, 1), more=TRUE)
  print(p2, split=c(2, 1, 3, 1), more=TRUE)
  print(p3, split=c(3, 1, 3, 1))
  dev.off()
  gc()
}
