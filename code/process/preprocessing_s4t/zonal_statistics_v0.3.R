#!/usr/bin/Rscript
# Title         : Compute zonal statistics
# Description   : Compute zonal statistics
# Date          : June 2018
# Version       : 0.3.1
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# load required libraries
library(raster)
library(rasterVis)
library(ncdf4)
library(ggplot2)
library(gridExtra)

# set output path where to store results
output_path <- normalizePath(path="/space/filipfe_data/cloud/elevation/zonal_stats/cloud_TSA_statistics_avhrr_global", winslash = "/", mustWork = FALSE)

# create output folder
dir.create(output_path, showWarnings=FALSE, recursive=TRUE)

# set input path for Space4Time result
#input_file <- normalizePath(path="/space/filipfe_data/cloud/space4time/s4t_test02/output_different_kernels/Space4Time_result_test01_kernel5.nc", winslash = "/", mustWork = TRUE)
#input_file <- normalizePath(path="/space/filipfe_data/cloud/space4time/s4t_test04/output/Space4Time_aqua_europe_result_cloud_low_climatology.nc", winslash = "/", mustWork = TRUE)
#input_file <- normalizePath(path="/space/filipfe_data/Tair/Space4Time/output/Space4Time_air_temperature_2014.nc", winslash = "/", mustWork = TRUE)
input_file <- normalizePath(path="/space/filipfe_data/space4time/cloud/aqua_europe/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2003_2014_monthly_avg_climatology_s4t.nc", winslash = "/", mustWork = TRUE)

# import and mask zone datasets
#sd_kernel <- raster("/space/filipfe_data/cloud/elevation/europe_0.02/DEM_Europe_0.02dd_sd_focal_avg.tif")
sd_kernel <- raster("/space/filipfe_data/Tair/elevation/DEM_Global_0.05dd_sd_kernel.tif")
values(sd_kernel) <- getValues(sd_kernel)

#avg_focal <- raster("/space/filipfe_data/cloud/elevation/europe_0.02/DEM_Europe_0.02dd_avg_focal_diff.tif")
avg_focal <- raster("/space/filipfe_data/Tair/elevation/DEM_Global_0.05dd_avg_focal.tif")
values(avg_focal) <- getValues(avg_focal)

#sd_focal <- raster("/space/filipfe_data/cloud/elevation/europe_0.02/DEM_Europe_0.02dd_sd_focal_diff.tif")
sd_focal <- raster("/space/filipfe_data/Tair/elevation/DEM_Global_0.05dd_sd_focal.tif")
values(sd_focal) <- getValues(sd_focal)

#############
# set threshold value for basedata
#threshold <- 100

message(paste(c("Computation started at: "), Sys.time(), sep=""))

# import dataset
nc_in <- ncdf4::nc_open(input_file, readunlim=FALSE, write=FALSE)

# extract list of transitions
#transition_list <- unlist(strsplit(ncatt_get(nc_in, "class_transition", attname="class_transition_list")$value, split=" ")) ### older s4t version
transition_list <- unlist(strsplit(ncatt_get(nc_in, "class_transition", attname="class_transition_list")$value, split=", "))
# define number of class transitions
tr_len <- nc_in$dim$class_transition$len

# set option for cleaning outliers
clean_outliers <- FALSE

# set list of variables to be processed
stlayer_name_list <- c("sd_kernel","avg_focal","sd_focal","co_occurrence","rsquared")
nc_varname_list <- c("delta","delta_error")
#nc_varname_list <- c("delta")

### for debug
# nc_varname <- "delta"
# stlayer_name <- "sd_kernel"

# import 'rsquared' and 'co_occurrence' data
rsquared <- sd_kernel
rsquared_in <- matrix(as.vector(ncdf4::ncvar_get(nc_in, varid = "rsquared")), ncol=nc_in$var$rsquared$size[3])
rsquared_mean <- apply(rsquared_in, 1, mean, na.rm=TRUE)
values(rsquared) <- as.vector(rsquared_mean)
rm(rsquared_in,rsquared_mean)
invisible(gc())
# create void 'co_occurrence' object
co_occurrence <- sd_kernel
values(co_occurrence) <- NaN

# Loop along the list of indicators
for(stlayer_name in stlayer_name_list){
  print(paste("Processing indicator: ", stlayer_name, sep=""))
  stlayer <- get(stlayer_name)
  
  # create list of indicator classes to be used
  if(stlayer_name %in% c("rsquared","co_occurrence")){
    indicator_class_list <- sort(seq(from=0.1, to=1, by=0.05), decreasing=FALSE)
  } else {
    indicator_class_list <- sort(seq(from=5, to=150, by=5), decreasing=TRUE)
  }
  icll <- length(indicator_class_list)
  
  # process the selected NetCDF variables
  for(nc_varname in nc_varname_list){
    
    print("------------------------------------------------------------------")
    
    # Process all the class transitions
    for(r in 1:tr_len){
      transition_name <- transition_list[r]
      print(paste("Processing indicator '", stlayer_name, "' for variable '", nc_varname, "' for transition '", transition_name, "'", " (", r, "/", tr_len, ")", sep=""))
      
      # run for multitemporal data
      time_len <- nc_in$dim$time$len
      
      # create output data frame to store statistic results
      df_result <- data.frame(value=indicator_class_list, y00=as.double(rep(NA, icll)), y01=as.double(rep(NA, icll)), y25=as.double(rep(NA, icll)), y50=as.double(rep(NA, icll)), y75=as.double(rep(NA, icll)), y99=as.double(rep(NA, icll)), y1=as.double(rep(NA, icll)), std=as.double(rep(NA, icll)), valid=as.double(rep(NA, icll)))
      
      # import data
      nc_varname <- "delta"
      nc_data <- ncdf4::ncvar_get(nc_in, varid = nc_varname, start = c(1,1,r,1), count = c(-1,-1,1,-1))
      time_lent <- length(nc_data[1,1,])
      nc_data <- matrix(as.vector(nc_data), ncol=time_lent)
      data_tot <- length(which(!is.na(as.vector(nc_data))))
      invisible(gc())
      
      # # clean outliers using a defined threshold
      # lss <- which(nc_data < -(threshold) | nc_data > threshold)
      # nc_data[lss] <- NaN
      
      # clean outliers using percentiles
      subtitle <- ""
      if(clean_outliers){
        dmin <- as.double(quantile(as.vector(nc_data), probs=c(0.001), na.rm=TRUE, names=FALSE))
        dmax <- as.double(quantile(as.vector(nc_data), probs=c(0.999), na.rm=TRUE, names=FALSE))
        lss <- which(nc_data < dmin | nc_data > dmax)
        nc_data[lss] <- NaN
        subtitle <- paste("(clip min: ", sprintf(fmt = "%#.2f", dmin), " - clip max: ", sprintf(fmt = "%#.2f", dmax), ")", sep="")
      }
      
      # import co_occurrence data
      if(stlayer_name == "co_occurrence"){
        values(co_occurrence) <- as.vector(ncdf4::ncvar_get(nc_in, varid = "co_occurrence", start = c(1,1,r), count = c(-1,-1,1)))
        stlayer <- co_occurrence
      }
      
      #pb <- utils::txtProgressBar(style=3)
      for(i in 1:icll){
        val <- indicator_class_list[i]
        #print(paste("Processing indicator value: ", val, sep=""))
        
        if(stlayer_name %in% c("rsquared","co_occurrence")){
          lss <- which(getValues(stlayer) < val)
        } else {
          lss <- which(getValues(stlayer) > val)
        }
        

        for(l in 1:time_lent){
          nc_data[lss,l] <- NaN
        }
        
        df_result[which(df_result$value == val),2:8] <- quantile(as.vector(nc_data), probs=c(0,0.01,0.25,0.5,0.75,0.99,1), na.rm=TRUE, names=FALSE)
        df_result[which(df_result$value == val),9] <- sd(as.vector(nc_data), na.rm=TRUE)
        df_result[which(df_result$value == val),10] <- as.double(length(which(!is.na(as.vector(nc_data)))) / data_tot * 100)
      }
      #utils::setTxtProgressBar(pb, i/icll)
      #close(pb)
      
      # plot results
      title <- paste("'", stlayer_name, "' - '", nc_varname, "' - '", transition_name, "'\n", subtitle, sep="")
      png(filename=normalizePath(paste(output_path, "/", "TSA", "_", stlayer_name, "_", nc_varname, "_", transition_name, ".png", sep=""), winslash="/", mustWork=FALSE), width=2400, height=1600, units="px", res=300, pointsize = 8)
      plot_box <- ggplot(df_result, aes(value)) + geom_boxplot(aes(ymin=y25, lower=y25, middle=y50, upper=y75, ymax=y75), stat="identity") + ggtitle(title) + theme_bw()
      plot(plot_box)
      dev.off()
      #Sys.sleep(5)
      
      # create a second plot
      png(filename=normalizePath(paste(output_path, "/", "TSA", "_", stlayer_name, "_", nc_varname, "_", transition_name, "_lines", ".png", sep=""), winslash="/", mustWork=FALSE), width=1800, height=3200, units="px", res=300, pointsize = 8)
      p1 <- ggplot(df_result, aes(x=value, y=valid)) + geom_line(color="black", size=0.5) + labs(x="", y="%") + ggtitle(title)
      p2 <- ggplot(df_result, aes(x=value, y=std)) + geom_line(color="chartreuse4", size=0.5) + labs(x="")
      p3 <- ggplot(df_result, aes(x=value, y=y01)) + geom_line(color="steelblue", size=0.5) + scale_y_continuous(trans="reverse") + labs(x="")
      p4 <- ggplot(df_result, aes(x=value, y=y99)) + geom_line(color="darkred", size=0.5) + labs(x=as.character(stlayer_name))
      grid.arrange(p1,p2,p3,p4, nrow=4)
      dev.off()
      
      # export results
      write.table(df_result, file = normalizePath(paste(output_path, "/", "TSA", "_", stlayer_name, "_", nc_varname, "_", transition_name, ".csv", sep=""), winslash="/", mustWork=FALSE), sep="\t", dec=".", row.names=F, col.names=T)
      rm(nc_data)
      invisible(gc())
      
    }
  }
}

message(paste(c("Computation ended at: "), Sys.time(), sep=""))
