#!/usr/bin/Rscript
# Title         : Space4Time orchestrator
# Description   : Compute Space4Time algorithm using parallel support
# Date          : Dec 2018
# Version       : 1.1
# Licence       : GPL v3
# Authors       : Federico Filipponi, Gregory Duveiller, Josh Hooker
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# Usage example
# s4t.r --k_file=input_cover_file.nc --z_file=input_time_series.nc --output=output_space4time.nc
# --exclude_list WAT,No_Data --list BRF,NEF,SAV,SHR,CaG --Pdelta_error --Ppredicted --Prsquared --Pestimates --Pcumulated_variance --Pco_occurrence -q 11 --verbose

# load optparse library for argument parsing
options("repos"="http://cran.at.r-project.org/")
invisible(tryCatch(find.package("optparse"), error=function(e) install.packages("optparse", dependencies=TRUE)))
require(optparse, quietly=TRUE)

# read arguments
option_list <- list(
  optparse::make_option(c("--k_file"), type="character", default=NULL, 
              help="Input 'k_file' path", metavar="file"),
  optparse::make_option(c("--z_file"), type="character", default=NULL, 
              help="Input 'z_file' path", metavar="file"),
  optparse::make_option(c("-o", "--output"), type="character", default=NULL, 
              help="Output NetCDF-4 file path", metavar="file"),
  optparse::make_option(c("-m", "--mask"), type="character", default=NULL, 
              help="Input mask file path", metavar="file"),
  optparse::make_option(c("-n", "--var_name"), type="character", default=NULL, 
              help="Name of variable in the input 'z_file' to be processed", metavar="character"),
  optparse::make_option(c("-l", "--list"), type="character", default=NULL, 
              help="Comma separated list of cover variables in the input 'k_file' to be processed (e.g. 'BRF,SAV')", metavar="character"),
  optparse::make_option(c("-x", "--exclude_list"), type="character", default=NULL, 
              help="Comma separated list of cover variables in the input 'k_file' to be excluded from analysis (e.g. 'WAT,No_Data')", metavar="character"),
  optparse::make_option(c("-w", "--winsize"), type="integer", default=5, 
              help="Size of the moving window (in pixels) [default = %default]", metavar="integer"),
  optparse::make_option(c("-p", "--percentage"), type="double", default=100.0, 
              help="Percentage of minimum number of pixels in the moving window [default = %default]", metavar="double"),
  optparse::make_option(c("-f", "--difference_percentage"), type="double", default=40.0, 
              help="Percentage of minimum number pixels in the moving window that must have different compositions (must be any value in the interval 30-100) [default = %default]", metavar="double"),
  optparse::make_option(c("--co_occurrence_threshold"), type="double", default=0.5,
                        help="Co-occurrence threshold to mask out pixels (must be any value in the interval 0-1) [default = %default]", metavar="double"),
  optparse::make_option(c("-d", "--deflate"), type="integer", default=9, 
                        help="Set the deflate compression level for output NetCDF-4 file (interval 1-9) [default = %default]", metavar="integer"),
  optparse::make_option(c("-q", "--q_parallelism"), type="integer", default=-1, 
              help="Set the maximum parallelism used for the computation. If not set all available cores are used for the analysis", metavar="integer"),
  optparse::make_option(c("--keep_low_co_occurrence"), type="logical", default=FALSE, action="store_true",
                        help="Keep low classes co-occurrences"),
  optparse::make_option(c("--Pdelta_error"), type="logical", default=FALSE, action="store_true",
              help="Output the error associated to the delta estimation"),
  optparse::make_option(c("--Ppredicted"), type="logical", default=FALSE, action="store_true",
              help="Output the predicted value"),
  optparse::make_option(c("--Prsquared"), type="logical", default=FALSE, action="store_true",
              help="Output the coefficient of determination for the predicted values"),
  optparse::make_option(c("--Pestimates"), type="logical", default=FALSE, action="store_true",
              help="Output estimates"),
  optparse::make_option(c("--Pcumulated_variance"), type="logical", default=FALSE, action="store_true",
              help="Output the cumulated variance"),
  optparse::make_option(c("--Pco_occurrence"), type="logical", default=FALSE, action="store_true",
                        help="Output the co-occurrence"),
  optparse::make_option(c("--verbose"), type="logical", default=FALSE, action="store_true",
              help="Verbose mode")
)

opt_parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(opt_parser)

# check if required arguments are supplied and point to existing files
if(is.null(opt$z_file)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (z_file).", call.=FALSE)
}
if(is.null(opt$k_file)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (k_file).", call.=FALSE)
}
if(is.null(opt$output)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (output).", call.=FALSE)
}

# define R objects from options
input_Ka <- normalizePath(path=opt$k_file, winslash="/", mustWork=TRUE)
input_Za <- normalizePath(path=opt$z_file, winslash="/", mustWork=TRUE)
output_file <- normalizePath(path=opt$output, winslash="/", mustWork=FALSE)
input_mask <- opt$mask

var_name <- opt$var_name
c_list <- opt$list
c_list_exclude <- opt$exclude_list

winsiz <- as.integer(opt$winsize)
minPxls_perc <- as.double(opt$percentage)
minDiffPxls_perc <- as.double(opt$difference_percentage)
co_occurrence_threshold <- as.double(opt$co_occurrence_threshold)
keep_low_co_occurrence <- as.logical(opt$keep_low_co_occurrence)

nc_deflate <- as.integer(opt$deflate)
cores <- as.integer(opt$q_parallelism)
verbose <- opt$verbose

flag_delta_error <- as.logical(opt$Pdelta_error)
flag_predicted <- as.logical(opt$Ppredicted)
flag_rsquared <- as.logical(opt$Prsquared)
flag_estimates <- as.logical(opt$Pestimates)
flag_cumulated_variance <- as.logical(opt$Pcumulated_variance)
flag_co_occurrence <- as.logical(opt$Pco_occurrence)

nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")

# ### for debug
# input_Za <- normalizePath('/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2004_2014_monthly_avg_climatology_clip_0.02dd.nc', winslash="/", mustWork=TRUE)
# input_Ka <- normalizePath('/space/filipfe_data/cloud/space4time/s4t_europe_comparisons/input/ESACCI-LC-L4-LCCS-Map-300m-P1Y-aggregated-0.020000Deg-USER_REGION-2014-v2.0.7_resize_clip_0.02dd.nc', winslash="/", mustWork=TRUE)
# output_file <- normalizePath('/space/filipfe_data/cloud/space4time/Space4Time_result_test_01.nc', winslash="/", mustWork=FALSE)
# #input_mask <- normalizePath('/space/filipfe_data/space4time/cloud/aqua_europe/input/Europe_MODIS_AQUA_mask4tests.nc', winslash="/", mustWork=TRUE)
# input_mask <- NULL
#
# #c_list <- c("BRF,NEF,SAV,SHR,CaG")
# c_list <- c("EFO,DFO,SHR,SAV,CaG,WET")
# #c_list <- c("BRF,NEF,SAV,SHR,CaG,WET,WAT,BSV,No_Data")
# c_list_exclude <- c("WAT,No_Data")
# var_name <- NULL
#
# #var_name <- "cmask"
# #PFTlist <- c('BRF','NEF','SAV','SHR','CaG','WET','WAT','BSV','No_Data')
#
# winsiz=5               # size of the moving window
# # minPxls=25           # minimum number of pixels needed in the moving window for the un-mixing to occur
# # minDiffPxls=10       # a further requirement on the number of pixels that must have diff compositions
# minPxls_perc <- 100    # percentage value
# minDiffPxls_perc <- 40 # percentage value
# co_occurrence_threshold <- as.double(0.5)
# keep_low_co_occurrence <- TRUE
#
# cores <- 10
# nc_deflate <- 9
# verbose <- TRUE
#
# nc_hhist <- ""

# set flags
# flag_delta_error <- TRUE
# flag_predicted <- TRUE
# flag_rsquared <- TRUE
# flag_estimates <- TRUE
# flag_cumulated_variance <- TRUE
# flag_co_occurrence <- TRUE

# ###################################
# load required libraries
# #library(stars)
# #library(rlist)
# #library(Thermimage)
# library(raster)
# library(ncdf4)
# library(doParallel)
invisible(tryCatch(find.package("raster"), error=function(e) install.packages("raster", dependencies=TRUE)))
invisible(tryCatch(find.package("ncdf4"), error=function(e) install.packages("ncdf4", dependencies=TRUE)))
invisible(tryCatch(find.package("doParallel"), error=function(e) install.packages("doParallel", dependencies=TRUE)))
require(raster, quietly=TRUE)
require(ncdf4, quietly=TRUE)
require(doParallel, quietly=TRUE)

message("Running Space4Time processor v1.1 ...")

# ###################################
# check input arguments

# check if window size is big enough
if(winsiz < 3)
  stop("Window size must be greater than 3")
if(winsiz > 21)
  warning("Window size is set to a high value and may slow down the process")
# check number of pixels in the moving window required for the analysis
if(minPxls_perc < 30 | minPxls_perc > 100){
  stop("Percentage of minimum number of pixels in the moving window available for the analysis is not in interval 30-100 %")
} else {
  minPxls <- round(winsiz*winsiz * minPxls_perc / 100)
  if(minPxls < 9)
    stop("Minimum number of pixels in the moving window available for the analysis must be higher than '9'.\nSet a higher value using the '--percentage' argument.\nWhen '--winsize' is set to '3' ''--percentage' must be set to '100'")
}
# check percentage and set number of pixel with different compositions
if(minDiffPxls_perc < 30 | minDiffPxls_perc > 100){
  stop("Percentage of pixels in the moving window that must have different compositions is not in interval 30-100 %")
} else {
  minDiffPxls <- round(winsiz*winsiz * minDiffPxls_perc / 100)
  if(minDiffPxls < 9)
    stop("Minimum number of pixels with different composition must be higher than '9'.\nSet a higher value using the '--difference_percentage' argument.\nWhen '--winsize' is set to '3' ''--difference_percentage' must be set to '100'")
}
# check if co-occurrence thresholud is in the interval 0-1
if(co_occurrence_threshold < 0 | co_occurrence_threshold > 1)
  stop("Co-occurrence threshold set using argument '--co_occurrence_threshold' is not in the interval '0-1'")
# check if c_list' argument has at leat two cover classes
if(!is.null(c_list)){
  if(length(unlist(strsplit(c_list, ","))) < 2)
    stop("A list of two or more comma separated elements should be supplied to argument '--c_list'")
}

# check if 'z_file' and 'k_file' have the same raster characteristics (extent, resolution, crs) 
if(!(suppressWarnings(raster::compareRaster(stack(input_Za)[[1]], stack(input_Ka)[[1]], extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))))
  stop("Input 'z_file' and 'k_file' raster characteristics differs")

# check if '--Pco_occurrence' flag is used when '--keep_low_co_occurrence' is set
if(keep_low_co_occurrence & !flag_co_occurrence){
  flag_co_occurrence <- TRUE
  warning("Flag '--keep_low_co_occurrence' is used and no co-occurrence output is requested. Forcing the creation of co-occurrence output")
}

# check if '--deflate' level is a value between 1 and 9
if(nc_deflate < 1 | nc_deflate > 9){
  warning(paste("NetCDF compression level set using the '--deflate' parameter is '", nc_deflate, "' and not included in the allowed interval 1-9\nCompression level is set to '9'", sep=""))
  nc_deflate <- 9
}

if(file.exists(output_file)){
  stop(paste("Output file path '", output_file,"' already exists. Set another output file name", sep=""))
}

# create output folder
dir.create(dirname(output_file), showWarnings=FALSE, recursive=TRUE)

# #####################
# import 'Z' dataset

# open NetCDF connection
nc_Za <- ncdf4::nc_open(input_Za, readunlim=FALSE, write=FALSE)
nc_Za$safemode <- TRUE

# check if NetCDF-4 has the required dimensions
if(sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_Za$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_Za$dim))) == 1 )  + as.numeric(sum(as.numeric(c("time","Time") %in% names(nc_Za$dim))) == 1 ) )  < 3 )
  stop(paste("Input NetCDF file '--z_file' does not contain explicit dimension names: \n", "'lon' ['longitude','Longitude'], 'lat' ['latitude','Latitude'], 'time' ['Time']", sep=""))

# check if 'var_name' argument is consistent with the input one
if(nc_Za$nvars == 1){
  if(is.null(var_name)){
    var_name <- as.character(names(nc_Za$var[1]))
    if(verbose){
      warning(paste("Variable name not set using '--var_name' argument.\nNetCDF variable '", var_name,"' contained in the input 'z_file' will be processed", sep=""))
    }
  } else {
    if(names(nc_Za$var)[1] != var_name){
      # overwrite custom variable name set as input
      if(verbose){
        warning(paste("Variable name '", var_name,"' differs from variable '", names(nc_Za$var)[1],"' contained in the input 'z_file'", sep=""))
      }
      var_name <- names(nc_Za$var)[1]
    }
  }
} else {
  if(is.null(var_name)){
    stop("Input 'z_file' contains multiple variables and '--var_name' argument is not set")
  } else {
    if(!(var_name %in% names(nc_Za$var))){
      stop("Variable set using '--var_name' argument is not included among variables in input 'z_file'")
    }
  }
}

# get dimensions position
nc_lon_pos <- which(names(nc_Za$dim) %in% c("lon","longitude","Longitude"))[1]
nc_lat_pos <- which(names(nc_Za$dim) %in% c("lat","latitude","Latitude"))[1]
nc_time_pos <- which(names(nc_Za$dim) %in% c("time","Time"))[1]

# get dimension values
nc_lon_dim <- ncdf4::ncvar_get(nc_Za, names(nc_Za$dim)[nc_lon_pos])
nc_lat_dim <- ncdf4::ncvar_get(nc_Za, names(nc_Za$dim)[nc_lat_pos])
nc_time_dim <- ncdf4::ncvar_get(nc_Za, names(nc_Za$dim)[nc_time_pos])

# get dimension units
nc_lon_units <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_lon_pos], "units")$value
nc_lat_units <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_lat_pos], "units")$value
nc_time_units <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_time_pos], "units")$value

# get dimension longnames
nc_lon_lname <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_lon_pos], "long_name")$value
nc_lat_lname <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_lat_pos], "long_name")$value
nc_time_lname <- ncdf4::ncatt_get(nc_Za, names(nc_Za$dim)[nc_time_pos], "long_name")$value

# get dimension length
n_row <- length(nc_Za$dim[[nc_lat_pos]]$vals)
n_col <- length(nc_Za$dim[[nc_lon_pos]]$vals)
nT <- length(nc_Za$dim[[nc_time_pos]]$vals)
xyOutDim <- c(n_row, n_col)

# check if latitude dimension is sorted north-to-south
Za_nc_lat_dim_in <- ncdf4::ncvar_get(nc_Za, names(nc_Za$dim)[nc_lat_pos])
Za_nc_lat_dim_ord <- sort(Za_nc_lat_dim_in, decreasing=TRUE)
if(as.integer(sum(as.numeric(Za_nc_lat_dim_in == Za_nc_lat_dim_ord))) < length(Za_nc_lat_dim_ord)){
  Za_nc_lat_reverse <- TRUE
} else {
  Za_nc_lat_reverse <- FALSE
}

# #####################
# import 'K' dataset

# open NetCDF connection
nc_Ka <- ncdf4::nc_open(input_Ka, readunlim=FALSE)

# check if NetCDF-4 has the required dimensions
if(sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_Ka$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_Ka$dim))) == 1 )) < 2)
  stop(paste("Input NetCDF file '--k_file' does not contain explicit dimension names: \n", "'lon' ['longitude','Longitude'], 'lat' ['latitude','Latitude']", sep=""))

# get dimensions position
nc_lon_pos_k <- which(names(nc_Ka$dim) %in% c("lon","longitude","Longitude"))[1]
nc_lat_pos_k <- which(names(nc_Ka$dim) %in% c("lat","latitude","Latitude"))[1]

n_row_Ka <- length(nc_Ka$dim[[nc_lat_pos_k]]$vals)
n_col_Ka <- length(nc_Ka$dim[[nc_lon_pos_k]]$vals)

if(is.null(c_list)){
  PFTlist <- names(nc_Ka$var)
  if(verbose){
    message(paste("No list of cover variables was supplied using the '--list' argument.\nImporting and processing all variables in 'k_file':\n", paste(PFTlist, collapse=", "), sep=""))
  }
} else {
  PFTlist <- unlist(strsplit(c_list, ","))
}

# check input NetCDF
# check if variables exists in input NetCDF
if(sum(as.numeric(PFTlist %in% names(nc_Ka$var))) < length(PFTlist))
  stop(paste('Some of the variables in list are not contained in input NetCDF file: \n', paste(PFTlist[which((PFTlist %in% names(nc_Ka$var)) == FALSE)], collapse=', '), sep=''))

# check if latitude dimension is sorted north-to-south
Ka_nc_lat_dim_in <- ncdf4::ncvar_get(nc_Ka, names(nc_Ka$dim)[nc_lat_pos_k])
Ka_nc_lat_dim_ord <- sort(Ka_nc_lat_dim_in, decreasing=TRUE)
if(as.integer(sum(as.numeric(Ka_nc_lat_dim_in == Ka_nc_lat_dim_ord))) < length(Ka_nc_lat_dim_ord)){
  Ka_nc_lat_reverse <- TRUE
} else {
  Ka_nc_lat_reverse <- FALSE
}

# print messages
if(verbose){
  message(paste("Processing file: ", input_Za, sep=""))
  message(paste("Using cover file: ", input_Ka, sep=""))
  message(paste("Results are saved to file: ", output_file, sep=""))
  if(!is.null(input_mask)){
    message(paste("Using mask file: ", input_mask, sep=""))
  }
}

# import data using 'stars' package
#Kin <- stars::read_stars(input_Ka, quiet=FALSE)

# create K array and fill in with data
PFTlist_import <- names(nc_Ka$var)
Ka <- array(NA, dim=c(n_col_Ka, n_row_Ka, length(PFTlist_import)))
for(iK in PFTlist_import){
  #Ka[,,which(PFTlist==iK)] <- Kin[[which(names(Kin)==iK)]]
  if(Ka_nc_lat_reverse){
    Ka_in_flip <- as.matrix(ncdf4::ncvar_get(nc_Ka, varid=iK))
    #Ka_in <- apply(Ka_in_flip,2,rev)
    Ka_in <- as.matrix(rev(as.data.frame(Ka_in_flip)))
  } else {
    Ka_in <- ncdf4::ncvar_get(nc_Ka, varid=iK)
  }
  Ka[,,which(PFTlist_import==iK)] <- Ka_in
}
# fix PFTlist
PFTlist <- PFTlist_import[PFTlist_import %in% PFTlist]
if(verbose){
  message(paste("Processing classes: ", paste(PFTlist, collapse=", "), sep=""))
}

# need to ensure no NAs in the composition 
Ka[is.na(Ka)] <- 0

# ###########################
# Import mask

if(!is.null(input_mask)){
  ext_mask <- TRUE
  input_mask_file <- normalizePath(path=input_mask, winslash="/", mustWork=FALSE)
  if(!(file.exists(input_mask_file))){
    ext_mask <- FALSE
    warning("Input 'mask' file does not exists.\nGoing on without using external mask layer")
  } else {
    # import mask file
    nc_mask <- ncdf4::nc_open(input_mask_file, readunlim=FALSE)
    if(nc_mask$nvars > 1){
      ext_mask <- FALSE
      if(verbose){
        warning("Input 'mask' file has more than one variable.\nGoing on without using external mask layer")
      }
    } else {
      if(nc_mask$ndims > 2 | sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_mask$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_mask$dim))) == 1 )) < 2){
        warning("Input 'mask' file does not contain the correct number of dimensions or explicit dimension names: 'lon' ['longitude','Longitude'], 'lat' ['latitude','Latitude'].\nGoing on without using external mask layer")
        ext_mask <- FALSE
      } else {
        # check if mask extent and resolution are the same of 'z_input' file
        mask_compare <- suppressWarnings(raster::compareRaster(stack(input_Za)[[1]],raster(input_mask), extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))
        if(mask_compare){
          # check if latitude dimension is sorted north-to-south
          mask_nc_lat_dim_in <- ncdf4::ncvar_get(nc_mask, names(nc_mask$dim)[which(names(nc_mask$dim) %in% c("lat","latitude","Latitude"))[1]])
          mask_nc_lat_dim_ord <- sort(mask_nc_lat_dim_in, decreasing=TRUE)
          if(as.integer(sum(as.numeric(mask_nc_lat_dim_in == mask_nc_lat_dim_ord))) < length(mask_nc_lat_dim_ord)){
            mask_nc_lat_reverse <- TRUE
          } else {
            mask_nc_lat_reverse <- FALSE
          }
          # import mask data
          r_mask <- ncdf4::ncvar_get(nc_mask)
          # flip mask if latitude is bottomup
          if(mask_nc_lat_reverse){
            #r_mask <- apply(r_mask,2,rev)
            r_mask <- as.matrix(rev(as.data.frame(r_mask)))
            # or alternatively use:
            #r_mask <- Thermimage::mirror.matrix(r_mask)
          }
          if(as.integer(sum(as.numeric(r_mask %in% c(0,1)))) < length(r_mask)){
            r_mask[,] <- 1
            if(verbose){
              warning("Input 'mask' file has values different from '0' and '1'.\nGoing on without using external mask layer")
            }
          }
        } else {
          ext_mask <- FALSE
          if(verbose){
            warning("Input 'mask' file has different extent or resolution than input 'z_file'.\nGoing on without using external mask layer")
          }
        }
      }
    }
  }
} else {
  ext_mask <- FALSE
}

# create mask with all values equal to 1
if(!ext_mask){
  r_mask <- matrix(data=as.integer(1), nrow=n_col, ncol=n_row)
}

# refine mask with classes listed in '--exclude_list'
if(!(is.null(c_list_exclude))){
  PFTlist_exclude <- unlist(strsplit(c_list_exclude, ","))
  if(sum(as.numeric(PFTlist_exclude %in% names(nc_Ka$var))) < length(PFTlist_exclude)){
    warning(paste("Some of the variables in '--exclude-list' are not contained in input NetCDF file: \n", paste(PFTlist_exclude[which((PFTlist_exclude %in% names(nc_Ka$var)) == FALSE)], collapse=', '), "\nGoing on without masking the 'exclude_list' classes", sep=''))
  } else {
    if(verbose){
      message(paste("Pixels corresponding to the following classes will be masked from analysis: ", paste(PFTlist_exclude, collapse=", "), sep=""))
    }
    for(iK in PFTlist_exclude){
      r_mask_ex <- ncdf4::ncvar_get(nc_Ka, varid=iK)
      r_mask[which(r_mask_ex > 0)] <- 0
      rm(r_mask_ex)
    }
  }
} else {
  PFTlist_exclude <- NULL
}

if(exists("r_mask")){
  if(all(as.vector(r_mask) == 1)){
    ext_mask <- FALSE
  } else {
    ext_mask <- TRUE
  }
} else {
  ext_mask <- FALSE
  r_mask <- matrix(data=as.integer(1), nrow=n_col, ncol=n_row)
}

# print messages on console
if(verbose){
  message(paste("Using a moving window of size: ", winsiz, " x ", winsiz, sep=""))
  message(paste("Percentage of minimum number of pixels in the moving window: ", minPxls_perc, " % (", minPxls, " pixels)", sep=""))
  message(paste("Percentage of minimum number pixels in the moving window that must have different compositions: ", minDiffPxls_perc, " % (", minDiffPxls, " pixels)", sep=""))
  if(!keep_low_co_occurrence){
    message(paste("Low co-occurrence index pixels will be masked out from the result. Co-occurrence threshold is set to: ", co_occurrence_threshold, sep=""))
  }
}

# ###########################
# Set variables for the NetCDF-4 history

nc_hist_k <- paste("k_input=", input_Ka, sep="")
nc_hist_z <- paste("z_input=", input_Za, sep="")
nc_hist_output <- paste("output=", output_file, sep="")
nc_hist_list <- paste("analysed classes list=", paste(PFTlist, collapse=", "), sep="")
nc_hist_excluded <- paste("excluded classes list=", paste(PFTlist_exclude, collapse=", "), sep="")
nc_hist_winsize <- paste("Size of moving window=", winsiz, sep="")
nc_hist_minPxls <- paste("Percentage of minimum number of pixels in the moving window=", minPxls_perc, sep="")
nc_hist_minDiffPxls <- paste("Percentage of pixels in the moving window that must have different compositions=", minDiffPxls_perc, sep="")
nc_hist_keep <- paste("keep_low_co_occurrence=", keep_low_co_occurrence, sep="")
nc_hist_keep_thr <- paste("low_co_occurrence_threshold=", co_occurrence_threshold, sep="")
if(ext_mask){
  nc_hist_mask <- paste("mask=", input_Za, sep="")
} else {
  nc_hist_mask <- ""
}

# remove unuseful objects
suppressWarnings(rm(c_list,c_list_exclude,iK,input_mask,ext_mask,input_Ka,input_Za,minDiffPxls_perc,minPxls_perc,nc_Ka,n_col_Ka,n_row_Ka,PFTlist_exclude,Ka_nc_lat_dim_in,Ka_nc_lat_dim_ord,Za_nc_lat_dim_in,Za_nc_lat_dim_ord))

# ###########################
# Set variables for the computation

# define off check function
is.odd <- function(x){
  x %% 2 != 0
}

if(is.odd(winsiz)){
  hwA <- floor(winsiz/2)
  hwB <- hwA
} else {
  warning("Window size is not odd.\nGoing on however")
  hwB <- floor(winsiz/2)
  hwA <- hwB-1
}

# set half window length
#hw <- floor(winsiz/2)

# lower triangular matrix index use further on
LtriIndex <- lower.tri(matrix(T,nrow=length(PFTlist),ncol=length(PFTlist)))
dimnames(LtriIndex) <- list(PFTlist,PFTlist)

# set names of transition combinations
PFT2trans_comb_names <- rep(as.character("NoName-->NoName"),length(which(LtriIndex==TRUE)))
n <- 1
for(m in 1:(ncol(LtriIndex)-1)){
  PFT2trans_comb_names[n:(n+sum(as.numeric(LtriIndex[,m]))-1)] <- paste(dimnames(LtriIndex)[[2]][m],as.vector(dimnames(LtriIndex)[[1]][which(LtriIndex[,m]==TRUE)]), sep='-->')
  n <- n+sum(as.numeric(LtriIndex[,m]))
}

# define number of transitions
nD <- sum(LtriIndex)

# define number of classes
nC <- as.integer(dim(Ka)[3])
# define number of classes to be processed
nK <- as.integer(length(PFTlist))

# ###########################
# Define co-occurrence function

cooccufun <- function(q1, q2){
  if(length(q1)==length(q2) & !anyNA(q1) & !anyNA(q2)){
    p1 <- seq(0, 1, length.out=length(q1))
    pUnif <- cbind(p1, p2=p1[length(p1):1])
    pMinDists <- apply(pUnif, 1, function(z) min((q1-z[1])^2 + (q2-z[2])^2))
    denom <- sum(sqrt(apply(pUnif^2, 1, sum)))
    return(1-(sum(sqrt(pMinDists))/denom))
  } else {
    return(0)
  }
}

# create list of layer pairs to be processed
lay_list <- which(LtriIndex == TRUE)
Ltri_rname <- as.vector(matrix(data=dimnames(LtriIndex)[[1]], nrow=length(LtriIndex[1,]), ncol=length(LtriIndex[,1]), byrow=FALSE))
Ltri_cname <- as.vector(matrix(data=dimnames(LtriIndex)[[1]], nrow=length(LtriIndex[1,]), ncol=length(LtriIndex[,1]), byrow=TRUE))

# ###########################
# create output NetCDF-4 file to store results
#ncout_filename <- normalizePath(output_file, winslash="/", mustWork=FALSE)

# define fill value
ncout_fill_value <- -999.0

# define new dimensions
ncout_londim <- ncdf4::ncdim_def(name="lon", longname=nc_lon_lname, units=nc_lon_units, vals=as.double(nc_lon_dim), unlim=FALSE)
ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=nc_lat_lname, units=nc_lat_units, vals=as.double(sort(nc_lat_dim, decreasing=TRUE)), unlim=FALSE)
ncout_timedim <- ncdf4::ncdim_def(name="time", longname=nc_time_lname, units=nc_time_units, vals=as.double(nc_time_dim))
ncout_nKdim <- ncdf4::ncdim_def(name="class", longname="cover_class", units="dl", vals=as.integer(1:nK))
ncout_nDdim <- ncdf4::ncdim_def(name="class_transition", longname="cover_class_transitions", units="dl", vals=as.integer(1:nD))

# define variable list
nc_varobj_list <- c("nc_var_delta")

# define variables
nc_var_delta <- ncdf4::ncvar_def(name="delta", longname="Delta", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_nDdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nD,nT), compression=nc_deflate)
# create list of variables for NetCDF-4 creation
nc_var_list <- list(nc_var_delta)

if(flag_delta_error){
  nc_var_delta_error <- ncdf4::ncvar_def(name="delta_error", longname="Delta Error", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_nDdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nD,nT), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_delta_error")
  nc_var_list <- c(nc_var_list, list(nc_var_delta_error))
  # nc_var_list <- rlist::list.append(nc_var_list, nc_var_delta_error) # alternative using rlist::list.append() function
}
if(flag_predicted){
  nc_var_predicted <- ncdf4::ncvar_def(name="predicted", longname="Predicted Value", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nT), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_predicted")
  nc_var_list <- c(nc_var_list, list(nc_var_predicted))
}
if(flag_rsquared){
  nc_var_rsquared <- ncdf4::ncvar_def(name="rsquared", longname="Coefficient of Determination", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nT), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_rsquared")
  nc_var_list <- c(nc_var_list, list(nc_var_rsquared))
}
if(flag_estimates){
  nc_var_estimates <- ncdf4::ncvar_def(name="estimates", longname="Estimates", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_nKdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nK,nT), compression=nc_deflate)
  nc_var_estimates_error <- ncdf4::ncvar_def(name="estimates_error", longname="Estimates Error", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_nKdim,ncout_timedim), chunksizes=c(xyOutDim[2],1,nK,nT), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_estimates", "nc_var_estimates_error")
  nc_var_list <- c(nc_var_list, list(nc_var_estimates, nc_var_estimates_error))
}
if(flag_cumulated_variance){
  nc_var_cumulated_variance <- ncdf4::ncvar_def(name="cumulated_variance", longname="Cumulated variance", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim), chunksizes=c(xyOutDim[2],1), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_cumulated_variance")
  nc_var_list <- c(nc_var_list, list(nc_var_cumulated_variance))
}
if(flag_co_occurrence){
  nc_var_co_occurrence <- ncdf4::ncvar_def(name="co_occurrence", longname="Cover classes co-occurrence", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_nDdim), chunksizes=c(xyOutDim[2],1,nD), compression=nc_deflate)
  nc_varobj_list <- c(nc_varobj_list, "nc_var_co_occurrence")
  nc_var_list <- c(nc_var_list, list(nc_var_co_occurrence))
  # nc_var_list <- rlist::list.append(nc_var_list, nc_var_delta_error) # alternative using rlist::list.append() function
}

# create output NetCDF-4 file
ncout <- ncdf4::nc_create(output_file, vars=nc_var_list, force_v4=TRUE)
#ncout <- ncdf4::nc_create(output_file, vars=as.list(get(nc_varobj_list)), force_v4=TRUE) # not working the list of variables, should be concatenated using rlist::list.append()
#ncout <- ncdf4::nc_create(output_file, vars=list(nc_var_delta, nc_var_delta_error, nc_var_predicted, nc_var_rsquared, nc_var_estimates, nc_var_estimates_error, nc_var_cumulated_variance), force_v4=TRUE)

# put additional attributes into dimension and data variables
ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
ncdf4::ncatt_put(ncout,"lon","axis","X")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"lat","axis","Y")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"time","standard_name","time")
ncdf4::ncatt_put(ncout,"time","calendar","standard")
nc_class_transition_list <- paste(PFT2trans_comb_names, collapse=", ")
ncdf4::ncatt_put(ncout,"class_transition","class_transition_list",nc_class_transition_list)

if(flag_estimates){
  nc_class_list <- paste(PFTlist, collapse=" ")
  ncdf4::ncatt_put(ncout,"class","class_list",nc_class_list)
}

# set global attributes to output NetCDF-4 file
ncdf4::ncatt_put(ncout,0,"title","Space4Time")
#ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., Hooker, J., & Cescatti, A. (2018). A dataset mapping the potential biophysical effects of vegetation cover change. Scientific data, 5, 180014.")
ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., Hooker, J., & Cescatti, A. (2018). The mark of vegetation change on Earth’s surface energy balance. Nature Communications, 9, 679.")
ncdf4::ncatt_put(ncout,0,"summary","Product generated using ’Space4Time’ algorithm")
ncdf4::ncatt_put(ncout,0,"keywords","Space4Time")
ncdf4::ncatt_put(ncout,0,"Conventions", "CF-1.7")
ncdf4::ncatt_put(ncout,0,"s4t_winsize", winsiz, prec="short")

# set class names
nc_class_transition_list <- paste(paste(1:sum(LtriIndex), PFT2trans_comb_names, sep=": "), collapse=", ")
nc_class_list_import <- paste(paste(1:length(PFTlist_import), PFTlist_import, sep=": "), collapse=", ")
nc_class_list <- paste(paste(1:length(PFTlist), PFTlist, sep=": "), collapse=", ")
ncdf4::ncatt_put(ncout,0,"Input_class_list", nc_class_list_import)
ncdf4::ncatt_put(ncout,0,"Analyzed_class_list", nc_class_list)
ncdf4::ncatt_put(ncout,0,"Class_transition_list", nc_class_transition_list)

# # extra global attributes
# ncdf4::ncatt_put(ncout,0,"crs", "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# ncdf4::ncatt_put(ncout,0,"crs_format","PROJ.4")

# remove unuseful objects
suppressWarnings(rm(list=c("ncout_fill_value", "ncout_londim", "ncout_latdim", "ncout_timedim", "ncout_nKdim", "ncout_nDdim", "n", "m", "nc_class_list", "nc_class_list_import", "nc_class_transition_list", "nc_varobj_list", "PFT2trans_comb_names", "output_file")))
suppressWarnings(rm(list=c("nc_lat_dim","nc_lon_dim","nc_time_dim","nc_lat_units","nc_lon_units","nc_time_units","nc_lat_lname","nc_lon_lname","nc_time_lname","nc_lon_pos","nc_lat_pos","nc_time_pos","nc_lon_pos_k","nc_lat_pos_k","nc_deflate")))
suppressWarnings(rm(list=ls(pattern="nc_var_")))

# ###########################
# set and check number of cores
if(cores == -1){
  cores <- parallel::detectCores()
  if(verbose){
    message(paste("Number of cores used for the processing is set to '", cores, "' according to local hardware", sep=""))
  }
}
# check if machine has enough available cores
if(cores > parallel::detectCores()){
  cores <- parallel::detectCores()
  if(verbose){
    warning(paste("Number of cores used for the processing is reduced to '", cores, "' according to local hardware", sep=""))
  }
} else {
  if(verbose){
    message(paste("Number of cores used for the processing is set to '", cores, "'", sep=""))
  }
}

# start cluster
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)
invisible(gc(verbose=FALSE))
# create list of object not to be exported in foreach loop
noexport_list <- c("Ka","Za","r_mask","cl","cores",ls(pattern="nc_hist_"),ls(pattern="flag_"),"ncout","nc_Za","coccufun","nc_hhist")
noexport_list_coccu <- c("Ka","Za","r_mask","cl","cores",ls(pattern="nc_hist_"),ls(pattern="flag_"),"ncout","nc_Za","nc_hhist")

# ###########################
# Process

# set raster lines and columns to be processed
proc_line_list <- as.vector((1 + hwA):(n_row - hwB))
proc_col_list <- as.vector((1 + hwA):(n_col - hwB))

# set lines to be skipped due to mask
r_mask_lines <- apply(r_mask, MARGIN=2, FUN=function(x) as.logical(sum(x) > 1))
proc_line_list_masked <- proc_line_list[which(proc_line_list %in% which(r_mask_lines == TRUE))]

### for debug
#proc_line_list_masked <- c(1631:1650)
#proc_line_list_masked <- c(931:940)

# stop the process if there are no lines to process
if(length(proc_line_list_masked) == 0)
  stop("Nothing to compute since all lines were masked from the process. No output to generate")
if(verbose){
  message(paste("Number of lines to be processed is ", length(proc_line_list_masked), " (", (n_row - length(proc_line_list_masked))," lines are masked from processing)", sep=""))
}

# # load function from source
# source("/space/filipfe_data/cloud/code/Space4Time/s4t_fun.r")
# # compile function
# library(compiler)
# s4t_fun_cmp <- cmpfun(s4t_fun)

if(verbose){
  message(paste(c("Space4Time analysis started at: "), Sys.time(), sep=""))
}
ptm <- proc.time()

pb <- utils::txtProgressBar(style=3)

if(flag_co_occurrence){
  # fill in co-occurrence variable in NetCDF-4 file with zeros
  ncdf4::ncvar_put(ncout, varid="co_occurrence", vals=rep(0,as.integer(xyOutDim[1]*xyOutDim[2]*nD)), start=c(1,1,1), count=c(-1,-1,-1))
}

# process using moving window
for(i in proc_line_list_masked){
  
  ### for debug
  # if(verbose){
  #   message(paste(" Processing line ", i, " ", "(", which((proc_line_list_masked %in% i) == TRUE), " / ", length(proc_line_list_masked), ")", sep=""))
  # }
  
  # create empty array to store results
  result_array <- array(as.numeric(NA), dim=c(xyOutDim[2],7,max(nK,nD),nT))
  
  # check if there are not masked pixels in the entire line
  if(sum(r_mask[,i]) > 1){
    
    # reverse i index if Za NetCDF is bottomup
    if(Za_nc_lat_reverse){
      Za_row <- xyOutDim[1]+1-i
      hwZ <- hwB
    } else {
      Za_row <- i
      hwZ <- hwA
    }
    # import data
    Za <- ncdf4::ncvar_get(nc_Za, varid=var_name, start=c(1,Za_row-hwZ,1), count=c(-1,winsiz,-1), verbose=FALSE)
    # permute dimensions
    Za_perm <- aperm(Za, c(2,1,3))
    # flip north to south if latitude is bottomup
    if(Za_nc_lat_reverse){
      Za_perm <- apply(Za_perm, 2:3, rev)
    }
    # read only few lines of Za object to reduce memory usage
    Ka_perm <- aperm(Ka[,(i-hwA):(i+hwB),], c(2,1,3))
    
    # remove columns from process list (masked pixels)
    proc_col_list_masked <- proc_col_list[which(proc_col_list %in% which(r_mask[,i] == 1) == TRUE)]
    
    # further refine mask list
    for(j in proc_col_list_masked){
      localComp <- matrix(as.vector(Ka_perm[,(j-hwA):(j+hwB),]), ncol=nC)
      pft_check <- as.numeric(sum(as.numeric((apply(localComp, 2, sum) > 0) == TRUE)) > 1)
      if(pft_check == 0){
        r_mask[j,i] <- 0
      }
    }
    suppressWarnings(rm(localComp,pft_check))
    
    # remove columns from process list (masked pixels)
    proc_col_list_masked <- proc_col_list[which(proc_col_list %in% which(r_mask[,i] == 1) == TRUE)]
    
    # ###############################
    # Compute co-occurrence of vegetation classes
    # create empty matrix to store results
    co_occurrence_res <- matrix(data=0, nrow=dim(Ka_perm)[2], ncol=length(lay_list))
    
    co_occurrence_result <- foreach::foreach(j=proc_col_list_masked,
                                             .combine=rbind,
                                             .inorder=TRUE,
                                             .noexport=noexport_list_coccu,
                                             .verbose=FALSE) %dopar% {
                                               # create emty object to store result
                                               coccu_res <- rep(0, length(lay_list))
                                               #for (j in (1+hw):(dim(Ka)[1]-hw)){
                                               for (l in 1:length(lay_list)){
                                                 # define the layer to be processed
                                                 K_lay_q1 <- which(PFTlist_import == Ltri_cname[lay_list[l]])
                                                 K_lay_q2 <- which(PFTlist_import == Ltri_rname[lay_list[l]])
                                                 # extract data to be compute
                                                 localPFT1 <- as.vector(Ka_perm[,(j-hwA):(j+hwB),K_lay_q1])
                                                 localPFT2 <- as.vector(Ka_perm[,(j-hwA):(j+hwB),K_lay_q2])
                                                 # compute co-occurrence
                                                 coccu_res[l] <- cooccufun(localPFT1,localPFT2)
                                               }
                                               return(as.vector(coccu_res))
                                             }

    # put result to full matrix
    co_occurrence_res[proc_col_list_masked,] <- co_occurrence_result
    
    if(flag_co_occurrence){
      # write result to NetCDF-4 file
      ncdf4::ncvar_put(ncout, varid="co_occurrence", vals=as.double(co_occurrence_res), start=c(1,i,1), count=c(-1,1,-1))
    }
    rm(co_occurrence_result)

    # ###############################
    # Run Space4Time algorithm
    
    # loop through raster columns in parallel
    result <- foreach::foreach(j=proc_col_list_masked,
                               .combine=rbind,
                               .inorder=TRUE,
                               .noexport=noexport_list,
                               #.packages=c("h5","ncdf4"),
                               .verbose=FALSE) %dopar% {
                                 
                                 #for(j in proc_col_list_masked){ ### for debug
                                 #print(j) ### for debug
                                 
                                 # make big result array
                                 res_arr <- array(NA, dim=c(7,max(nK,nD),nT))
                                 
                                 localTemp <- matrix(as.vector(Za_perm[,(j-hwA):(j+hwB),]), ncol=nT)
                                 localComp <- matrix(as.vector(Ka_perm[,(j-hwA):(j+hwB),]), ncol=nC)
                                 
                                 # # run Space4Time using a dedicated function
                                 # s4t_fun_cmp(localTemp=localTemp, localComp=localComp, winsiz=winsiz, minPxls=minPxls, minDiffPxls=minDiffPxls, LtriIndex=LtriIndex)
                                 
                                 # to make sure that sum of rows is 1
                                 localComp_fix <- apply(localComp,1,FUN=function(x){1-sum(x,na.rm=T)})
                                 localComp <- matrix(data=cbind(localComp,localComp_fix), nrow=nrow(localComp), ncol=ncol(localComp)+1)
                                 # to make sure compositions are really (precisely) right
                                 localComp <- round(localComp,digits=4)

                                 # some PFTs might not be present in the 5*5 window
                                 # these must be identified and removed, as they cannot be predicted
                                 pftPres_check <- apply(localComp, 2, sum) > 0
                                 pftPres <- as.logical(c(PFTlist_import %in% PFTlist, FALSE) * pftPres_check)
                                 pftPos <- PFTlist %in% PFTlist_import[pftPres]

                                 # nPFT[i,j] <- sum(pftPres)

                                 #       # sometimes there is only 1 PFT in all 25 gridcells, making the problem 0-dimensional!
                                 #       # and this means the regression cannot be done
                                 #       # so only do regression if there are enough PFTs...
                                 #       if(sum(pftPres) > 1) {

                                 # sometimes there are only a small number of the
                                 #if(length(unique(apply(localComp, 1, function(z) paste0(z, collapse="")))) > minDiffPxls){
                                 if(length(unique(apply(localComp, 1, function(z) paste0(z, collapse="")))) > minDiffPxls & sum(pftPres) > 1){

                                   # prepare transformed variates #########################################################################

                                   # close composition (in case)
                                   #lC1 <- t(apply(localComp[,pftPres], 1, function(clas) clas/sum(clas))) # could put "drop=F" in here...
                                   lC1 <- t(apply(localComp, 1, function(clas) clas/(sum(clas)+0.000001)))
                                   # centre the columns (to be in the centre wrt new space)
                                   lC2 <- apply(lC1, 2, function(cc) cc-mean(cc))
                                   # remember col means for the subsequent predictions
                                   lCm <- apply(lC2, 2, mean)
                                   # decompose the resulting table
                                   lCsvd <- svd(lC2)

                                   # related to "enough PFTs", is there enough variability between observations?
                                   # if all obs have exactly the same composition, the regression is not possible
                                   # so only do the regression if there is some variability...
                                   if(sum(lCsvd$d) > 0){
                                     # nDim <- min(which((cumsum(lCsvd$d^2) / sum(lCsvd$d^2)) >= minVarExp))
                                     nDim <- min(which((cumsum(lCsvd$d^2) / sum(lCsvd$d^2)) >= 1))
                                     # nDim <- sum(pftPres)-1

                                     # store results to output object
                                     #sumSvdD[j-hw] <- sum(lCsvd$d)
                                     #nDimMat[j-hw] <- nDim
                                     res_arr[7,1,1] <- as.double(sum(lCsvd$d))
                                     #res_arr[8,1,1] <- as.integer(nDim)

                                     # if(nDim > 1) {

                                     LR <- lC2%*%lCsvd$v[,1:nDim]
                                     
                                     # create bogus composition dataset
                                     bogusComp <- matrix(0, nrow=nC+1, ncol=nC+1)
                                     diag(bogusComp) <- 1

                                     # remove absent PFTs from bogus predictor compositions & close the comps
                                     bogusC1 <- t(apply(bogusComp[pftPres,pftPres], 1, function(b) b/sum(b)))
                                     # center the columns as the training data were centered
                                     bogusC2 <- t(t(bogusC1)-lCm[pftPres])
                                     # so the adapted projector matrix works properly
                                     bogusC3 <- bogusC2%*%lCsvd$v[pftPres,1:nDim]

                                     # now loop for each for each regression
                                     for(iT in 1:dim(localTemp)[2]){

                                       if(sum(!is.na(localTemp[,iT])) >= minPxls){

                                         # Calculate the regression
                                         compRegr <- lm(lT~., data=data.frame(lT=localTemp[,iT], z=LR))
                                         
                                         # continue only if there are no NA in the estimated coefficients
                                         if(!anyNA(compRegr$coefficients)){
                                           # then do predictions for the log-normal approach
                                           bogusPred <- stats::predict(compRegr, newdata=data.frame(z=bogusC3))
                                           
                                           #X <- cbind(1,LR)
                                           X2pred <- cbind(1,bogusC3)
                                           
                                           #               # get the variance and covariance of the model
                                           #               vB <- (solve(t(X)%*%X))*sum((compRegr$residuals)^2)/(length(compRegr$residuals)-length(compRegr$coefficients))
                                           #               # use it to estimate the 'uncertainty' (expressed as variances)
                                           #               Sigma <- X2pred%*%vB%*%t(X2pred)
                                           
                                           vcv <- suppressWarnings(vcov(compRegr))
                                           Sigma <- X2pred%*%vcv%*%t(X2pred)
                                           
                                           # now store the target variables
                                           # but make sure appropriate temperatures go back to appropriate PFTs (as absent PFTs were removed)
                                           
                                           # Value of Z for pure PFTs
                                           predRes <- rep(NA, nK)
                                           predRes[pftPos] <- bogusPred
                                           #regRes[j-hw,iT,] <- predRes
                                           res_arr[5,c(1:nK),iT] <- predRes[1:nK]
                                           
                                           # Prediction error (as standard error) for each pure PFT Z
                                           predErr <- rep(NA, nK)
                                           predErr[pftPos] <- sqrt(diag(Sigma))
                                           #regErr[j-hw,iT,] <- predErr
                                           res_arr[6,c(1:nK),iT] <- predErr[1:nK]
                                           
                                           # Prediction of Z for the central pixel with its real PFT combination
                                           #regEst[j-hw,iT] <- compRegr$fitted.values[ceiling((winsiz^2)/2)]
                                           res_arr[3,1,iT] <- as.double(compRegr$fitted.values[ceiling((winsiz^2)/2)])
                                           
                                           # R square of the regression
                                           #rSqRes[j-hw,iT] <- summary(compRegr)$r.sq
                                           res_arr[4,1,iT] <- suppressWarnings(summary(compRegr)$r.sq)
                                           
                                           # and now for the transitions
                                           # only the PFTs identified in PFTlist are to be used
                                           
                                           # subset the covariance matrix
                                           Sigma1 <- matrix(NA,length(PFTlist),length(PFTlist))
                                           Sigma1[pftPos,pftPos] <- Sigma
                                           
                                           # calculate the difference in Z caused by going From one PFT to another
                                           #dZ <- round(outer(predRes[pftPos],predRes[pftPos],FUN='-')[LtriIndex],digits=10)
                                           dZ <- round(outer(predRes,predRes,FUN='-')[LtriIndex],digits=10)
                                           # propagate the error (as variances) taking into account the covariance terms
                                           dZvar <- round((outer(diag(Sigma1),diag(Sigma1),FUN='+') - 2*Sigma1)[LtriIndex],digits=10)
                                           # flag out those with zero error (may occur with identical compositions for 2 pfts)
                                           dZ[round(dZvar,digits=8)==0] <- NA
                                           dZvar[round(dZvar,digits=8)==0] <- NA
                                           # mask out low co-occurrence
                                           if(!keep_low_co_occurrence){
                                             dZ[which(as.vector(co_occurrence_res[j,]) < co_occurrence_threshold)] <- NA
                                             dZvar[which(as.vector(co_occurrence_res[j,]) < co_occurrence_threshold)] <- NA
                                           }
                                           
                                           # Store them
                                           #tPFTdelta[j-hw,iT,] <- dZ
                                           #tPFTerror[j-hw,iT,] <- sqrt(dZvar) # sqrt() to bring back to standard error
                                           res_arr[1,c(1:nD),iT] <- dZ
                                           res_arr[2,c(1:nD),iT] <- sqrt(dZvar)
                                         }
                                       }
                                     }
                                   }
                                 }
                                 return(as.vector(res_arr))
                               }
    # fill in resulting array with value
    #result_array[((1+hw):(xyOutDim[2]-hw)),,,] <- result
    result_array[proc_col_list_masked,,,] <- result
    rm(result)
    
    # save results to NetCDF-4 file
    #message("Write results to NetCDF-4 file")
    ncdf4::ncvar_put(ncout, varid="delta", vals=as.double(result_array[,1,c(1:nD),]), start=c(1,i,1,1), count=c(-1,1,-1,-1))
    if(flag_delta_error){
      ncdf4::ncvar_put(ncout, varid="delta_error", vals=as.double(result_array[,2,c(1:nD),]), start=c(1,i,1,1), count=c(-1,1,-1,-1))
    }
    if(flag_predicted){
      ncdf4::ncvar_put(ncout, varid="predicted", vals=as.double(result_array[,3,1,]), start=c(1,i,1), count=c(-1,1,-1))
    }
    if(flag_rsquared){
      ncdf4::ncvar_put(ncout, varid="rsquared", vals=as.double(result_array[,4,1,]), start=c(1,i,1), count=c(-1,1,-1))
    }
    if(flag_estimates){
      ncdf4::ncvar_put(ncout, varid="estimates", vals=as.double(result_array[,5,c(1:nK),]), start=c(1,i,1,1), count=c(-1,1,-1,-1))
      ncvar_put(ncout, varid="estimates_error", vals=as.double(result_array[,6,c(1:nK),]), start=c(1,i,1,1), count=c(-1,1,-1,-1))
    }
    if(flag_cumulated_variance){
      ncdf4::ncvar_put(ncout, varid="cumulated_variance", vals=as.double(result_array[,7,1,1]), start=c(1,i), count=c(-1,1))
    }
    # flush the data to NetCDF-4
    ncdf4::nc_sync(ncout)
    
    # remove unuseful objects and clean workspace
    rm(result_array)
    rm(co_occurrence_res)
    #invisible(gc(verbose=FALSE))
  }
  # Show progress bar
  utils::setTxtProgressBar(pb, which(proc_line_list_masked == i)/length(proc_line_list_masked))
}

close(pb)
#message("")
# stop cluster
parallel::stopCluster(cl)

# print usage time
if(verbose){
  message(paste(c("Space4Time computation ended at: "), Sys.time(), sep=""))
  message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
}

# write processing history and comment to output NetCDF-4 file
nc_history <- paste(date(), ": ", "s4t.r", " --", nc_hhist, sep="")
ncdf4::ncatt_put(ncout,0,"history",nc_history)
nc_comment <- paste(paste(date(), ": ", "Generated using ’Space4Time’ algorithm in R language", sep=""), nc_hist_k, nc_hist_z, nc_hist_output, nc_hist_mask, nc_hist_list, nc_hist_excluded, nc_hist_winsize, nc_hist_minPxls, nc_hist_minDiffPxls, nc_hist_keep, nc_hist_keep_thr, sep=" + ")  
ncdf4::ncatt_put(ncout,0,"comment",nc_comment)

# close the file, writing data to disk
ncdf4::nc_sync(ncout)
ncdf4::nc_close(ncout)

# clean workspace and exist
rm(list=ls())
invisible(gc(verbose=FALSE))
q("no")
