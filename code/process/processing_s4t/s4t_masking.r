#!/usr/local/bin/Rscript
# Title         : Space4Time NetCDF-4 masking
# Description   : Apply custom masking to Space4Time results
# Date          : Dec 2018
# Version       : 1.1
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# Usage example
# s4t_masking.r --input=output_space4time.nc --output=output_space4time_masked.nc --mask=input_mask.nc 
# --minmax=-1,1 --co_occurrence_thresholds=0.5 --rsquared_thresholds=0.2 -k --verbose


# load optparse library for argument parsing
options("repos"="http://cran.at.r-project.org/")
invisible(tryCatch(find.package("optparse"), error=function(e) install.packages("optparse", dependencies=TRUE)))
require(optparse, quietly=TRUE)

# read arguments
option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character", default=NULL, 
                        help="Input NetCDF file path", metavar="file"),
  optparse::make_option(c("-o", "--output"), type="character", default=NULL, 
                        help="Output NetCDF file path", metavar="file"),
  optparse::make_option(c("-m", "--mask"), type="character", default=NULL, 
                        help="Input mask file path", metavar="file"),
  optparse::make_option(c("-x", "--minmax"), type="character", default=NULL, 
                        help="Minimum and maximum values allowed for delta", metavar="character"),
  optparse::make_option(c("-c", "--co_occurrence_thresholds"), type="character", default=NULL, 
                        help="Thresholds for variable 'co_occurrence'. Should be a list of values in the interval 0-1 with length equal to one or to the same length of 'class_transition' dimension", metavar="character"),
  optparse::make_option(c("-r", "--rsquared_thresholds"), type="character", default=NULL, 
                        help="Thresholds for variable 'rsquared'. Should be a list of values in the interval 0-1 with length equal to one or to the same length of 'time' dimension", metavar="character"),
  optparse::make_option(c("-k","--keep_in_memory"), type="logical", default=FALSE, action="store_true",
                        help="Keep 'rsquared' layer in memory to reduce computation time"),
  optparse::make_option(c("--verbose"), type="logical", default=FALSE, action="store_true",
                        help="Verbose mode")
)

opt_parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(opt_parser)

# check if required arguments are supplied and point to existing files
if(is.null(opt$input)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (input).", call.=FALSE)
}
if(is.null(opt$output)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (output).", call.=FALSE)
}

# define R objects from options
nc_file <- normalizePath(path=opt$input, winslash="/", mustWork=TRUE)
nc_mask_arg <- opt$mask
#nc_mask_file <- normalizePath(path=opt$mask, winslash="/", mustWork=TRUE)
output_file <- opt$output
#output_file <- normalizePath(path=opt$output, winslash="/", mustWork=FALSE)

delta_minmax <- opt$minmax
co_occ_thr <- opt$co_occurrence_thresholds
rsq_thr <- opt$rsquared_thresholds

keep_in_memory <- opt$keep_in_memory
verbose <- opt$verbose

nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")

### for debug
# nc_file <- "/space/filipfe_data/cloud/space4time/s4t_global_avhrr_pm_test02/Space4Time_cloud_cfc_all_without_2012.nc"
# nc_mask_arg <- "/space/filipfe_data/cloud/space4time/s4t_global_avhrr_pm_test03/input/DEM_Global_0.05dd_elevation_mask.nc"
# output_file <- "/space/filipfe_data/cloud/space4time/s4t_global_avhrr_pm_test02/Space4Time_cloud_cfc_all_without_2012_mask_v13.nc"
# 
# #delta_minmax <- NULL
# delta_minmax <- "-1,1"
# co_occ_thr <- "0.5,0.5,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.0,1.0,0.5,0.5"
# rsq_thr <- "0.3"
# 
# keep_in_memory <- TRUE
# verbose <- TRUE
# nc_hhist <- ""

# ###################################
# load required libraries
# library(raster)
# library(ncdf4)
invisible(tryCatch(find.package("raster"), error=function(e) install.packages("raster", dependencies=TRUE)))
invisible(tryCatch(find.package("ncdf4"), error=function(e) install.packages("ncdf4", dependencies=TRUE)))
require(raster, quietly=TRUE)
require(ncdf4, quietly=TRUE)

message("Space4Time NetCDF masking operator v1.0 ...")

# ###################################
# import NetCDF file and check for consistency

# chek if output file already exists
output_file <- normalizePath(path=output_file, winslash="/", mustWork=FALSE)
if(file.exists(output_file)){
  stop(paste("Output file path '", output_file,"' already exists. Set another output file name", sep=""))
}

# check if mask file exists
if(!is.null(nc_mask_arg)){
  nc_mask_file <- normalizePath(path=nc_mask_arg, winslash="/", mustWork=FALSE)
  if(!file.exists(nc_mask_file)){
    stop(paste("Mask file '", nc_mask_file,"' does not exists. Set another mask file name", sep=""))
  }
}

# open NetCDF connection
nc_in <- ncdf4::nc_open(nc_file, readunlim=FALSE, write=FALSE)

# set list of variables to be processed
var_list <- c("delta","delta_error")

# check if NetCDF-4 has the required dimensions
if(sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_in$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_in$dim))) == 1 )) < 2)
  stop(paste("Input NetCDF file does not contain explicit dimension names: \n", "'lon' ['longitude','Longitude'], 'lat' ['latitude','Latitude']", sep=""))

if(is.null(var_list)){
  var_list <- names(nc_in$var)
  if(verbose){
    message(paste("No list of variables to process was supplied using the '--var' argument.\nImporting and processing all variables in input NetCDF:\n", paste(var_list, collapse=", "), sep=""))
  }
} else {
  var_list <- unlist(strsplit(var_list, ","))
}

# get dimensions position
nc_lon_pos_k <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
nc_lat_pos_k <- which(names(nc_in$dim) %in% c("lat","latitude","Latitude"))[1]

nc_n_row <- length(nc_in$dim[[nc_lat_pos_k]]$vals)
nc_n_col <- length(nc_in$dim[[nc_lon_pos_k]]$vals)

# check input NetCDF
# check if 'rsquared' and 'co_occurrence' variables exists in the input file
if(sum(as.numeric(var_list %in% names(nc_in$var))) < length(var_list))
  stop(paste("Some of the variables supplied with the '--var' argument are not contained in input NetCDF file: \n", paste(var_list[which((var_list %in% names(nc_in$var)) == FALSE)], collapse=', '), sep=''))

# check if variables exists in input NetCDF
if(sum(as.numeric(var_list %in% names(nc_in$var))) < length(var_list))
  stop(paste("Some of the variables supplied with the '--var' argument are not contained in input NetCDF file: \n", paste(var_list[which((var_list %in% names(nc_in$var)) == FALSE)], collapse=', '), sep=''))

# check if 'delta' and 'delta_error' variables are set for masking
if(sum(as.numeric(var_list %in% c("delta","delta_error"))) < 2)
  warning("List of variables to be masked does not contain the variables required for next Space4Time aggregate step: 'delta' and 'delta_error'")

# get 'class_transition' dimension size
clt_dim_size <- nc_in$dim[[which(names(nc_in$dim) == "class_transition")]]$len
# get 'rsquared' third ('time') dimension size
time_dim_size <-  nc_in$dim[[which(names(nc_in$dim) == "time")]]$len

# get class transition names
if(ncdf4::ncatt_get(nc_in, "class_transition", attname="class_transition_list")$hasatt){
  class_tl <- unlist(strsplit(ncdf4::ncatt_get(nc_in, "class_transition", attname="class_transition_list")$value, split=", "))
} else {
  class_tl <- ""
}

# ###################################
# check input arguments

# check '--minmax' argument
if(!is.null(delta_minmax)){
  if(length(unlist(strsplit(delta_minmax, split=","))) != 2){
    stop("Argument '--minmax' should be comma separeted and of length two (example: '-1,1')")
  }
}

# check '--co_occurrence_thresholds' argument
if(!is.null(co_occ_thr)){
  # check if argument is single or a list
  if(grepl(",", co_occ_thr)){
    co_occ_thr_in <- unlist(strsplit(co_occ_thr, split=","))
  } else {
    co_occ_thr_in <- co_occ_thr
  }
  if(length(which(is.na(suppressWarnings(as.double(co_occ_thr_in))))) > 0){
    stop("Argument '--co_occurrence_thresholds' only allows numerical values in the interval '0-1'")
  } else {
    if(length(co_occ_thr_in) != 1 & length(co_occ_thr_in) != clt_dim_size){
      stop("Argument '--co_occurrence_thresholds' should be comma separeted and length equal to one or to length of 'class_transition' dimension (example: '0.5,0.3,0.45')")
    }
    if(sum(as.integer(as.double(co_occ_thr_in) < 0), as.integer(as.double(co_occ_thr_in) > 1)) > 0){
      stop("Argument '--co_occurrence_thresholds' only allows numerical values in the interval '0-1'")
    }
  }
}

# check '--rsquared_thresholds' argument
if(!is.null(rsq_thr)){
  # check if argument is single or a list
  if(grepl(",", rsq_thr)){
    rsq_thr_in <- unlist(strsplit(rsq_thr, split=","))
  } else {
    rsq_thr_in <- rsq_thr
  }
  if(length(which(is.na(suppressWarnings(as.double(rsq_thr_in))))) > 0){
    stop("Argument '--rsquared_thresholds' only allows numerical values in the interval '0-1'")
  } else {
    if(length(rsq_thr_in) != 1 & length(rsq_thr_in) != time_dim_size){
      stop("Argument '--rsquared_thresholds' should be comma separated and length equal to one or to length of 'time' dimension (example: '0.5,0.3,0.45')")
    }
    if(sum(as.integer(as.double(rsq_thr_in) < 0), as.integer(as.double(rsq_thr_in) > 1)) > 0){
      stop("Argument '--rsquared_thresholds' only allows numerical values in the interval '0-1'")
    }
  }
}

# check if mask extent and resolution are the same of input NetCDF file
if(!is.null(nc_mask_arg)){
  mask_compare <- suppressWarnings(raster::compareRaster(stack(nc_file)[[1]],raster(nc_mask_file), extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))
  if(!mask_compare){
    stop("Input NetCDF file and mask raster characteristics differs")
  }
}

# create output folder
dir.create(dirname(output_file), showWarnings=FALSE, recursive=TRUE)

# #####################
# set variables from input

# set 'delta_min' and 'delta_max'
if(!is.null(delta_minmax)){
  delta_min <- min(as.double(unlist(strsplit(delta_minmax, split=","))))
  delta_max <- max(as.double(unlist(strsplit(delta_minmax, split=","))))
}

# set 'co_occurrence_thresholds'
if(!is.null(co_occ_thr)){
  if(length(co_occ_thr_in) == 1){
    co_occurrence_thresholds <- rep(as.double(co_occ_thr_in), clt_dim_size)
  } else {
    co_occurrence_thresholds <- as.double(co_occ_thr_in)
  }
}

# set 'rsquared_thresholds'
if(!is.null(rsq_thr)){
  if(length(rsq_thr_in) == 1){
    rsquared_thresholds <- rep(as.double(rsq_thr_in), time_dim_size)
  } else {
    rsquared_thresholds <- as.double(rsq_thr_in)
  }
}

# #####################
# import mask dataset

# check if latitude dimension is sorted north-to-south
nc_lat_dim_in <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lat_pos_k])
nc_lat_dim_ord <- sort(nc_lat_dim_in, decreasing=TRUE)
if(as.integer(sum(as.numeric(nc_lat_dim_in == nc_lat_dim_ord))) < length(nc_lat_dim_ord)){
  nc_lat_reverse <- TRUE
} else {
  nc_lat_reverse <- FALSE
}

# import mask file
if(!is.null(nc_mask_arg)){
  nc_mask <- ncdf4::nc_open(nc_mask_file, readunlim=FALSE)
  #if(nc_mask$nvars > 1 | nc_mask$ndims > 2 | sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_mask$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_mask$dim))) == 1 )) < 2){
  if( nc_mask$ndims > 2 | sum(as.numeric(sum(as.numeric(c("lon","longitude","Longitude") %in% names(nc_mask$dim))) == 1 ) + as.numeric(sum(as.numeric(c("lat","latitude","Latitude") %in% names(nc_mask$dim))) == 1 )) < 2){
      stop("Input 'mask' file does not contain the correct number of dimensions or explicit dimension names: 'lon' ['longitude','Longitude'], 'lat' ['latitude','Latitude']")
  } else {
    # check if latitude dimension is sorted north-to-south
    mask_nc_lat_dim_in <- ncdf4::ncvar_get(nc_mask, names(nc_mask$dim)[which(names(nc_mask$dim) %in% c("lat","latitude","Latitude"))[1]])
    mask_nc_lat_dim_ord <- sort(mask_nc_lat_dim_in, decreasing=TRUE)
    if(as.integer(sum(as.numeric(mask_nc_lat_dim_in == mask_nc_lat_dim_ord))) < length(mask_nc_lat_dim_ord)){
      mask_nc_lat_reverse <- TRUE
    } else {
      mask_nc_lat_reverse <- FALSE
    }
    mask_nc_lat_reverse_check <- as.logical(!(mask_nc_lat_reverse == nc_lat_reverse))
    
    # import mask data
    r_mask <- as.matrix(ncdf4::ncvar_get(nc_mask))
    # flip mask if latitude is bottomup
    if(mask_nc_lat_reverse_check){
      #r_mask <- apply(r_mask,2,rev)
      r_mask <- as.matrix(rev(as.data.frame(r_mask)))
    }
    # if(as.integer(sum(as.numeric(r_mask %in% c(0,1)))) < length(r_mask)){
    #   stop("Input 'mask' file has values different from '0' and '1'.")
    # }
  }
} else {
  # create full raster mask if mask file is not provided
  r_mask <- matrix(data=1, nrow=nc_n_col, ncol=nc_n_row)
}

# print messages
if(verbose){
  message(paste("Processing file: ", nc_file, sep=""))
  message(paste("Results are saved to file: ", output_file, sep=""))
  if(!is.null(nc_mask_arg)){
    message(paste("Using mask file: ", nc_mask_file, sep=""))
  } else {
    message("Not using external mask file")
  }
}

# print messages on console
if(verbose){
  if(!is.null(delta_minmax)){
    message(paste("Masking out 'delta' values exceeding '--minmax' values:\n ", paste("'", delta_min, "','", delta_max, "'", sep=""), sep=""))
  }
  if(!is.null(co_occ_thr)){
    message(paste("Masking out 'delta' values exceeding '--co_occurrence' values:\n ", paste(paste(c(1:clt_dim_size), " ", class_tl, ": '", co_occurrence_thresholds, "'", sep=""), collapse=", "), sep=""))
  }
  if(!is.null(rsq_thr)){
    message(paste("Masking out 'delta' values exceeding '--rsquared' values:\n ", paste(paste("time ", c(1:time_dim_size), ": '", rsquared_thresholds, "'", sep=""), collapse=", "), sep=""))
  }
}

# #####################
# create output NetCDF file

var_pos <- which(names(nc_in$var) == var_list[1])
n_dims <- nc_in$var[[var_pos]]$ndims

# get variable dimension names
var_dim_names <- rep(NA, n_dims)
for(d in 1:n_dims){
  var_dim_names[d] <- nc_in$var[[var_pos]]$dim[[d]]$name
}

# get position of dimensions different from "lon' and "lat"
dims_cycle <- which(var_dim_names != "lon" & var_dim_names != "lat" & var_dim_names != "longitude" & var_dim_names != "latitude" & var_dim_names != "Longitude" & var_dim_names != "Latitude")
# set chunksize
dim_chuncksize <- nc_in$var[[var_pos]]$varsize
dim_chuncksize[dims_cycle] <- rep(1, length(dims_cycle))

# create new variable
nc_var_new <- ncdf4::ncvar_def(name=names(nc_in$var)[var_pos], longname=nc_in$var[[var_pos]]$longname, units=nc_in$var[[var_pos]]$units, missval=nc_in$var[[var_pos]]$missval, prec=nc_in$var[[var_pos]]$prec, dim=nc_in$var[[var_pos]]$dim, chunksizes=dim_chuncksize, compression=9)
nc_var_list <- list(nc_var_new)

if(length(var_list) > 1){
  for(u in 2:length(var_list)){
    var_pos <- which(names(nc_in$var) == var_list[u])
    n_dims <- nc_in$var[[var_pos]]$ndims
    
    # get position of dimensions different from "lon' and "lat"
    var_dim_names <- rep(NA, n_dims)
    for(d in 1:n_dims){
      var_dim_names[d] <- nc_in$var[[var_pos]]$dim[[d]]$name
    }
    dims_cycle <- which(var_dim_names != "lon" & var_dim_names != "lat" & var_dim_names != "longitude" & var_dim_names != "latitude" & var_dim_names != "Longitude" & var_dim_names != "Latitude")
    # set chunksize
    dim_chuncksize <- nc_in$var[[var_pos]]$varsize
    dim_chuncksize[dims_cycle] <- rep(1, length(dims_cycle))
    
    # create new variable
    nc_var_new <- ncdf4::ncvar_def(name=names(nc_in$var)[var_pos], longname=nc_in$var[[var_pos]]$longname, units=nc_in$var[[var_pos]]$units, missval=nc_in$var[[var_pos]]$missval, prec=nc_in$var[[var_pos]]$prec, dim=nc_in$var[[var_pos]]$dim, chunksizes=dim_chuncksize, compression=9)
    nc_var_list <- c(nc_var_list, list(nc_var_new))
  }
}

# create output NetCDF-4 file
ncout <- ncdf4::nc_create(output_file, vars=nc_var_list, force_v4=TRUE)

# set NetCDF-4 variable attributes
# put additional attributes into dimension and data variables
ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
ncdf4::ncatt_put(ncout,"lon","axis","X")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"lat","axis","Y")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"time","standard_name","time")
ncdf4::ncatt_put(ncout,"time","calendar","standard")
if(ncdf4::ncatt_get(nc_in, "class_transition", attname="class_transition_list")$hasatt){
  ncdf4::ncatt_put(ncout,"class_transition","class_transition_list", ncdf4::ncatt_get(nc_in, "class_transition", attname="class_transition_list")$value)
}

# set dimensions long names
# get dimension longnames
ncdf4::ncatt_put(ncout,"lon","long_name","Longitude")
ncdf4::ncatt_put(ncout,"lat","long_name","Latitude")
ncdf4::ncatt_put(ncout,"class_transition","long_name","cover_class_transitions")
nc_out_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[which(names(nc_in$dim) == "time")], "long_name")$value
ncdf4::ncatt_put(ncout,"time","long_name",nc_out_time_lname)

# set global attributes to output NetCDF-4 file
if(ncdf4::ncatt_get(nc_in, 0, "title")$hasatt){
  ncdf4::ncatt_put(ncout,0,"title", ncdf4::ncatt_get(nc_in, 0, "title")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "references")$hasatt){
  ncdf4::ncatt_put(ncout,0,"references", ncdf4::ncatt_get(nc_in, 0, "references")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "summary")$hasatt){
  ncdf4::ncatt_put(ncout,0,"summary", ncdf4::ncatt_get(nc_in, 0, "summary")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "keywords")$hasatt){
  ncdf4::ncatt_put(ncout,0,"keywords", ncdf4::ncatt_get(nc_in, 0, "keywords")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "Conventions")$hasatt){
  ncdf4::ncatt_put(ncout,0,"Conventions", ncdf4::ncatt_get(nc_in, 0, "Conventions")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "s4t_winsize")$hasatt){
  ncdf4::ncatt_put(ncout,0,"s4t_winsize", as.integer(ncdf4::ncatt_get(nc_in, 0, "s4t_winsize")$value), prec="short")
}
if(ncdf4::ncatt_get(nc_in, 0, "Input_class_list")$hasatt){
  ncdf4::ncatt_put(ncout,0,"Input_class_list", ncdf4::ncatt_get(nc_in, 0, "Input_class_list")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "Analyzed_class_list")$hasatt){
  ncdf4::ncatt_put(ncout,0,"Analyzed_class_list", ncdf4::ncatt_get(nc_in, 0, "Analyzed_class_list")$value)
}
if(ncdf4::ncatt_get(nc_in, 0, "Class_transition_list")$hasatt){
  ncdf4::ncatt_put(ncout,0,"Class_transition_list", ncdf4::ncatt_get(nc_in, 0, "Class_transition_list")$value)
}

# set parameters for NetCDF history
nc_history_par <- c(" + ")
if(!is.null(nc_mask_arg)){
  nc_history_par <- paste(nc_history_par, "mask_file=", nc_mask_file, sep="")
}
if(!is.null(delta_minmax)){
  nc_history_par <- paste(nc_history_par, "minmax=", delta_minmax, sep="")
}
if(!is.null(co_occ_thr)){
  nc_history_par <- paste(nc_history_par, " + co_occurrence_thresholds=’", paste(paste(c(1:clt_dim_size), " ", class_tl, ": '", co_occurrence_thresholds, "'", sep=""), collapse=", "), "’", sep="")
}
if(!is.null(rsq_thr)){
  nc_history_par <- paste(nc_history_par, " + rsquared_thresholds=’", paste(paste("time ", c(1:time_dim_size), ": '", rsquared_thresholds, "'", sep=""), collapse=", "), "’", sep="")
}

# #####################
# set variable loop

# get position of dimensions different from "lon' and "lat"
var_dim_names <- rep(NA, n_dims)
for(d in 1:n_dims){
  var_dim_names[d] <- nc_in$var[[var_pos]]$dim[[d]]$name
}
dims_cycle <- which(var_dim_names != "lon" & var_dim_names != "lat" & var_dim_names != "longitude" & var_dim_names != "latitude" & var_dim_names != "Longitude" & var_dim_names != "Latitude")

# create list of dimensions
dmn_0 <- as.numeric(1)
dmn_00 <- as.numeric(1)
for(n in dims_cycle){
  obj_name <- paste("dmn_", n, sep="")
  assign(obj_name, 1:nc_in$var[[var_pos]]$varsize[n])
}
lista <- ls(pattern = "dmn_")
# fix dimension ordering
#lista <- c("dmn_0","dmn_00","dmn_4","dmn_3")
expand_cmd <- paste("expand.grid(", paste(lista, collapse=","), ")", sep="")

# create list of layers to be imported
nc_start_list <- eval(parse(text=expand_cmd))

# reorder list by 'class_transition' column
if(!is.null(co_occ_thr)){
 nc_start_list <- nc_start_list[order(nc_start_list$Var3),] 
}

data_count <- c(rep(-1, 2), rep(1, n_dims-2))

# get 'co_occurrence' third ('class_transition') dimension size
co_dim_size <- nc_in$dim[[which(var_dim_names == "class_transition")]]$len
# get 'rsquared' third ('time') dimension size
rsq_dim_size <-  nc_in$dim[[which(var_dim_names == "time")]]$len

# get latitide dimension position (for parallel implementation)
#lat_dim_pos <- which(var_dim_names == "lat" | var_dim_names == "latitude" | var_dim_names == "Latitude")
#lat_dim_list <- as.vector(1:as.integer(nc_in$dim[[lat_dim_pos]]$len))

# remove unuseful objects
suppressWarnings(rm(list=c("dim_chuncksize","dims_cycle","dmn_0","dmn_00","dmn_3","dmn_4","expand_cmd","lista","mask_compare","mask_nc_lat_dim_in","mask_nc_lat_dim_ord","mask_nc_lat_reverse","mask_nc_lat_reverse_check","n")))
suppressWarnings(rm(list=c("nc_file","nc_lat_dim_in","nc_lat_dim_ord","nc_lat_pos_k","nc_lat_reverse","nc_lon_pos_k","nc_mask","nc_mask_arg","nc_mask_file","nc_n_col","nc_n_row","nc_var_list","nc_var_new","nc_out_time_lname")))
suppressWarnings(rm(list=c("clt_dim_size","co_dim_size","co_occ_thr_in","d","n_dims","obj_name","output_file","rsq_dim_size","rsq_thr_in","time_dim_size","u","var_dim_names","var_pos")))

# #####################
# mask variables

# store in memory 'co_occurrence' and 'rsquared' if '--keep_in_memory' argument is selected
if(keep_in_memory){
  if(verbose){
    message("Reading and keeping in memory variable 'rsquared")
  }
  if(!is.null(rsq_thr)){
    nc_data_rsq_full <- ncdf4::ncvar_get(nc_in, varid="rsquared")
  }
}
# clean workspace
invisible(gc(verbose=FALSE))

# set index for 'class_transition' and 'time' dimensions
cls <- 0
tm <- 0

# create loop processing list
proc_list <- as.vector(1:length(nc_start_list[,1]))
### for debug
#proc_list <- proc_list[1:30]

if(verbose){
  message(paste("Masking variables '", paste(var_list, collapse="', '"), "' started at: ", Sys.time(), sep=""))
  message(paste("Number of layers to be processed is: ", length(proc_list), sep=""))
}

ptm <- proc.time()

pb <- utils::txtProgressBar(style=3)

# mask data
for(m in proc_list){
  # duplicate input mask information
  r_mask_apply <- r_mask
  data_start <- as.numeric(nc_start_list[m,])
  # if(verbose){
  #   message(paste("   ","Processing layer ", m, "/", length(nc_start_list[,1]), sep=""))
  #   #message(paste("Processing layer ", m, "/", length(nc_start_list[,1])," - START = ", paste(data_start, collapse=","), " - END - ", paste(data_count, collapse=","), sep=""))
  # }
  # mask co_occurrence
  if(!is.null(co_occ_thr)){
    # import and update 'co_occurrence' layer if changed
    if(cls != nc_start_list[m,3]){
      #print(paste("Reading co_occurrence layer: ", nc_start_list[m,3], sep=""))
      # import 'co_occurrence' variable
      nc_data_coc <- as.matrix(ncdf4::ncvar_get(nc_in, varid="co_occurrence", start=data_start[1:3], count=data_count[1:3]))
      # refine raster mask
      mpos_coc <- which(nc_data_coc < co_occurrence_thresholds[nc_start_list[m,3]])
      rm(nc_data_coc)
      # update index
      cls <- nc_start_list[m,3]
    }
    r_mask_apply[mpos_coc] <- 0
  }

  # mask rsquared
  if(!is.null(rsq_thr)){
    if(keep_in_memory){
      nc_data_rsq <- as.matrix(nc_data_rsq_full[,,nc_start_list[m,4]])
      mpos_rsq <- which(nc_data_rsq < rsquared_thresholds[nc_start_list[m,4]])
      rm(nc_data_rsq)
    } else {
      # import and update 'co_occurrence' layer if changed
      if(tm != nc_start_list[m,4]){
        #print(paste("Reading rsquared layer: ", nc_start_list[m,4], sep=""))
        # import 'rsquared' variable
        nc_data_rsq <- as.matrix(ncdf4::ncvar_get(nc_in, varid="rsquared", start=data_start[c(1:2,4)], count=data_count[c(1:2,4)]))
        # refine raster mask
        mpos_rsq <- which(nc_data_rsq < rsquared_thresholds[nc_start_list[m,4]])
        rm(nc_data_rsq)
        # update index
        tm <- nc_start_list[m,4]
      }
    }
    r_mask_apply[mpos_rsq] <- 0
  }

  # process 'delta' variable
  #print(paste("Read 'delta' variable"))
  nc_data <- as.matrix(ncdf4::ncvar_get(nc_in, varid="delta", start=data_start, count=data_count))
  
  #message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
  #ptm <- proc.time()

  # apply 'minmax' thresholding
  if(!is.null(delta_minmax)){
    mpos_mm <- which(nc_data < delta_min | nc_data > delta_max)
    r_mask_apply[mpos_mm] <- 0
  }
  
  # mask variable
  nc_data[which(r_mask_apply == 0)] <- NA
  ncdf4::ncvar_put(ncout, varid="delta", vals=nc_data, start=data_start, count=data_count)

  # process 'delta_error' variable
  #print(paste("Read 'delta_error' variable"))
  nc_data <- as.matrix(ncdf4::ncvar_get(nc_in, varid="delta_error", start=data_start, count=data_count))
  
  #message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
  #ptm <- proc.time()

  # mask variable
  nc_data[which(r_mask_apply == 0)] <- NA
  ncdf4::ncvar_put(ncout, varid="delta_error", vals=nc_data, start=data_start, count=data_count)

  # Show progress bar
  utils::setTxtProgressBar(pb, which(proc_list == m)/length(proc_list))
}

close(pb)

# print usage time
if(verbose){
  message(paste(c("Variables masking ended at: "), Sys.time(), sep=""))
  message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
}

# write processing comments and history to output NetCDF-4 file
if(ncdf4::ncatt_get(nc_in, varid=0, attname="history")$hasatt){
  nc_history_old <- c(ncdf4::ncatt_get(nc_in, varid=0, attname="history")$value)
} else {
  nc_history_old <- ""
}
nc_history <- paste(paste(date(), ": ", "s4t_masking.r", " --", nc_hhist, sep=""), nc_history_old, sep="\n")
ncdf4::ncatt_put(ncout,0,"history",nc_history)

if(ncdf4::ncatt_get(nc_in, varid=0, attname="comment")$hasatt){
  nc_comment_old <- c(ncdf4::ncatt_get(nc_in, varid=0, attname="comment")$value)
} else {
  nc_comment_old <- ""
}
nc_comment <- paste(paste(date(), ": ", "Generated using ’Space4Time’ masking in R language", nc_history_par, sep=""), nc_comment_old, sep=" ++ ")
ncdf4::ncatt_put(ncout,0,"comment",nc_comment)

# close the file, writing data to disk
ncdf4::nc_sync(ncout)
ncdf4::nc_close(ncout)

# clean workspace and exist
rm(list=ls())
invisible(gc(verbose=FALSE))
q("no")
