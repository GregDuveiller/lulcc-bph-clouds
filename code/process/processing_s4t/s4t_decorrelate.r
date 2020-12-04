#!/usr/local/bin/Rscript
# Title         : Space4Time aggregation
# Description   : Perform spatial aggregation of results generated using Space4Time algorithm
# Date          : Dec 2018
# Version       : 1.1
# Licence       : GPL v3
# Authors       : Federico Filipponi, Gregory Duveiller
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ########################################################

# Usage example
# s4t_decorrelate.r --input=output_space4time_masked.nc --output=output_space4time_masked_aggregate.nc --loop='line' -w 5 --q 8 --verbose

# load optparse library for argument parsing
options("repos"="http://cran.at.r-project.org/")
invisible(tryCatch(find.package("optparse"), error=function(e) install.packages("optparse", dependencies=TRUE)))
require(optparse, quietly=TRUE)

# read arguments
option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character", default=NULL, 
                        help="Input file path (generated using Space4Time)", metavar="file"),
  optparse::make_option(c("-o", "--output"), type="character", default=NULL, 
                        help="Output NetCDF-4 file path", metavar="file"),
  #optparse::make_option(c("-r", "--resolution"), type="double", default=NULL, 
  #                      help="Target resolution (same CRS as 'input' file)", metavar="double"),
  optparse::make_option(c("-w", "--winsize"), type="integer", default=NULL, 
                        help="Size of the moving window (in pixels)  [default get value from input file]", metavar="integer"),
  optparse::make_option(c("-l", "--loop"), type="character", default="auto", 
                        help="Parellel loop type ('auto', 'pixel' or 'line') [default 'auto']", metavar="character"),
  optparse::make_option(c("-q", "--q_parallelism"), type="integer", default=-1, 
                        help="Set the maximum parallelism used for the computation. If not set all available cores are used for the analysis", metavar="integer"),
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
# if(is.null(opt$resolution)){
#   optparse::print_help(opt_parser)
#   stop("At least one argument must be supplied (resolution).", call.=FALSE)
# }

# define R objects from options
input_file <- normalizePath(path=opt$input, winslash="/", mustWork=TRUE)
output_file <- normalizePath(path=opt$output, winslash="/", mustWork=FALSE)

#target_res <- opt$resolution
winsiz <- opt$winsize
loop <- opt$loop

cores <- as.integer(opt$q_parallelism)
verbose <- opt$verbose

nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")

# ### for debug
# input_file <- normalizePath('/space/filipfe_data/space4time/cloud/aqua_europe/results/ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0_cmask_2003_2014_monthly_avg_climatology_s4t_masked.nc', winslash="/", mustWork=TRUE)
# output_file <- normalizePath('/space/filipfe_data/cloud/space4time/s4t_global_avhrr_pm_test01/Space4Time_europe_aggregate11.nc', winslash="/", mustWork=FALSE)
# #
# # target_res <- 1.0
# winsiz <- 5
# cores <- 10
# loop <- "auto"
# verbose <- TRUE
# nc_hhist <- ""

# ###################################
# load required libraries
# library(raster)
# library(ncdf4)
# library(doParallel)
invisible(tryCatch(find.package("raster"), error=function(e) install.packages("raster", dependencies=TRUE)))
invisible(tryCatch(find.package("ncdf4"), error=function(e) install.packages("ncdf4", dependencies=TRUE)))
invisible(tryCatch(find.package("doParallel"), error=function(e) install.packages("doParallel", dependencies=TRUE)))
require(raster, quietly=TRUE)
require(ncdf4, quietly=TRUE)
require(doParallel, quietly=TRUE)

message("Running Space4Time aggregator v1.1 ...")

# ###################################
# check arguments

# check loop argument
if(!loop %in% c("auto","line","pixel"))
  stop("Loop argument should be one of the following: 'auto' ,'line', 'pixel'")

# check if output file exists
if(file.exists(output_file)){
  stop(paste("Output file path '", output_file,"' already exists. Set another output file name", sep=""))
}

# read input NetCDF-4 file
nc_in <- ncdf4::nc_open(input_file, readunlim=FALSE, write=FALSE)

# check if input file contains variables 'delta' and 'delta_error'
if(sum(as.numeric(c("delta", "delta_error") %in% names(nc_in$var))) != 2)
  stop("Input file should contains variables 'delta' and 'delta_error'")

# check if 'delta' and 'delta_error' have the same dimension size
if(sum(as.numeric(nc_in$var[[which(names(nc_in$var) == "delta")]]$varsize != nc_in$var[[which(names(nc_in$var) == "delta_error")]]$varsize)) > 0)
  stop("Variables 'delta' and 'delta_error' in 'input' file have not the same dimension size")

# get 'winsize' for input NetCDF-4 file global attributes
if(ncdf4::ncatt_get(nc_in, varid=0, "s4t_winsize")$hasatt){
  winsize <- as.integer(ncdf4::ncatt_get(nc_in, varid=0, "s4t_winsize")$value)
  if(!is.null(winsiz)){
    if(winsize != winsiz){
      warning(paste("Size of the moving window set using the 'winsize' argument differs from value detected in the input NetCDF file: set = '", winsiz, "', found = '", winsize,"'\nContinue processing using moving window of size: '", winsize, "'", sep=""))
    }
  }
} else {
  if(is.null(winsiz)){
    stop("Size of the moving window was not set and cannot be found in the input NetCDF file\nUse the 'winsize' argument to define the moving window value used in the Space4Time algorithm")
  } else {
    winsize <- as.integer(winsiz)
  }
}

# set number of dimensions for variable 'delta'
var_ndims <- length(nc_in$var[[which(names(nc_in$var) == "delta")]]$varsize)
# check if number of dimensions for variable 'delta' is different from 3 or 4
if(var_ndims != 3 & var_ndims != 4)
  stop(paste("Not supported number of dimensions for variable 'delta' in the input file: '", var_ndims, "'", sep=""))

# set list of layers to be processed
if(var_ndims == 3){
  llist <- data.frame(dat=c(1:nc_in$var[[which(names(nc_in$var) == "delta")]]$varsize[3]))
  names(llist) <- c(nc_in$var[[which(names(nc_in$var) == "delta")]]$dim[[3]]$name)
} else {
  ll1 <- nc_in$var[[which(names(nc_in$var) == "delta")]]$varsize[3]
  ll2 <- nc_in$var[[which(names(nc_in$var) == "delta")]]$varsize[4]
  llist <- expand.grid(1:ll1,1:ll2)
  names(llist) <- c(nc_in$var[[which(names(nc_in$var) == "delta")]]$dim[[3]]$name, nc_in$var[[which(names(nc_in$var) == "delta")]]$dim[[4]]$name)
}

# check if target resolution is different from source resolution
source_raster_ref <- suppressWarnings(raster::raster(input_file, varname="delta"))
source_res_get <- raster::res(source_raster_ref)
source_res <- round(source_res_get[1], digits=4)
if(length(source_res_get > 1)){
  source_res2 <- round(source_res_get[2], digits=4)
  if(source_res != source_res2){
    warning(paste("Input non-square pixels (spatial resolution: ", source_res, " x ", source_res2, ")\nGoing on using source spatial resolution of ", source_res, sep=""))
  }
}
# get number of rows and columns
nRows <- as.integer(nrow(source_raster_ref))
nCols <- as.integer(ncol(source_raster_ref))

# set target resolution (from source resolution and winsize)
target_res <- as.double(source_res * winsize)

# set scale factor to be used
scale_fact <- as.integer(round(target_res/source_res))
# if(scale_fact < 3)
#   stop("Scale factor is lower than 3. Run again selecting a coarser target resolution")

if(source_res >= target_res)
  stop("Target resolution set using 'resolution' argument is equal or greater 'input' file resolution")
if(round((target_res/source_res) - round(target_res/source_res), digits = 4) != 0)
  stop(paste("Target resolution '", target_res, "' is not multiple of source resolution '", source_res, "'\nThis is currently not supported, set target resolution to a multiple value of source resolution", sep=""))
source_ext <- raster::extent(source_raster_ref)
source_coords <- coordinates(source_raster_ref)

# create output folder
dir.create(dirname(output_file), showWarnings=FALSE, recursive=TRUE)

if(verbose){
  message(paste("Processing file: ", input_file, sep=""))
  message(paste("Results are saved to file: ", output_file, sep=""))
  message(paste("Using a moving window of size:", winsize, "x", winsize, sep=" "))
  message(paste("Source spatial resolution is: ", source_res, sep=""))
  message(paste("Target spatial resolution is: ", target_res, sep=""))
  message(paste("Scale factor is: ", scale_fact, sep=""))
}

# ###########################
# Setup analysis

# define off check function
is.odd <- function(x){
  x %% 2 != 0
}

# set reading loop bounds
RsfB <- as.integer(floor(scale_fact/2))
CsfB <- as.integer(floor(scale_fact/2))
if(is.odd(scale_fact)){
  RsfA <- as.integer(RsfB)
  CsfA <- as.integer(CsfB)
} else {
  warning("Target resolution is not a odd multiple of source resolution.\nGoing on however")
  RsfA <- as.integer(RsfB - 1)
  CsfA <- as.integer(CsfB - 1)
}
Ssf <- as.integer(scale_fact*scale_fact)

# define output raster
output_raster <- raster(xmn=source_ext@xmin, xmx=source_ext@xmax, ymn=source_ext@ymin, ymx=source_ext@ymax, resolution=target_res, vals=NULL)

# set output coordinates list
output_coords <- coordinates(output_raster)
# set number of cells
nCells <- as.integer(nrow(output_raster)*ncol(output_raster))

# define reference pixel coordinates for new grid computation
ref_pixel_row <- as.integer(1 + RsfA)
ref_pixel_col <- as.integer(1 + CsfA)

# define rows and columns positions of pixels to be processed
Clist <- as.integer(seq(from=ref_pixel_row, by=scale_fact, length.out=ncol(output_raster)))
Rlist <- as.integer(seq(from=ref_pixel_col, by=scale_fact, length.out=nrow(output_raster)))

# only needed for parallel pixel loop
# set pixels to be processed
plist <- expand.grid(c=Clist,r=Rlist)
# create list of pixels to be processed
pix_list <- as.vector(1:length(plist$r))

# set loop mode based on scale factor
if(loop == "auto"){
  if(scale_fact > 25){
    loop <- "pixel"
  } else {
    loop <- "line"
  }
}
if(loop == "line" & scale_fact > 25){
  warning(paste("Selected loop mode '", loop, "' could run slowly for scale factor of '", scale_fact, "'.\nChange loop mode to 'auto' for better performances", sep=""))
}
if(loop == "pixel" & scale_fact <= 25){
  warning(paste("Selected loop mode '", loop, "' could run slowly for scale factor of '", scale_fact, "'.\nChange loop mode to 'auto' for better performances", sep=""))
}

if(verbose){
  message(paste("Loop mode is set to: ", loop, sep=""))
}

# ##########################
# define functions for the analysis

agr.decor <- function(y,s,R){
  # function to aggregate while decorrelating
  #
  # Inputs: 
  #   y    vector of values to be aggregated
  #   s    vector of uncertainties to be used as weights
  #   R    matrix providing correlation info
  
  rnNA <- !is.na(s)
  N <- as.double(sum(rnNA)/length(s))
  
  D <- diag(s)
  S <- D[rnNA,rnNA]%*%R[rnNA,rnNA]%*%t(D[rnNA,rnNA])
  
  invS <- tryCatch({invS <- chol2inv(chol(S))}, error=function(cond){return(invS=NULL)})
  
  if(!is.null(invS)){
    A <- 1/sum(as.vector(invS))* (invS %*% matrix(1,nrow=dim(invS)[1],ncol=1))
    #out <- data.frame('y.mu'=as.vector(y[rnNA]%*%A),'s.mu'=sqrt(1/sum(as.vector(invS))),'n.mu'=N)
    out <- c(as.double(y[rnNA]%*%A),as.double(sqrt(1/sum(as.vector(invS)))),as.double(N))
  } else {
    #out <- data.frame('y.mu'=NA,'s.mu'=NA,'n.mu'=N)
    out <- c(as.double(NA),as.double(NA),as.double(N))
  }
  return(out)
}

makeRmatrix <- function(nr, nc, dLoc){
  # function to make the R matrix for decorrelated aggregation
  #
  # Inputs: 
  #   nr    scaling factor in 'row' dimensions
  #   nc    scaling factor in 'column' dimensions
  #   dLoc  size of the moving window
  
  r1 <- rep(1:nr, nc)
  c1 <- sort(rep(1:nc, nr))
  rc <- cbind(r1, c1)
  
  result <- matrix(NA, nrow=nr*nc, ncol=nr*nc)
  for(i in 1:(nr*nc)) {
    t1 <- t(abs(rc[i,] - t(rc)))
    t2 <- ifelse(t1 >= dLoc, dLoc, t1)
    t3 <- apply(t2, 1, function(z) prod(dLoc-z))
    result[,i] <- t3/(dLoc^2)
  }
  return(result)
}

# create working matrix
R <- makeRmatrix(scale_fact, scale_fact, winsize)

# if(verbose){
#   message(paste("Weights kernel size: ", ncol(R), sep=""))
# }

# ##########################
# create output NetCDF-4 file to store results

# define fill value
ncout_fill_value <- -999.0

# get dimensions position
ncout_lon_pos <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
ncout_lat_pos <- which(names(nc_in$dim) %in% c("lat","latitude","Latitude"))[1]
ncout_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]
ncout_class_transition_pos <- which(names(nc_in$dim) %in% c("class_transition"))[1]

# get dimension units
ncout_lon_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_lon_pos], "units")$value
ncout_lat_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_lat_pos], "units")$value
ncout_time_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_time_pos], "units")$value
ncout_class_transition_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_class_transition_pos], "units")$value

# get dimension longnames
ncout_lon_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_lon_pos], "long_name")$value
ncout_lat_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_lat_pos], "long_name")$value
ncout_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_time_pos], "long_name")$value
ncout_class_transition_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[ncout_class_transition_pos], "long_name")$value

# get dimension values
ncout_time_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[ncout_time_pos])
ncout_class_transition_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[ncout_class_transition_pos])

# set dimension values for new spatial resolution
ncout_lon_dim <- as.vector(output_coords[1:ncol(output_raster),1])
ncout_lat_dim <- as.vector(sort(output_coords[seq(from=1, to=ncell(output_raster), by = ncol(output_raster)),2], decreasing=TRUE))

# define new dimensions
ncout_londim <- ncdf4::ncdim_def(name="lon", longname=ncout_lon_lname, units=ncout_lon_units, vals=as.double(ncout_lon_dim), unlim=FALSE)
ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=ncout_lat_lname, units=ncout_lat_units, vals=as.double(sort(ncout_lat_dim, decreasing=TRUE)), unlim=FALSE)
ncout_timedim <- ncdf4::ncdim_def(name="time", longname=ncout_time_lname, units=ncout_time_units, vals=as.double(ncout_time_dim))
ncout_class_transitiondim <- ncdf4::ncdim_def(name="class_transition", longname="cover_class_transitions", units="dl", vals=ncout_class_transition_dim)

# set chunksizes
ncout_chunksizes <- c(length(ncout_lon_dim), length(ncout_lat_dim), 1, 1)

# create NetCDF-4 variables
nc_var_delta <- ncdf4::ncvar_def(name="delta", longname="Delta", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_class_transitiondim,ncout_timedim), chunksizes=ncout_chunksizes, compression=9)
nc_var_uncertainty <- ncdf4::ncvar_def(name="uncertainty", longname="delta_uncertainty", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_class_transitiondim,ncout_timedim), chunksizes=ncout_chunksizes, compression=9)
nc_var_fraction <- ncdf4::ncvar_def(name="fraction", longname="fraction of valid cells of low resolution", units="dl", missval=ncout_fill_value, prec="double", dim=list(ncout_londim,ncout_latdim,ncout_class_transitiondim,ncout_timedim), chunksizes=ncout_chunksizes, compression=9)

# define variable list
nc_var_list <- list(nc_var_delta, nc_var_uncertainty, nc_var_fraction)

# create output NetCDF-4 file
ncout <- ncdf4::nc_create(output_file, vars=nc_var_list, force_v4=TRUE)

# put additional attributes into dimension and data variables
ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
ncdf4::ncatt_put(ncout,"lon","axis","X")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"lat","axis","Y")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"time","standard_name","time")
ncdf4::ncatt_put(ncout,"time","calendar","standard")
nc_var_class_transition_list <- c(ncatt_get(nc_in, varid="class_transition", attname="class_transition_list")$value)
ncdf4::ncatt_put(ncout,"class_transition","class_transition_list",nc_var_class_transition_list)
ncdf4::ncatt_put(ncout,"fraction","total_cell_number",Ssf,prec="short")

# set global attributes to output NetCDF-4 file
ncdf4::ncatt_put(ncout,0,"title","Space4Time")
ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., Hooker, J., & Cescatti, A. (2018). The mark of vegetation change on Earth’s surface energy balance. Nature Communications, 9, 679.")
ncdf4::ncatt_put(ncout,0,"summary","Product generated using ’Space4Time’ algorithm")
ncdf4::ncatt_put(ncout,0,"keywords","Space4Time")
ncdf4::ncatt_put(ncout,0,"Conventions", "CF-1.6")

# remove unuseful objects
suppressWarnings(rm(list=c("opt_parser","opt","input_file","ll1","ll2","makeRmatrix","output_coords","output_file","output_raster","ref_pixel_col","ref_pixel_row","source_coords","source_ext","source_raster_ref","is.odd")))
suppressWarnings(rm(list=ls(pattern="ncout_")))
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
noexport_list <- c("llist","var_ndims","nc_in","ncout","cores","source_res","target_res","winsize","scale_fact","Ssf","r","t","nc_hhist")

# ###########################
# Process
if(verbose){
  message(paste(c("Space4Time aggregator started at: "), Sys.time(), sep=""))
}
ptm <- proc.time()

pb <- utils::txtProgressBar(style=3)

for(l in 1:length(llist$class_transition)){
  # print(l)
  # import data from NetCDF-4 input file
  if(var_ndims == 3){
    q <- llist$class_transition[l]
    delta_in <- ncdf4::ncvar_get(nc_in, varid="delta", start=c(1,1,q), count=c(-1,-1,1))
    delta_error_in <- ncdf4::ncvar_get(nc_in, varid="delta_error", start=c(1,1,q), count=c(-1,-1,1))
  } 
  if(var_ndims == 4){
    q <- llist$class_transition[l]
    t <- llist$time[l]
    delta_in <- ncdf4::ncvar_get(nc_in, varid="delta", start=c(1,1,q,t), count=c(-1,-1,1,1))
    delta_error_in <- ncdf4::ncvar_get(nc_in, varid="delta_error", start=c(1,1,q,t), count=c(-1,-1,1,1))
  }
  
  # process data only if there are valid pixels in the layer
  if(!all(is.na(delta_in))){
    
    # # ##############
    # # Process data using a single core
    # if(loop == "none"){
    #   # create empty matrix to store results
    #   out_result <- matrix(data=as.numeric(NA), nrow=nCells, ncol=3)
    #   for(p in pix_list){
    #     print(paste("Processing pixel ", p, "/", length(plist$r), sep=""))
    #     #print(paste("col: ", plist$c[p], " row: ", plist$r[p], sep=""))
    #     # skip computation if kernel window falls outside the input raster
    #     if((plist$c[p] - CsfA) < 1 | (plist$c[p] + CsfB) > nCols | (plist$r[p] - RsfA) < 1 | (plist$r[p] + RsfB) > nRows){
    #       out_result[p,] <- rep(NA,3)
    #     } else {
    #       Din <- as.vector(delta_in[(plist$c[p] - CsfA):(plist$c[p] + CsfB),(plist$r[p] - RsfA):(plist$r[p] + RsfB)])
    #       if(all(is.na(Din))){
    #         #print(paste("Skipping pixel ", p, "/", length(plist$r), sep=""))
    #         out_result[p,] <- rep(NA,3)
    #       } else {
    #         #print(paste("Processing pixel ", p, "/", length(plist$r), sep=""))
    #         DEin <- as.vector(delta_error_in[(plist$c[p] - CsfA):(plist$c[p] + CsfB),(plist$r[p] - RsfA):(plist$r[p] + RsfB)])
    #         out_result[p,] <- agr.decor(y=Din, s=DEin, R=R)
    #       }
    #     }
    #   }
    # 
    #   # write results to output NetCDF-4 file
    #   #message("Write results to NetCDF-4 file")
    #   if(var_ndims == 3){
    #     ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[,1], start=c(1,1,q), count=c(-1,-1,1))
    #     ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[,2], start=c(1,1,q), count=c(-1,-1,1))
    #     ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[,3], start=c(1,1,q), count=c(-1,-1,1))
    #   }
    #   if(var_ndims == 4){
    #     ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[,1], start=c(1,1,q,t), count=c(-1,-1,1,1))
    #     ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[,2], start=c(1,1,q,t), count=c(-1,-1,1,1))
    #     ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[,3], start=c(1,1,q,t), count=c(-1,-1,1,1))
    #   }
    # }
    
    
    # ##############
    # Process data using multicore and foreach (pixel loop)
    if(loop == "pixel"){
      out_result <- foreach::foreach(p=pix_list,
                                     .combine=rbind,
                                     .inorder=TRUE,
                                     .noexport=noexport_list,
                                     .verbose=FALSE) %dopar% {
                                       # skip computation if kernel window falls outside the input raster
                                       if((plist$c[p] - CsfA) < 1 | (plist$c[p] + CsfB) > nCols | (plist$r[p] - RsfA) < 1 | (plist$r[p] + RsfB) > nRows){
                                         outr <- rep(NA,3)
                                       } else {
                                         Din <- as.vector(delta_in[(plist$c[p] - CsfA):(plist$c[p] + CsfB),(plist$r[p] - RsfA):(plist$r[p] + RsfB)])
                                         # skip pixel if there are no data
                                         if(all(is.na(Din))){
                                           outr <- rep(NA,3)
                                         } else {
                                           DEin <- as.vector(delta_error_in[(plist$c[p] - CsfA):(plist$c[p] + CsfB),(plist$r[p] - RsfA):(plist$r[p] + RsfB)])
                                           outr <- agr.decor(y=Din, s=DEin, R=R)
                                         }
                                         return(outr)
                                       }
                                     }
      
      # write results to output NetCDF-4 file
      #message("Write results to NetCDF-4 file")
      if(var_ndims == 3){
        ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[,1], start=c(1,1,q), count=c(-1,-1,1))
        ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[,2], start=c(1,1,q), count=c(-1,-1,1))
        ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[,3], start=c(1,1,q), count=c(-1,-1,1))
      }
      if(var_ndims == 4){
        ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[,1], start=c(1,1,q,t), count=c(-1,-1,1,1))
        ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[,2], start=c(1,1,q,t), count=c(-1,-1,1,1))
        ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[,3], start=c(1,1,q,t), count=c(-1,-1,1,1))
      }
    }
    
    # ##############
    # Process data using multicore and foreach (line loop)
    if(loop == "line"){
      Clen <- length(Clist)
      out_result <- foreach::foreach(r=Rlist,
                                     .combine=cbind,
                                     .inorder=TRUE,
                                     .noexport=noexport_list,
                                     .verbose=FALSE) %dopar% {
                                       # create empty matrix where to store results
                                       out_matrix <- matrix(data=as.numeric(NA), nrow=Clen, ncol=3)
                                       for(c in 1:Clen){
                                         # skip computation if kernel window falls outside the input raster
                                         if((Clist[c] - CsfA) < 1 | (Clist[c] + CsfB) > nCols | (r - RsfA) < 1 | (r + RsfB) > nRows){
                                           out_matrix[c,] <- rep(NA,3)
                                         } else {
                                           Din <- as.vector(delta_in[(Clist[c] - CsfA):(Clist[c] + CsfB),(r - RsfA):(r + RsfB)])
                                           # skip pixel if there are no data
                                           if(all(is.na(Din))){
                                             out_matrix[c,] <- rep(NA,3)
                                           } else {
                                             DEin <- as.vector(delta_error_in[(Clist[c] - CsfA):(Clist[c] + CsfB),(r - RsfA):(r + RsfB)])
                                             out_matrix[c,] <- agr.decor(y=Din, s=DEin, R=R)
                                           }
                                         }
                                       }
                                       return(as.vector(out_matrix))
                                     }
      
      # write results to output NetCDF-4 file
      #message("Write results to NetCDF-4 file")
      if(var_ndims == 3){
        ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[(1:Clen),], start=c(1,1,q), count=c(-1,-1,1))
        ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[((Clen+1):(Clen*2)),], start=c(1,1,q), count=c(-1,-1,1))
        ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[((Clen*2+1):(Clen*3)),], start=c(1,1,q), count=c(-1,-1,1))
      }
      if(var_ndims == 4){
        ncdf4::ncvar_put(ncout, varid="delta", vals=out_result[(1:Clen),], start=c(1,1,q,t), count=c(-1,-1,1,1))
        ncdf4::ncvar_put(ncout, varid="uncertainty", vals=out_result[((Clen+1):(Clen*2)),], start=c(1,1,q,t), count=c(-1,-1,1,1))
        ncdf4::ncvar_put(ncout, varid="fraction", vals=out_result[((Clen*2+1):(Clen*3)),], start=c(1,1,q,t), count=c(-1,-1,1,1))
      }
    }
    
    # remove temporary result and clean workspace
    rm(out_result)
    invisible(gc(verbose=FALSE))
    
  }
  
  # Show progress bar
  utils::setTxtProgressBar(pb, l/length(llist$class_transition))
}

close(pb)
# stop cluster
parallel::stopCluster(cl)

# print usage time
if(verbose){
  message(paste(c("Space4Time aggregator ended at: "), Sys.time(), sep=""))
  message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
}

# write processing comments and history to output NetCDF-4 file
if(ncdf4::ncatt_get(nc_in, varid=0, attname="history")$hasatt){
  nc_history_old <- c(ncdf4::ncatt_get(nc_in, varid=0, attname="history")$value)
} else {
  nc_history_old <- ""
}
nc_history <- paste(paste(date(), ": ", "s4t_aggregate.r", " --", nc_hhist, sep=""), nc_history_old, sep="\n")
ncdf4::ncatt_put(ncout,0,"history",nc_history)

if(ncdf4::ncatt_get(nc_in, varid=0, attname="comment")$hasatt){
  nc_comment_old <- c(ncdf4::ncatt_get(nc_in, varid=0, attname="comment")$value)
} else {
  nc_comment_old <- ""
}
nc_comment <- paste(paste(date(), ": ", "Generated using ’Space4Time’ aggregator in R language", " + source_resolution=", source_res, " + target_resolution=", target_res, " + winsize=", winsize, " + scale_factor=", scale_fact, " + total_cell_number=", Ssf, sep=""), nc_comment_old, sep=" ++ ")
ncdf4::ncatt_put(ncout,0,"comment",nc_comment)

# close the file, writing data to disk
ncdf4::nc_sync(ncout)
ncdf4::nc_close(ncout)

# clean workspace and exist
rm(list=ls())
invisible(gc(verbose=FALSE))
q("no")
