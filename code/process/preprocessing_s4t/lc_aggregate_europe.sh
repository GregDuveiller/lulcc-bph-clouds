#!/bin/sh

#######################################################
#
# Script Name: lc_aggregate_europe.sh
# Version: 0.1
#
# Author: Federico Filipponi
# Date : 15.03.2018
#
# Copyright name: CC BY-SA
# License: GPLv3
#
# Purpose: Aggregate and resample ESA-CCI Land Cover data based on Plant Functional Types LUT
#
#######################################################

# set input variables
INPUTDIR="/space/filipfe_data/cloud/landcover/ESA-CCI"
OUTPUTDIR="/space/filipfe_data/cloud/landcover/lc_pft_aggregate_newLUT"
LC_AGGREGATE_BIN="/opt/lc-user-tools/bin/aggregate-map.sh"
PFT_LUT="/space/filipfe_data/cloud/landcover/tempv/simplifiedCCI4chgclim.csv"
FILENAME="ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v2.0.7.nc"

# list of PFT variables to extract
VARLIST="EFO DFO SAV SHR CaG WET URB WAT SNO BSV No_Data"
#VARLIST="BRF NEF SAV SHR CaG WET WAT BSV No_Data"

# create output folder
mkdir -p $OUTPUTDIR

#######################################################

echo "Aggregate ESA-CCI Land Cover"
echo "Processing started at `date`"
STARTPROCESS="$(date +%s%N)"

# add Java bin folder to environmental variables
export PATH=/opt/snap/jre/bin:$PATH

# set input file and get year
FILE=${INPUTDIR}/${FILENAME}
LC_YEAR=${FILENAME:31:4}
#LC_YEAR="2014"

echo "Aggregate ESA-CCI Land Cover to 0.02 degrees spatial resolution for Europe"
$LC_AGGREGATE_BIN -PgridName=GEOGRAPHIC_LAT_LON -PnumRows=9000 -Pnorth=75 -Psouth=35 -Pwest=-15 -Peast=45 -PuserPFTConversionTable=$PFT_LUT -PtargetDir=$OUTPUTDIR $FILE

# set variables for next step
LC_AGGREGATE_FILE=`find ${OUTPUTDIR}/** -type f -name "*-$LC_YEAR-*.nc"`
LC_AGGREGATE_BASENAME=$(basename $LC_AGGREGATE_FILE .nc)

#######################################################
# subset and resize to target extent using NCO

echo "Resize using NCO"
VARLIST_NC=`echo $VARLIST | sed -e "s/ /,/g"`
ncea -4 -d lat,35.0,75.0 -d lon,-15.0,45.0 -v ${VARLIST_NC} ${LC_AGGREGATE_FILE} $OUTPUTDIR/${LC_AGGREGATE_BASENAME}"_resize.nc"

#######################################################
# subset and resize to target extent using GDAL

#echo "Resize using GDAL"
#for VAR in $VARLIST
#do
#echo "Processing $VAR"
#gdal_translate -of NetCDF -ot Float32 -projwin -15 75 45 35 NETCDF:"$LC_AGGREGATE_FILE":$VAR $OUTPUTDIR/$LC_AGGREGATE_BASENAME"_"$VAR.nc
#done

###########################################

## merge variables back in a NetCDF
#echo "Merging variables"
#ncecat --gag -o $OUTPUTDIR/$LC_AGGREGATE_BASENAME"_"$VAR"_gag.nc" $OUTPUTDIR/$LC_AGGREGATE_BASENAME"_*.nc"
#ncks -3 -G : $OUTPUTDIR/$LC_AGGREGATE_BASENAME"_"$VAR"_gag.nc" $OUTPUTDIR/$LC_AGGREGATE_BASENAME"_"$VAR"_group.nc" ### not working

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTPROCESS))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

