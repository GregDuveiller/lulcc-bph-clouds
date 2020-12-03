#!/bin/bash

#######################################################
#
# Script Name: nco_average.sh
# Version: 0.1
#
# Author: Federico Filipponi
# Date : 24.05.2018
#
# Copyright name: CC BY-SA
# License: GPLv3
#
# Purpose: Create stack, statistics and climatologies of ESA-CCI Cloud data using NetCDF Operators (NCO)
#
#######################################################

# set input variables
INPUTDIR="/space/filipfe_data/cloud/aqua_europe/ESA_CCI"
TMPDIR="/space/filipfe_data/cloud/aqua_europe/run06/work"
STACKDIR="/space/filipfe_data/cloud/aqua_europe/run06/stack"
OUTPUTDIR="/space/filipfe_data/cloud/aqua_europe/run06/stats"
OUTPUTCLIMATOLOGY="/space/filipfe_data/cloud/aqua_europe/run06/climatology"
CORES=10

# list list of variables to process
#VARLIST="cot_asc,cccot_asc,cer_asc,cmask_asc,cph_asc" # to extract all the vairables using a single nco command
#VARLIST="cot_asc cccot_asc cer_asc cmask_asc cph_asc" # to extract single variables in a loop cycle
#VARLIST="cot_asc cccot_asc"
#VARLIST="cmask_asc"
#VARLIST="cmask_asc cloud_class cloud_low"
VARLIST="cmask_asc cloud_types cloud_low"

echo "Average NetCDF files"
echo "Processing started at `date`"
STARTPROCESS="$(date +%s%N)"

# set variable names
FILE_LIST=${TMPDIR}/file_list.txt

# set command list file name
COMMAND_LIST_PASS1=${TMPDIR}/command_list_pass1.sh
COMMAND_LIST_PASS2=${TMPDIR}/command_list_pass2.sh
COMMAND_LIST_PASS3=${TMPDIR}/command_list_pass3.sh
COMMAND_LIST_PASS4=${TMPDIR}/command_list_pass4.sh
COMMAND_LIST_PASS5=${TMPDIR}/command_list_pass5.sh
COMMAND_LIST_PASS51=${TMPDIR}/command_list_pass51.sh
COMMAND_LIST_PASS6=${TMPDIR}/command_list_pass6.sh

# create output folder
mkdir -p $TMPDIR
mkdir -p $STACKDIR
mkdir -p $OUTPUTDIR
mkdir -p $OUTPUTCLIMATOLOGY

###########################################

# create list of files to be clip
find ${INPUTDIR}/** -type f -name "*.nc" | sort > $FILE_LIST

###########################################
# Processing setup

echo "Processing variables: $VARLIST"
echo "Processing setup ..."

# get basename to set output file names
FILE1=`head -n 1 $FILE_LIST`
BASENAME=$(basename $FILE1 .nc)
OUTBASENAME=`echo "$BASENAME" | sed -e 's:^[0-9]*-*::'`

# create command list file
#echo '#!/bin/sh' > $COMMAND_LIST

######
# Pass 1
# Extract variables and set time dimension

for FILE in $(< $FILE_LIST)
do
#echo "Processing file: $FILE"
BASENAME=$(basename $FILE .nc)
OUTPUTVARS=${TMPDIR}/${BASENAME}_all_tmp.nc

# set time dimension
#echo "ncks --mk_rec_dmn time -v "$VARLIST" $FILE $OUTPUTVARS" >> $COMMAND_LIST

for VAR in $VARLIST
do
if [[ "$VAR" != "cloud_class" && "$VAR" != "cloud_low" ]]
then
#echo "Processing variable: $VAR"
OUTPUTFILE=$TMPDIR/$BASENAME"_"$VAR".nc"
OUTPUTVAR=$TMPDIR/$BASENAME"_"$VAR".nc"
# set time dimension
#ncks -4 --mk_rec_dmn time -v $VAR $FILE $OUTPUTVAR
echo "ncks -4 --mk_rec_dmn time -v $VAR $FILE $OUTPUTVAR" >> $COMMAND_LIST_PASS1

fi
done
done

######
# Pass 2
# apply Logarithm transformation to 'cot_asc' variable and convert 'cmask' to double type

if [[ $VARLIST =~ (^|( ))cot_asc(( )|$) ]]
then
VAR='cot_asc'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
OUTPUTVAR=$TMPDIR/$BASENAME"_"$VAR".nc"
OUTPUTVARLOG=$TMPDIR/$BASENAME"_"$VAR"_log.nc"
# compute log transformation  on 'cot_asc' variable
#ncap2 -4 -s 'cot_log_asc=log10(cot_asc)' $OUTPUTVAR $OUTPUTVARLOG
#echo "ncap2 -4 -v -s 'cot_asc_log=log10(cot_asc)' $OUTPUTVAR $OUTPUTVARLOG" >> $COMMAND_LIST_PASS2
echo "ncap2 -4 -v -s 'cot_asc_log=log10(cot_asc); cot_asc_log.change_miss(0); cot_asc_log.set_miss(-999.0)' $OUTPUTVAR $OUTPUTVARLOG" >> $COMMAND_LIST_PASS2
done

# add 'cot_log_asc' to 'VARLIST'
VARLIST=${VARLIST}" cot_asc_log"
echo "Variable list updated: $VARLIST"
fi

#if [[ $VARLIST =~ (^|( ))cmask_asc(( )|$) && $VAR == "cmask_asc" ]]
if [[ $VARLIST =~ (^|( ))cmask_asc(( )|$) ]]
then
VAR='cmask_asc'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
OUTPUTVAR=$TMPDIR/$BASENAME"_"$VAR".nc"
OUTPUTVARTMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
#ncatted -a flag_values,cmask_asc,d,, $OUTPUTVAR $OUTPUTVARTMP
#ncap2 -4 -O -v -s 'cmask=double(cmask_asc)' $OUTPUTVARTMP $OUTPUTVAR
#rm -f $OUTPUTVARTMP
echo "ncatted -a flag_values,cmask_asc,d,, $OUTPUTVAR $OUTPUTVARTMP" >> $COMMAND_LIST_PASS2
done
fi

# convert 'cmask' to double type
if [[ $VARLIST =~ (^|( ))cmask_asc(( )|$) ]]
then
VAR='cmask_asc'
echo "sleep 60" >> $COMMAND_LIST_PASS2
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
OUTPUTVARTMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
OUTPUTVARCMASK=$TMPDIR/$BASENAME"_cmask.nc"
#ncatted -a flag_values,cmask_asc,d,, $OUTPUTVAR $OUTPUTVARTMP
#ncap2 -4 -O -v -s 'cmask=double(cmask_asc)' $OUTPUTVARTMP $OUTPUTVAR
#rm -f $OUTPUTVARTMP
echo "ncap2 -4 -O -v -s 'cmask=double(cmask_asc)' $OUTPUTVARTMP $OUTPUTVARCMASK" >> $COMMAND_LIST_PASS2
done
fi

# set NaN in 'cot_asc' and 'cot_asc_log' to zero
if [[ $VARLIST =~ (^|( ))cot_asc(( )|$) ]]
then
VAR='cot_asc'
echo "sleep 60" >> $COMMAND_LIST_PASS2
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
COT_VAR=$TMPDIR/$BASENAME"_"$VAR".nc"
COT_VAR_TEMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
COT_LOG_VAR=$TMPDIR/$BASENAME"_"$VAR"_log.nc"
COT_LOG_VAR_TEMP=$TMPDIR/$BASENAME"_"$VAR"_log_temp.nc"
CMASK_VAR=$TMPDIR/$BASENAME"_cmask.nc"
#cdo -s -O -f nc4 mul $COT_VAR $CMASK_VAR $COT_VAR
#cdo -s -O -f nc4 mul $COT_LOG_VAR $CMASK_VAR $COT_LOG_VAR
echo "cdo -s -f nc4 mul $COT_VAR $CMASK_VAR $COT_VAR_TEMP" >> $COMMAND_LIST_PASS2
#echo "cdo -s -f nc4 mul $COT_LOG_VAR $CMASK_VAR $COT_LOG_VAR_TEMP" >> $COMMAND_LIST_PASS2
done
fi

if [[ $VARLIST =~ (^|( ))cot_asc(( )|$) ]]
then
VAR='cot_asc'
echo "sleep 60" >> $COMMAND_LIST_PASS2
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
COT_VAR=$TMPDIR/$BASENAME"_"$VAR".nc"
COT_VAR_TEMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
COT_LOG_VAR=$TMPDIR/$BASENAME"_"$VAR"_log.nc"
COT_LOG_VAR_TEMP=$TMPDIR/$BASENAME"_"$VAR"_log_temp.nc"
echo "mv -f $COT_VAR_TEMP $COT_VAR" >> $COMMAND_LIST_PASS2
#echo "mv -f $COT_LOG_VAR_TEMP $COT_LOG_VAR" >> $COMMAND_LIST_PASS2
done
fi

## compute new variable 'cloud_class'
#if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
#then
#VAR='cloud_class'
#for FILE in $(< $FILE_LIST)
#do
#BASENAME=$(basename $FILE .nc)
#CC_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
#echo "ncap2 -v -s 'cloud_class=byte(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_class=byte(11); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_class=byte(12); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_class=byte(13); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_class=byte(21); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_class=byte(22); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_class=byte(23); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_class=byte(31); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_class=byte(32); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_class=byte(33);' $FILE $CC_VAR_TMP" >> $COMMAND_LIST_PASS2
#done
#fi
#
## edit attributes of variable 'cloud_class'
#if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
#then
#VAR='cloud_class'
#for FILE in $(< $FILE_LIST)
#do
#BASENAME=$(basename $FILE .nc)
#CC_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
#echo "ncatted -a standard_name,cloud_class,o,c,'cloud_class' -a long_name,cloud_class,o,c,'cloud class based on ISCCP' -a units,cloud_class,o,c,'dl' -a valid_max,cloud_class,o,s,'33' $CC_VAR_TMP" >> $COMMAND_LIST_PASS2
#done
#fi
#
## set time record dimension for variable 'cloud_class'
#if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
#then
#VAR='cloud_class'
#for FILE in $(< $FILE_LIST)
#do
#BASENAME=$(basename $FILE .nc)
#CC_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
#CC_VAR=$TMPDIR/$BASENAME"_"$VAR".nc"
#echo "ncks -4 --mk_rec_dmn time -v cloud_class $CC_VAR_TMP $CC_VAR" >> $COMMAND_LIST_PASS2
#done
#fi

# compute new variable 'cloud_low'
if [[ $VARLIST =~ (^|( ))cloud_low(( )|$) ]]
then
VAR='cloud_low'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CL_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
echo "ncap2 -v -s 'cloud_low=double(0.0*ctp_asc); where(ctp_asc >= 680.0) cloud_low=double(1); elsewhere cloud_low=double(0)' $FILE $CL_VAR_TMP" >> $COMMAND_LIST_PASS2
done
fi

# edit attributes of variable 'cloud_low'
if [[ $VARLIST =~ (^|( ))cloud_low(( )|$) ]]
then
VAR='cloud_low'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CL_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
echo "ncatted -a standard_name,cloud_low,o,c,'cloud_low' -a long_name,cloud_low,o,c,'low cloud based on ISCCP' -a units,cloud_low,o,c,'dl' -a valid_max,cloud_low,o,s,'10000' $CL_VAR_TMP" >> $COMMAND_LIST_PASS2
done
fi

# set time record dimension for variable 'cloud_low'
if [[ $VARLIST =~ (^|( ))cloud_low(( )|$) ]]
then
VAR='cloud_low'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CL_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
CL_VAR=$TMPDIR/$BASENAME"_"$VAR".nc"
echo "ncks -4 --mk_rec_dmn time -v cloud_low $CL_VAR_TMP $CL_VAR" >> $COMMAND_LIST_PASS2
done
fi

# compute new variable 'cloud_types'
if [[ $VARLIST =~ (^|( ))cloud_types(( )|$) ]]
then
VAR='cloud_types'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CT_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
echo "ncap2 -h -v -s 'cloud_type_cumulus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_cumulus=double(1); elsewhere cloud_type_cumulus=double(0); cloud_type_stratocumulus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_stratocumulus=double(1); elsewhere cloud_type_stratocumulus=double(0); cloud_type_stratus=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_stratus=double(1); elsewhere cloud_type_stratus=double(0); cloud_type_altocumulus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_altocumulus=double(1); elsewhere cloud_type_altocumulus=double(0); cloud_type_altostratus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_altostratus=double(1); elsewhere cloud_type_altostratus=double(0); cloud_type_nimbostratus=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_nimbostratus=double(1); elsewhere cloud_type_nimbostratus=double(0); cloud_type_cirrus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_cirrus=double(1); elsewhere cloud_type_cirrus=double(0); cloud_type_cirrostratus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_cirrostratus=double(1); elsewhere cloud_type_cirrostratus=double(0); cloud_type_deep_convection=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_deep_convection=double(1); elsewhere cloud_type_deep_convection=double(0);' $FILE $CT_VAR_TMP" >> $COMMAND_LIST_PASS2
done
fi

# edit attributes of variable 'cloud_types'
if [[ $VARLIST =~ (^|( ))cloud_types(( )|$) ]]
then
VAR='cloud_types'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CT_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
echo "ncatted -h -a standard_name,cloud_type_cumulus,o,c,'cloud_type_cumulus' -a long_name,cloud_type_cumulus,o,c,'cloud type cumulus based on ISCCP' -a units,cloud_type_cumulus,o,c,'dl' -a valid_max,cloud_type_cumulus,o,s,'10000' -a standard_name,cloud_type_stratocumulus,o,c,'cloud_type_stratocumulus' -a long_name,cloud_type_stratocumulus,o,c,'cloud type stratocumulus based on ISCCP' -a units,cloud_type_stratocumulus,o,c,'dl' -a valid_max,cloud_type_stratocumulus,o,s,'10000' -a standard_name,cloud_type_stratus,o,c,'cloud_type_stratus' -a long_name,cloud_type_stratus,o,c,'cloud type stratus based on ISCCP' -a units,cloud_type_stratus,o,c,'dl' -a valid_max,cloud_type_stratus,o,s,'10000' -a standard_name,cloud_type_altocumulus,o,c,'cloud_type_altocumulus' -a long_name,cloud_type_altocumulus,o,c,'cloud type altocumulus based on ISCCP' -a units,cloud_type_altocumulus,o,c,'dl' -a valid_max,cloud_type_altocumulus,o,s,'10000' -a standard_name,cloud_type_altostratus,o,c,'cloud_type_altostratus' -a long_name,cloud_type_altostratus,o,c,'cloud type altostratus based on ISCCP' -a units,cloud_type_altostratus,o,c,'dl' -a valid_max,cloud_type_altostratus,o,s,'10000' -a standard_name,cloud_type_nimbostratus,o,c,'cloud_type_nimbostratus' -a long_name,cloud_type_nimbostratus,o,c,'cloud type nimbostratus based on ISCCP' -a units,cloud_type_nimbostratus,o,c,'dl' -a valid_max,cloud_type_nimbostratus,o,s,'10000' -a standard_name,cloud_type_cirrus,o,c,'cloud_type_cirrus' -a long_name,cloud_type_cirrus,o,c,'cloud type cirrus based on ISCCP' -a units,cloud_type_cirrus,o,c,'dl' -a valid_max,cloud_type_cirrus,o,s,'10000' -a standard_name,cloud_type_cirrostratus,o,c,'cloud_type_cirrostratus' -a long_name,cloud_type_cirrostratus,o,c,'cloud type cirrostratus based on ISCCP' -a units,cloud_type_cirrostratus,o,c,'dl' -a valid_max,cloud_type_cirrostratus,o,s,'10000' -a standard_name,cloud_type_deep_convection,o,c,'cloud_type_deep_convection' -a long_name,cloud_type_deep_convection,o,c,'cloud type deep convection based on ISCCP' -a units,cloud_type_deep_convection,o,c,'dl' -a valid_max,cloud_type_deep_convection,o,s,'10000' $CT_VAR_TMP" >> $COMMAND_LIST_PASS2
done
fi

# set time record dimension for variable 'cloud_types'
if [[ $VARLIST =~ (^|( ))cloud_types(( )|$) ]]
then
VAR='cloud_types'
for FILE in $(< $FILE_LIST)
do
BASENAME=$(basename $FILE .nc)
CT_VAR_TMP=$TMPDIR/$BASENAME"_"$VAR"_temp.nc"
CT_VAR=$TMPDIR/$BASENAME"_"$VAR".nc"
echo "ncks -4 --mk_rec_dmn time $CT_VAR_TMP $CT_VAR" >> $COMMAND_LIST_PASS2
done
fi

# # compute new variables for different cloud types
# if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
# then
# for FILE in $(< $FILE_LIST)
# do
# BASENAME=$(basename $FILE .nc)
# # set output file names
# CT_CUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cumulus_temp.nc"
# CT_STRATOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratocumulus_temp.nc"
# CT_STRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratus_temp.nc"
# CT_ALTOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altocumulus_temp.nc"
# CT_ALTOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altostratus_temp.nc"
# CT_NIMBOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_nimbostratus_temp.nc"
# CT_CIRRUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrus_temp.nc"
# CT_CIRROSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrostratus_temp.nc"
# CT_DEEP_CONVECTION_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_deep_convection_temp.nc"
# 
# # compute cloud types
# echo "ncap2 -v -s 'cloud_type_cumulus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_cumulus=double(1); elsewhere cloud_type_cumulus=double(0);' $FILE $CT_CUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_stratocumulus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_stratocumulus=double(1); elsewhere cloud_type_stratocumulus=double(0);' $FILE $CT_STRATOCUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_stratus=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 680.0 && ctp_asc < 1000.0)) cloud_type_stratus=double(1); elsewhere cloud_type_stratus=double(0);' $FILE $CT_STRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_altocumulus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_altocumulus=double(1); elsewhere cloud_type_altocumulus=double(0);' $FILE $CT_ALTOCUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_altostratus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_altostratus=double(1); elsewhere cloud_type_altostratus=double(0);' $FILE $CT_ALTOSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_nimbostratus=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 440.0 && ctp_asc < 680.0)) cloud_type_nimbostratus=double(1); elsewhere cloud_type_nimbostratus=double(0);' $FILE $CT_NIMBOSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_cirrus=double(0.0*cot_asc); where((cot_asc > 0.0 && cot_asc < 3.6) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_cirrus=double(1); elsewhere cloud_type_cirrus=double(0);' $FILE $CT_CIRRUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_cirrostratus=double(0.0*cot_asc); where((cot_asc >= 3.6 && cot_asc < 23.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_cirrostratus=double(1); elsewhere cloud_type_cirrostratus=double(0);' $FILE $CT_CIRROSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncap2 -v -s 'cloud_type_deep_convection=double(0.0*cot_asc); where((cot_asc >= 23.0 && cot_asc < 379.0) && (ctp_asc >= 50.0 && ctp_asc < 440.0)) cloud_type_deep_convection=double(1); elsewhere cloud_type_deep_convection=double(0);' $FILE $CT_DEEP_CONVECTION_VAR_TMP" >> $COMMAND_LIST_PASS2
# 
# done
# fi
# 
# # set attributes
# if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
# then
# for FILE in $(< $FILE_LIST)
# do
# BASENAME=$(basename $FILE .nc)
# # set output file names
# CT_CUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cumulus_temp.nc"
# CT_STRATOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratocumulus_temp.nc"
# CT_STRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratus_temp.nc"
# CT_ALTOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altocumulus_temp.nc"
# CT_ALTOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altostratus_temp.nc"
# CT_NIMBOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_nimbostratus_temp.nc"
# CT_CIRRUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrus_temp.nc"
# CT_CIRROSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrostratus_temp.nc"
# CT_DEEP_CONVECTION_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_deep_convection_temp.nc"
# 
# # change attribute values
# echo "ncatted -a standard_name,cloud_type_cumulus,o,c,'cloud_type_cumulus' -a long_name,cloud_type_cumulus,o,c,'cloud type cumulus based on ISCCP' -a units,cloud_type_cumulus,o,c,'dl' -a valid_max,cloud_type_cumulus,o,s,'10000' $CT_CUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_stratocumulus,o,c,'cloud_type_stratocumulus' -a long_name,cloud_type_stratocumulus,o,c,'cloud type stratocumulus based on ISCCP' -a units,cloud_type_stratocumulus,o,c,'dl' -a valid_max,cloud_type_stratocumulus,o,s,'10000' $CT_STRATOCUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_stratus,o,c,'cloud_type_stratus' -a long_name,cloud_type_stratus,o,c,'cloud type stratus based on ISCCP' -a units,cloud_type_stratus,o,c,'dl' -a valid_max,cloud_type_stratus,o,s,'10000' $CT_STRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_altocumulus,o,c,'cloud_type_altocumulus' -a long_name,cloud_type_altocumulus,o,c,'cloud type altocumulus based on ISCCP' -a units,cloud_type_altocumulus,o,c,'dl' -a valid_max,cloud_type_altocumulus,o,s,'10000' $CT_ALTOCUMULUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_altostratus,o,c,'cloud_type_altostratus' -a long_name,cloud_type_altostratus,o,c,'cloud type altostratus based on ISCCP' -a units,cloud_type_altostratus,o,c,'dl' -a valid_max,cloud_type_altostratus,o,s,'10000' $CT_ALTOSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_nimbostratus,o,c,'cloud_type_nimbostratus' -a long_name,cloud_type_nimbostratus,o,c,'cloud type nimbostratus based on ISCCP' -a units,cloud_type_nimbostratus,o,c,'dl' -a valid_max,cloud_type_nimbostratus,o,s,'10000' $CT_NIMBOSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_cirrus,o,c,'cloud_type_cirrus' -a long_name,cloud_type_cirrus,o,c,'cloud type cirrus based on ISCCP' -a units,cloud_type_cirrus,o,c,'dl' -a valid_max,cloud_type_cirrus,o,s,'10000' $CT_CIRRUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_cirrostratus,o,c,'cloud_type_cirrostratus' -a long_name,cloud_type_cirrostratus,o,c,'cloud type cirrostratus based on ISCCP' -a units,cloud_type_cirrostratus,o,c,'dl' -a valid_max,cloud_type_cirrostratus,o,s,'10000' $CT_CIRROSTRATUS_VAR_TMP" >> $COMMAND_LIST_PASS2
# echo "ncatted -a standard_name,cloud_type_deep_convection,o,c,'cloud_type_deep_convection' -a long_name,cloud_type_deep_convection,o,c,'cloud type deep convection based on ISCCP' -a units,cloud_type_deep_convection,o,c,'dl' -a valid_max,cloud_type_deep_convection,o,s,'10000' $CT_DEEP_CONVECTION_VAR_TMP" >> $COMMAND_LIST_PASS2
# 
# done
# fi
# 
# # fix time as record dimension
# if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
# then
# for FILE in $(< $FILE_LIST)
# do
# BASENAME=$(basename $FILE .nc)
# # set output file names
# CT_CUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cumulus_temp.nc"
# CT_STRATOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratocumulus_temp.nc"
# CT_STRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_stratus_temp.nc"
# CT_ALTOCUMULUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altocumulus_temp.nc"
# CT_ALTOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_altostratus_temp.nc"
# CT_NIMBOSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_nimbostratus_temp.nc"
# CT_CIRRUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrus_temp.nc"
# CT_CIRROSTRATUS_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_cirrostratus_temp.nc"
# CT_DEEP_CONVECTION_VAR_TMP=$TMPDIR/$BASENAME"_cloud_type_deep_convection_temp.nc"
# CT_CUMULUS_VAR=$TMPDIR/$BASENAME"_cloud_type_cumulus.nc"
# CT_STRATOCUMULUS_VAR=$TMPDIR/$BASENAME"_cloud_type_stratocumulus.nc"
# CT_STRATUS_VAR=$TMPDIR/$BASENAME"_cloud_type_stratus.nc"
# CT_ALTOCUMULUS_VAR=$TMPDIR/$BASENAME"_cloud_type_altocumulus.nc"
# CT_ALTOSTRATUS_VAR=$TMPDIR/$BASENAME"_cloud_type_altostratus.nc"
# CT_NIMBOSTRATUS_VAR=$TMPDIR/$BASENAME"_cloud_type_nimbostratus.nc"
# CT_CIRRUS_VAR=$TMPDIR/$BASENAME"_cloud_type_cirrus.nc"
# CT_CIRROSTRATUS_VAR=$TMPDIR/$BASENAME"_cloud_type_cirrostratus.nc"
# CT_DEEP_CONVECTION_VAR=$TMPDIR/$BASENAME"_cloud_type_deep_convection.nc"
# 
# # compute cloud types
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_cumulus $CT_CUMULUS_VAR_TMP $CT_CUMULUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_stratocumulus $CT_STRATOCUMULUS_VAR_TMP $CT_STRATOCUMULUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_stratus $CT_STRATUS_VAR_TMP $CT_STRATUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_altocumulus $CT_ALTOCUMULUS_VAR_TMP $CT_ALTOCUMULUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_altostratus $CT_ALTOSTRATUS_VAR_TMP $CT_ALTOSTRATUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_nimbostratus $CT_NIMBOSTRATUS_VAR_TMP $CT_NIMBOSTRATUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_cirrus $CT_CIRRUS_VAR_TMP $CT_CIRRUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_cirrostratus $CT_CIRROSTRATUS_VAR_TMP $CT_CIRROSTRATUS_VAR" >> $COMMAND_LIST_PASS2
# echo "ncks -4 --mk_rec_dmn time -v cloud_type_deep_convection $CT_DEEP_CONVECTION_VAR_TMP $CT_DEEP_CONVECTION_VAR" >> $COMMAND_LIST_PASS2
# 
# done
# fi

# remove temp files
echo "sleep 60" >> $COMMAND_LIST_PASS2
echo "rm -f $TMPDIR/*_temp.nc" >> $COMMAND_LIST_PASS2
# alternative solution if there are too many files to be deleted
#find $TMPDIR -name '*_temp.nc' | xargs rm

# # change 'cloud_class' to cloud types in 'VARLIST'
# if [[ $VARLIST =~ (^|( ))cloud_class(( )|$) ]]
# then
# VARLIST=`echo $VARLIST | sed -e 's/cloud_class/cloud_type_cumulus cloud_type_stratocumulus cloud_type_stratus cloud_type_altocumulus cloud_type_altostratus cloud_type_nimbostratus cloud_type_cirrus cloud_type_cirrostratus cloud_type_deep_convection/g'`
# echo "Variable list updated: $VARLIST"
# fi

# change 'cmask_asc' to 'cmask' in 'VARLIST'
if [[ $VARLIST =~ (^|( ))cmask_asc(( )|$) ]]
then
VARLIST=`echo $VARLIST | sed -e 's/cmask_asc/cmask/g'`
echo "Variable list updated: $VARLIST"
fi

###########################################
# Processing Pass 1

echo "Pass 1: extract variables and set time dimension"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS1 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Processing Pass 2

echo ""
echo "Pass 2: apply custom intermediates steps for specific variables (logarithm transformation to 'cot_asc' variable, convert 'cmask' to double type, calculate 'cloud_class', calculate 'cloud_low'"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS2 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Processing setup step 3 and 4

echo ""
echo "Processing setup ..."

######
# Pass 3
# Stack monthly observations

# get first and last date from processed files
FIRST_DATE=`exec ls ${TMPDIR} | grep -E '^[0-9]+' | sed -e 's/^\(.\{6\}\).*/\1/' | sort | head -n 1`
FIRST_DATE=$(($FIRST_DATE + 0))
LAST_DATE=`exec ls ${TMPDIR} | grep -E '^[0-9]+' | sed -e 's/^\(.\{6\}\).*/\1/' | sort | tail -1`
LAST_DATE=$(($LAST_DATE + 0))

# set first and last yera and month
FIRST_YEAR=${FIRST_DATE:0:4}
FIRST_MONTH=${FIRST_DATE:4:2}
LAST_YEAR=${LAST_DATE:0:4}
LAST_MONTH=${LAST_DATE:4:2}

# stack images
for YEAR in $(eval echo "{$FIRST_YEAR..$LAST_YEAR}")
do

for MONTH in {01..12}
do
YYYYMM=$((${YEAR}${MONTH} + 0 ))

if [ "$YYYYMM" -ge "$FIRST_DATE" ] && [ "$YYYYMM" -le "$LAST_DATE" ]
then
#echo "Processing month: ${YYYYMM}"

for VAR in $VARLIST
do
OUTPUT_MONTHLY_STACK=${STACKDIR}/${YYYYMM}-${OUTBASENAME}_${VAR}_stack.nc
#OUTPUT_MONTHLY_AVERAGE=${STACKDIR}/${YYYYMM}-${OUTBASENAME}_${VAR}_avg.nc
# stack monthly observations
#ncrcat -h -o $OUTPUT_MONTHLY_STACK ${TMPDIR}/${YYYYMM}*${VAR}*.nc
echo "ncrcat -4 -h -o $OUTPUT_MONTHLY_STACK ${TMPDIR}/${YYYYMM}*_${VAR}.nc" >> $COMMAND_LIST_PASS3
done
fi

done
done

######
# Pass 4
# Compute monthly averages 

for YEAR in $(eval echo "{$FIRST_YEAR..$LAST_YEAR}")
do

for MONTH in {01..12}
do
YYYYMM=$((${YEAR}${MONTH} + 0 ))

if [[ $YYYYMM -ge $FIRST_DATE && $YYYYMM -le $LAST_DATE ]]
then
#echo "Processing month: ${YYYYMM}"

for VAR in $VARLIST
do
OUTPUT_MONTHLY_STACK=${STACKDIR}/${YYYYMM}-${OUTBASENAME}_${VAR}_stack.nc
OUTPUT_MONTHLY_AVERAGE=${OUTPUTDIR}/${YYYYMM}-${OUTBASENAME}_${VAR}_avg.nc
# compute monthly averages
#ncra -v $VAR -y avg $OUTPUT_MONTHLY_STACK $OUTPUT_MONTHLY_AVERAGE
#echo "ncra -4 -v $VAR -y avg $OUTPUT_MONTHLY_STACK $OUTPUT_MONTHLY_AVERAGE" >> $COMMAND_LIST_PASS4
echo "ncra -4 -y avg $OUTPUT_MONTHLY_STACK $OUTPUT_MONTHLY_AVERAGE" >> $COMMAND_LIST_PASS4

## compute median values using CDO
#OUTPUT_MONTHLY_MEDIAN=${OUTPUTDIR}/${YYYYMM}-${OUTBASENAME}_${VAR}_med.nc
##cdo -s -f nc4 timpctl,50 $OUTPUT_MONTHLY_STACK -timmin $OUTPUT_MONTHLY_STACK -timmax $OUTPUT_MONTHLY_STACK $OUTPUT_MONTHLY_MEDIAN
#echo "cdo -s -f nc4 timpctl,50 $OUTPUT_MONTHLY_STACK -timmin $OUTPUT_MONTHLY_STACK -timmax $OUTPUT_MONTHLY_STACK $OUTPUT_MONTHLY_MEDIAN" >> $COMMAND_LIST_PASS4

# compute summer averages
MM_INT=$(( 10#${MONTH} + 0 ))
YEARJUNE=${YYYY}"06"
if [[ $MM_INT -eq 8 ]]
then
if [[ $YEARJUNE -ge $FIRST_DATE && $YYYYMM -le $LAST_DATE ]]
then
OUTPUT_JJA_AVERAGE=${OUTPUTDIR}/${YEAR}_JJA-${OUTBASENAME}_${VAR}_avg.nc
#echo "ncra -4 -v $VAR -y avg -o $OUTPUT_JJA_AVERAGE ${STACKDIR}/${YEAR}06-${OUTBASENAME}_${VAR}_stack.nc ${STACKDIR}/${YEAR}07-${OUTBASENAME}_${VAR}_stack.nc ${STACKDIR}/${YEAR}08-${OUTBASENAME}_${VAR}_stack.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 -y avg -o $OUTPUT_JJA_AVERAGE ${STACKDIR}/${YEAR}06-${OUTBASENAME}_${VAR}_stack.nc ${STACKDIR}/${YEAR}07-${OUTBASENAME}_${VAR}_stack.nc ${STACKDIR}/${YEAR}08-${OUTBASENAME}_${VAR}_stack.nc" >> $COMMAND_LIST_PASS4
fi
fi
done
fi

done
done

echo "Done"

###########################################
# Processing Pass 3

echo ""
echo "Pass 3: create monthly stacks"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS3 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Processing Pass 4

echo ""
echo "Pass 4: compute monthly statistics (average)"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS4 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Pass 5
# Compute climatologies

### for debug
#VARLIST="cot_asc cccot_asc cot_asc_log cmask"
#OUTBASENAME="ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0"

for VAR in $VARLIST
do
# create stack of monthly averages
echo "ncrcat -4 -h -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_monthly.nc ${OUTPUTDIR}/??????-*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS5

# create JJA average
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_JJA.nc ${OUTPUTDIR}/????_JJA-${OUTBASENAME}_${VAR}_avg.nc" >> $COMMAND_LIST_PASS5

# create stack of all daily observations
#echo "ncrcat -4 -h -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_full.nc ${TMPDIR}/*_${VAR}.nc" >> $COMMAND_LIST_PASS5

done

for VAR in $VARLIST
do
# create overall average
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_full.nc ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_full.nc" >> $COMMAND_LIST_PASS51
done

###########################################
# Processing Pass 5

echo ""
echo "Pass 5: compute climatologies"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS5 | parallel -j $CORES --no-notice
cat $COMMAND_LIST_PASS51 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Pass 6
# Compute climatologies (monthly averages)

### for debug
#VARLIST="cmask"
#OUTBASENAME="ESACCI-L3U_CLOUD-CLD_PRODUCTS-MODIS_AQUA_Europe-fv2.0"

for VAR in $VARLIST
do
# create overall average from monthly averages
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_01.nc ${OUTPUTDIR}/????01*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_02.nc ${OUTPUTDIR}/????02*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_03.nc ${OUTPUTDIR}/????03*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_04.nc ${OUTPUTDIR}/????04*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_05.nc ${OUTPUTDIR}/????05*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_06.nc ${OUTPUTDIR}/????06*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_07.nc ${OUTPUTDIR}/????07*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_08.nc ${OUTPUTDIR}/????08*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_09.nc ${OUTPUTDIR}/????09*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_10.nc ${OUTPUTDIR}/????10*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_11.nc ${OUTPUTDIR}/????11*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6
echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_12.nc ${OUTPUTDIR}/????12*_${VAR}_avg.nc" >> $COMMAND_LIST_PASS6

# create overall average from monthly median
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_01.nc ${OUTPUTDIR}/????01*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_02.nc ${OUTPUTDIR}/????02*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_03.nc ${OUTPUTDIR}/????03*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_04.nc ${OUTPUTDIR}/????04*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_05.nc ${OUTPUTDIR}/????05*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_06.nc ${OUTPUTDIR}/????06*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_07.nc ${OUTPUTDIR}/????07*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_08.nc ${OUTPUTDIR}/????08*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_09.nc ${OUTPUTDIR}/????09*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_10.nc ${OUTPUTDIR}/????10*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_11.nc ${OUTPUTDIR}/????11*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
#echo "ncra -4 -y avg -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_med_avg_12.nc ${OUTPUTDIR}/????12*_${VAR}_med.nc" >> $COMMAND_LIST_PASS6
done

###########################################
# Processing Pass 6

echo ""
echo "Pass 6: compute climatologies (monthly averages)"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process command list
cat $COMMAND_LIST_PASS6 | parallel -j $CORES --no-notice

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Processing Pass 7

echo ""
echo "Pass 7: stack monthly climatologies (monthly averages)"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Process data
for VAR in $VARLIST
do
ncrcat -4 -h -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_monthly_avg_climatology.nc ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_avg_avg_??.nc
done

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTPROCESS))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

