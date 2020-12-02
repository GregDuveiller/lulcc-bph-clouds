#!/bin/bash

# set variables
INPUTDIR="/space/filipfe_data/cloud/avhrr_pm/2004-2016_ESACCI-L3X_CLOUD-JRC-AVHRR_NOAA-PM-fv3.0"
TMPDIR="/space/filipfe_data/cloud/avhrr_pm/analysis/work"
OUTPUTSTACK="/space/filipfe_data/cloud/avhrr_pm/analysis/stack"
OUTPUTCLIMATOLOGY="/space/filipfe_data/cloud/avhrr_pm/analysis/climatology"
CORES=10
CLOUDTYPE=FALSE
LOWCLOUDTYPE=FALSE
VARLIST="cfc_day cfc_all"

echo "Average NetCDF files"
echo "Processing started at `date`"
STARTPROCESS="$(date +%s%N)"

# create output folder
mkdir -p $OUTPUTSTACK
mkdir -p $OUTPUTCLIMATOLOGY
mkdir -p $TMPDIR

###########################################

# set variable names
FILE_LIST=${TMPDIR}/file_list.txt

# set command list file name
COMMAND_LIST_PASS1=${TMPDIR}/command_list_pass1.sh
COMMAND_LIST_PASS4=${TMPDIR}/command_list_pass4.sh

# create list of files to be clip
#find ${INPUTDIR}/** -type f -name "*.nc" | sort > $FILE_LIST

# get basename to set output file names
FILE1=`head -n 1 $FILE_LIST`
BASENAME=$(basename $FILE1 .nc)
OUTBASENAME=`echo "$BASENAME" | sed -e 's:^[0-9]*-*::'`

###########################################
# Pass 1
# Extract variables and set time dimension

for FILE in $(< $FILE_LIST)
do
#echo "Processing file: $FILE"
CTBASENAME=$(basename $FILE .nc)

for VAR in $VARLIST
do
# extract 'cfc_day' variable
#ncks -4 --deflate 9 --mk_rec_dmn time -v cfc_day $FILE ${TMPDIR}/$CTBASENAME'_cfc_day.nc'
echo "ncks -4 --deflate 9 --mk_rec_dmn time -v $VAR $FILE ${TMPDIR}/${CTBASENAME}_$VAR.nc" >> $COMMAND_LIST_PASS1

done
done

###
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
# Pass 2
# Extract low cloud

if [[ "$LOWCLOUDTYPE" == "TRUE" ]]
then

echo ""
echo "Pass 2: calculate 'cloud_low'"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

for FILE in $(< $FILE_LIST)
do
#echo "Processing file: $FILE"
CTBASENAME=$(basename $FILE .nc)

# process cumulus
# extract low cloud type
ncks -v jch_day -d ctp_bin_centre,2 $FILE ${TMPDIR}/$CTBASENAME"_cloud_low_a_temp.nc"
# calculate 'cloud_low' by summing up all 'cot_bin_centre' dimension layers for 'ctp_bin_centre' dimension equal '2'
ncap2 -4 -v -s 'cloud_low=jch_day.total($cot_bin_centre);' ${TMPDIR}/$CTBASENAME"_cloud_low_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_low_b_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_low_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_low_c_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_low_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_low_d_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_low_d_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_low.nc"

# remove temporary files
rm -rf ${TMPDIR}/*_temp.nc

done

# add 'cloud_low' to 'VARLIST'
VARLIST=${VARLIST}" cloud_low"
echo "Variable list updated: $VARLIST"

fi

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Pass 3
# Extract cloud types

if [[ "$CLOUDTYPE" == "TRUE" ]]
then

echo ""
echo "Pass 3: extract cloud types"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

for FILE in $(< $FILE_LIST)
do
#echo "Processing file: $FILE"
CTBASENAME=$(basename $FILE .nc)

# process cumulus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,0 -d ctp_bin_centre,2 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus.nc"

# process stratocumulus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,1 -d ctp_bin_centre,2 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus.nc"

# process stratus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,2 -d ctp_bin_centre,2 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_stratus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratus.nc"

# process altocumulus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,0 -d ctp_bin_centre,1 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus.nc"

# process altostratus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,1 -d ctp_bin_centre,1 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus.nc"

# process nimbostratus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,2 -d ctp_bin_centre,1 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_a_temp.nc"

# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus.nc"

# process cirrus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,0 -d ctp_bin_centre,0 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus.nc"

# process cirrostratus
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,1 -d ctp_bin_centre,0 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus.nc"

# process deep_convection
# extract a single cloud type
ncks -v jch_day -d cot_bin_centre,2 -d ctp_bin_centre,0 $FILE ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_a_temp.nc"
# remove unuseful dimensions
ncwa -a cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_a_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_b_temp.nc"
ncks -C -x -v cot_bin_centre,ctp_bin_centre ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_b_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_c_temp.nc"
# set time record dimension
ncks -4 --deflate 9 --mk_rec_dmn time ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection_c_temp.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection.nc"

# create stack from single layers
#ncrcat -4 -o ${TMPDIR}/$CTBASENAME"_cloud_types_time_dmn.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cumulus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratocumulus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_stratus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altocumulus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_altostratus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_nimbostratus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_cirrostratus.nc" ${TMPDIR}/$CTBASENAME"_cloud_type_deep_convection.nc"

# remove temporary files
rm -rf ${TMPDIR}/*_temp.nc

done

# add cloud types to 'VARLIST'
VARLIST=${VARLIST}" cloud_type_cumulus cloud_type_stratocumulus cloud_type_stratus cloud_type_altocumulus cloud_type_altostratus cloud_type_nimbostratus cloud_type_cirrus cloud_type_cirrostratus cloud_type_deep_convection"
echo "Variable list updated: $VARLIST"

fi

# time ending
echo "Processing ended at" `date`
ENDCYCLE="$(($(date +%s%N)-$STARTCYCLE))"
S="$((ENDCYCLE/1000000000))"
M="$((ENDCYCLE%1000000000/1000000))"
printf "Elapsed time: %02d:%02d:%02d:%02d.%03d\n" "$((S/86400))" "$((S/3600%24))" "$((S/60%60))" "$((S%60))" "${M}"

###########################################
# Processing Pass 4

echo ""
echo "Pass 4: compute climatologies"
echo "Processing started at `date`"
STARTCYCLE="$(date +%s%N)"
echo "Processing ..."

# Setup processing
for VAR in $VARLIST
do
# create overall average from monthly averages
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_01.nc ${TMPDIR}/????01*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_02.nc ${TMPDIR}/????02*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_03.nc ${TMPDIR}/????03*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_04.nc ${TMPDIR}/????04*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_05.nc ${TMPDIR}/????05*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_06.nc ${TMPDIR}/????06*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_07.nc ${TMPDIR}/????07*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_08.nc ${TMPDIR}/????08*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_09.nc ${TMPDIR}/????09*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_10.nc ${TMPDIR}/????10*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_11.nc ${TMPDIR}/????11*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_12.nc ${TMPDIR}/????12*_${VAR}.nc" >> $COMMAND_LIST_PASS4
done

# Process command list
cat $COMMAND_LIST_PASS4 | parallel -j $CORES --no-notice

for VAR in $VARLIST
do
ncrcat -4 --deflate 9 -o ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_monthly_avg_climatology.nc ${OUTPUTSTACK}/${OUTBASENAME}_${VAR}_avg_??.nc
# fix time dimension
ncap2 -4 --deflate 9 -s 'time(:)={12067,12098,12126,12157,12187,12218,12248,12279,12310,12340,12371,12401}' ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_monthly_avg_climatology.nc ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_monthly_avg_climatology_fix_time.nc
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


