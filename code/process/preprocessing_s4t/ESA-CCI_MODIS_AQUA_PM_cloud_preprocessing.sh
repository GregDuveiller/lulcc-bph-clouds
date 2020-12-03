#!/bin/bash

# set variables
INPUTDIR="/space/filipfe/cloud/ESA_CCI_Cloud/ESACCI-L3X_CLOUD-MODIS_AQUA-fv2.0"
TMPDIR="/space/filipfe/cloud/ESA_CCI_Cloud/tmp"
OUTPUTCLIMATOLOGY="/space/filipfe/cloud/ESA_CCI_Cloud/climatology"
CORES=10
VARLIST="cfc_all"

echo "Average NetCDF files"
echo "Processing started at `date`"
STARTPROCESS="$(date +%s%N)"

# create output folder
mkdir -p $OUTPUTCLIMATOLOGY
mkdir -p $TMPDIR

###########################################

# set variable names
FILE_LIST=${TMPDIR}/file_list.txt

# set command list file name
COMMAND_LIST_PASS1=${TMPDIR}/command_list_pass1.sh
COMMAND_LIST_PASS4=${TMPDIR}/command_list_pass4.sh

# create list of files to be clip
find ${INPUTDIR}/** -type f -name "*.nc" | sort > $FILE_LIST

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
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_01.nc ${TMPDIR}/????01*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_02.nc ${TMPDIR}/????02*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_03.nc ${TMPDIR}/????03*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_04.nc ${TMPDIR}/????04*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_05.nc ${TMPDIR}/????05*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_06.nc ${TMPDIR}/????06*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_07.nc ${TMPDIR}/????07*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_08.nc ${TMPDIR}/????08*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_09.nc ${TMPDIR}/????09*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_10.nc ${TMPDIR}/????10*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_11.nc ${TMPDIR}/????11*_${VAR}.nc" >> $COMMAND_LIST_PASS4
echo "ncra -4 --deflate 9 -y avg -o ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_12.nc ${TMPDIR}/????12*_${VAR}.nc" >> $COMMAND_LIST_PASS4
done

# Process command list
cat $COMMAND_LIST_PASS4 | parallel -j $CORES --no-notice

for VAR in $VARLIST
do
ncrcat -4 --deflate 9 -o ${TMPDIR}/${OUTBASENAME}_${VAR}_monthly_avg_climatology_tmp.nc ${TMPDIR}/${OUTBASENAME}_${VAR}_avg_??.nc
# reverse latitude
ncpdq -4 -a '-lat' ${TMPDIR}/${OUTBASENAME}_${VAR}_monthly_avg_climatology_tmp.nc ${TMPDIR}/${OUTBASENAME}_${VAR}_monthly_avg_climatology_lat_fix.nc
# fix time dimension
ncap2 -4 --deflate 9 -s 'time(:)={12067,12098,12126,12157,12187,12218,12248,12279,12310,12340,12371,12401}' ${TMPDIR}/${OUTBASENAME}_${VAR}_monthly_avg_climatology_lat_fix.nc ${OUTPUTCLIMATOLOGY}/${OUTBASENAME}_${VAR}_monthly_avg_climatology.nc
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

