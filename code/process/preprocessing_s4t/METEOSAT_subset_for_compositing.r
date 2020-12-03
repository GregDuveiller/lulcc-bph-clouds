#!/bin/bash

OUTPUT_PATH="/space/filipfe/clouds/s4t_cloud_meteosat/localTime"
#INPUT_FILE="/space/filipfe/clouds/s4t_cloud_meteosat/dataResults/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked.nc"
INPUT_FILE="/space/filipfe/clouds/s4t_cloud_meteosat/dataResults/COMET_CFC_MMDC_hour_climatology_land_topography_masked_2004_2014_montlhy_avg_of_hourly_mean_s4t_winsize7_s4t_subset_masked_aggregated_0.35.nc"
BASENAME=$(basename ${INPUT_FILE} .nc)

# set time positions to extract
TIME_LIST="14 38 62 86 110 134 158 182 206 230 254 278"
TIME_NAME=$((1))

# extract data
for TIME in ${TIME_LIST}
do
TIME_LNAME=$(printf "%02d" ${TIME_NAME})
ncks -4 --deflate 9 --mk_rec_dim time -d time,${TIME} ${INPUT_FILE} ${OUTPUT_PATH}/${BASENAME}_${TIME_LNAME}.nc
TIME_NAME=$(( $TIME_NAME + 1))
done

# merge results
ncrcat -h -4 --deflate 9 -v delta -o ${OUTPUT_PATH}/${BASENAME}_stack.nc ${OUTPUT_PATH}/${BASENAME}_??.nc

