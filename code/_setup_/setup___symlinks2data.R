# Set-up connections to ancillary data 


## ancillary shapefiles for making maps
data_path <- '/Volumes/home/work/data/external_datasets/WorldVector/'
file.symlink(to = 'data/input_data/world_vectors', from = data_path)
# Source: Natural Earth

## setup path for source data of cloud cover (ESA CCI v2 MODIS-Aqua)
data_path <- '/Volumes/home/work/data/internal_datasets/lulcc-bph-clouds/sourceData/'
file.symlink(to = 'data/input_data/source_cloud_data', from = data_path)
# Source: Stengel ESA CCI v2

## ancillary shapefiles for exploring in the space4time domain
data_path <- '/Users/greg/work/data/external_datasets/lulcc_bph_RS_S4T/v2.0/'
file.symlink(to = 'data/input_data/S4T_ancillary', from = data_path)
# Source: NCOMM



## setup path for bulk intermediate data processing folder
data_path <- '/Volumes/home/work/data/internal_datasets/lulcc-bph-clouds/'
file.symlink(to = 'data/inter_data/s4t_processing', from = data_path)
# Source: Stengel ESA CCI v2