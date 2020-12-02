# Set-up connections to ancillary data 


## ancillary shapefiles for making maps
data_path <- '/Volumes/home/work/data/external_datasets/WorldVector/'
file.symlink(to = 'data/input_data/world_vectors', from = data_path)
# Source: Natural Earth



## ancillary shapefiles for exploring in the space4time domain
data_path <- '/Users/greg/work/data/external_datasets/lulcc_bph_RS_S4T/v2.0/'
file.symlink(to = 'data/input_data/S4T_ancillary', from = data_path)
# Source: Natural Earth