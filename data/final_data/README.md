# lulcc-bph-clouds data repository

Here we describe the data repository containing the final results of the following study:

G. Duveiller, F. Filiponni, A. Ceglar, J. Bojanowski, R. Alkama & A. Cescatti. Widespread cloud enhancement adds further value to the world's forests. _Nature Communications_ (in review).

The data are stored in a dedicated __data repository__ that is archived on Zenodo under this DOI: [DOI: 10.5281/zenodo.4727774]

The code associated with this data is stored in a distinct __code repository__, also archived on Zenodo under this DOI: [DOI: 10.5281/zenodo.4727822]

## Organisation of the data

The data repository is organised at two levels. 
+ folder ```study_results``` contains the final data generated in the study. The code necessary to produce this data can be found in the ```code/process``` folder in the code repository
+ folder ```data_4_figures``` contains a more condensed format of the data, providing what is required to reproduce the figures in the paper. The code necessary to *harvest* this data from the larger data results stored in the folder ```study_results``` can be found in the ```code/harvest``` folder of the code repository. Finally, the code to generate the figures from the folder ```data_4_figures``` can be found in the ```code/figures``` folder of the code repository.
