require(tidyverse)
require(ncdf4)
require(raster)


fpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/lulcc-bph_clouds/firstResults/climSpace/'
cloud_path <- '/ESS_Datasets/USERS/Filipponi/Space4Time/cloud_global/avhrr_pm_2004_2016_s4t_results/'
era5_path <- '/ESS_Datasets/USERS/Filipponi/era5/stack_2004_2016/'

dir.create(fpath, showWarnings = F,recursive = T)



cloud_file <- paste0(cloud_path,'Space4Time_cloud_cfc_all_from_2004_masked_aggregated_0.25_deg_line_att.nc')
nc_cloud <- nc_open(cloud_file)

TrString <- ncatt_get(nc_cloud,'class_transition',attname = 'class_transition_list')$value

for(iTr in ncvar_get(nc_cloud, varid='class_transition')){
  
TrLabel <- str_split(TrString,', ')[[1]][iTr]

delta_cloud <- ncvar_get(nc_cloud,varid='delta',start=c(1,1,1,mm),count = c(-1,-1,1,1))
delta_cloud <- ncvar_get(nc_cloud,varid='delta',start=c(1,1,iTr,1),count = c(-1,-1,1,-1))


nc_t2_climato <- nc_open(paste0(era5_path,'2t/ERA5_2t_2004-2016_climatology_stack.nc'))
# t2_climato <- ncvar_get(nc_t2_climato,varid='t2m',start=c(1,1,mm),count = c(-1,-1,1))
t2_climato <- ncvar_get(nc_t2_climato,varid='t2m')

nc_tp_climato <- nc_open(paste0(era5_path,'tp/ERA5_tp_2004-2016_climatology_stack.nc'))
# tp_climato <- ncvar_get(nc_tp_climato,varid='tp',start=c(1,1,mm),count = c(-1,-1,1))
tp_climato <- ncvar_get(nc_tp_climato,varid='tp')

nc_t2_anomaly <- nc_open(paste0(era5_path,'2t/ERA5_2t_2004-2016_climatology_anomaly_stack.nc'))
# t2_anomaly <- ncvar_get(nc_t2_anomaly,varid='t2m',start=c(1,1,mm),count = c(-1,-1,1))
t2_anomaly <- ncvar_get(nc_t2_anomaly,varid='t2m')

nc_tp_anomaly <- nc_open(paste0(era5_path,'tp/ERA5_tp_2004-2016_climatology_anomaly_stack.nc'))
# tp_anomaly <- ncvar_get(nc_tp_anomaly,varid='tp',start=c(1,1,mm),count = c(-1,-1,1))
tp_anomaly <- ncvar_get(nc_tp_anomaly,varid='tp')

nc_swvl1_anomaly <- nc_open(paste0(era5_path,'swvl1/ERA5_swvl1_2004-2016_climatology_anomaly_stack.nc'))
# swvl1_anomaly <- ncvar_get(nc_swvl1_anomaly,varid='swvl1',start=c(1,1,mm),count = c(-1,-1,1))
swvl1_anomaly <- ncvar_get(nc_swvl1_anomaly,varid='swvl1')



df <- data.frame(month=rep(month.abb,each=dim(delta_cloud)[1]*dim(delta_cloud)[2]),
                 delta_cloud=as.vector(delta_cloud),
                 t2_cl=as.vector(t2_climato),
                 tp_cl=as.vector(tp_climato),
                 t2_an=as.vector(t2_anomaly),
                 tp_an=as.vector(tp_anomaly),
                 swvl1_an=as.vector(swvl1_anomaly)) %>%
  filter(!is.na(delta_cloud)) %>%
  gather('anomaly.variable','anomaly.value',c('t2_an', 'tp_an', 'swvl1_an')) %>%
  gather('climato.variable','climato.value',c('t2_cl', 'tp_cl'))


require(RColorBrewer)
require(scales)

g <- ggplot(df)+
  stat_summary_2d(aes(x=anomaly.value,y=climato.value,z=delta_cloud),bins = c(50,50))+
  geom_vline(xintercept = 0, color='grey20')+
  scale_fill_gradientn('Cloud cover fraction change',colours=brewer.pal(9,'RdBu'),limits=c(-0.1,0.1),oob=squish)+
  facet_grid(climato.variable~anomaly.variable,scales="free")+
  theme(legend.position='bottom',legend.key.width = unit(3.4, "cm"))+
  ggtitle(paste('Change in cloud cover fraction for transition',TrLabel))+
  guides(fill = guide_colourbar(title.position="top",title.hjust = 0.5))

ggsave(filename = paste0('ChgCloudFrc__',gsub('-->','_to_',TrLabel),'.png'),
       path = fpath, plot = g, width = 10, height = 8)

}