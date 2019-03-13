
inPoints <- '/ESS_Datasets/USERS/Duveiller/Workspace/lulcc-bph_clouds/dataProcessing/SYNOP/SYNOP_points_with_local_cover.shp'

pts <- sf::st_read(inPoints, quiet = TRUE)

ggplot(pts)+geom_sf(data=world,fill='grey20') + 
  geom_sf(aes(size=X1_treecove,colour=X0_occurren)) + 
  coord_sf(xlim=c(20,40),ylim=c(50,55))





