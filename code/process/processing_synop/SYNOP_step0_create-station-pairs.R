#!/usr/local/bin/Rscript
################################################################################
# Purpose: Select SYNOP pairs, preliminary step
# License: GPL v3
# Authors: Andrej Ceglar - Dec. 2020
################################################################################

 
library(geosphere)
library(raster)

#read synop data
synop = read.table("data/data_input/synop.csv", header=TRUE, sep=",")

# calculate distances between stations
dists = c()
for(loc in 4569:(dim(synop)[1]-1))
{
  for(comp in (loc+1):dim(synop)[1])
  {
    dist_ = distm(c(synop$lon[loc], synop$lat[loc]), c(synop$lon[comp], synop$lat[comp]), fun = distHaversine)
    if(dist_[1,1]/1000 <= 100)
      dists = rbind(dists, data.frame(loc1=synop$num[loc], loc2=synop$num[comp], dist=dist_[1,1]/1000))
  }
}

# this step for preliminary assessment of the number of stations pairs within predefined distance range
# count pairs globally, classify in classes from 0 to 100 km ,by 5 km, to obtain frequency distribution
dists.count = c()
for(dist in seq(from=0,to=100,by=5))
{
  dists.count = rbind(dists.count, data.frame(dist=dist, count=length(which(dists$dist<=dist))))
}

# establish pairs pairs 
dists.25 = dists[which(dists$dist<100 & dists$dist>0),]

pair.1 = c()
pair.2 = c()

for(i in 1:dim(dists.25)[1])
{
  lat=synop$lat[which(synop$num==dists.25$loc1[i])]
  lon=synop$lon[which(synop$num==dists.25$loc1[i])]
  pair.1 = rbind(pair.1, data.frame(id=synop$sitename[synop$num==dists.25$loc1[i]], lat=lat, lon=lon))
  
  lat=synop$lat[which(synop$num==dists.25$loc2[i])]
  lon=synop$lon[which(synop$num==dists.25$loc2[i])]
  pair.2 = rbind(pair.2, data.frame(id=synop$sitename[synop$num==dists.25$loc2[i]], lat=lat, lon=lon))
}

Mydata = c()
for(i in 1:dim(pair.1)[1])
{
    Mydata = rbind(Mydata, data.frame(pair=as.character(pair.1$id[i]), member=1, lat=pair.1$lat[i], lon=pair.1$lon[i]))
    Mydata = rbind(Mydata, data.frame(pair=as.character(pair.2$id[i]), member=2, lat=pair.2$lat[i], lon=pair.2$lon[i]))
}

coordinates(Mydata)=~lon+lat
proj4string(Mydata)<- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(Mydata, "data/inter_data/GlobalStationPairs_rev.shp", overwrite = TRUE)
