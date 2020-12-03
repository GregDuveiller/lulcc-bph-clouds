# 
library(geosphere)

#read data from Jed
synop = read.table("../cescatti/data/synop.csv", header=TRUE, sep=",")

# calculate distances between stations
dists = c()
for(loc in 1:(dim(synop)[1]-1))
{
  for(comp in (loc+1):dim(synop)[1])
  {
    dist_ = distm(c(synop$lon[loc], synop$lat[loc]), c(synop$lon[comp], synop$lat[comp]), fun = distHaversine)
    if(dist_[1,1]/1000 <= 100)
      dists = rbind(dists, data.frame(loc1=synop$num[loc], loc2=synop$num[comp], dist=dist_[1,1]/1000))
  }
}

# htis step for preliminary assessment of the number of stations pairs within predefined distance range
# count pairs globally, classify in classes from 0 to 100 km ,by 5 km, to obtain frequency distribution
dists.count = c()
for(dist in seq(from=0,to=100,by=5))
{
  dists.count = rbind(dists.count, data.frame(dist=dist, count=length(which(dists$dist<=dist))))
}

# create paired dataframe
library(ncdf4)
frac = nc_open("../cescatti/ESACCI-LC-L4-LCCS_0.05dd_EFO_evergreen_forest.nc")
frac1 = ncvar_get(frac, "EFO", c(1,1), c(-1,-1))
frac.lon = ncvar_get(frac, "lon", 1, -1)
frac.lat = ncvar_get(frac, "lat", 1, -1)


# establish pairs pairs 
dists.25 = dists[which(dists$dist<100 & dists$dist>0),]

pair.1 = c()
pair.2 = c()

for(i in 1:dim(dists.25)[1])
{
  lat=synop$lat[which(synop$num==dists.25$loc1[i])]
  lon=synop$lon[which(synop$num==dists.25$loc1[i])]
  which_ = c(which(abs(lon-frac.lon)==min(abs(lon-frac.lon)))[1], which(abs(lat-frac.lat)==min(abs(lat-frac.lat)))[1])
  pair.1 = rbind(pair.1, data.frame(id=synop$sitename[synop$num==dists.25$loc1[i]], lat=lat, lon=lon, area=frac.area(frac1, which_[1],which_[2],3)))
  
  lat=synop$lat[which(synop$num==dists.25$loc2[i])]
  lon=synop$lon[which(synop$num==dists.25$loc2[i])]
  which_ = c(which(abs(lon-frac.lon)==min(abs(lon-frac.lon)))[1], which(abs(lat-frac.lat)==min(abs(lat-frac.lat)))[1])
  pair.2 = rbind(pair.2, data.frame(id=synop$sitename[synop$num==dists.25$loc2[i]], lat=lat, lon=lon, area=frac.area(frac1, which_[1],which_[2],3)))
}

#global assessment
selected = c()
for(i in 1:dim(pair.1)[1])
{
  if(!is.nan(pair.1$area[i]) && !is.nan(pair.2$area[i]) && (pair.1$area[i]>0 || pair.2$area[i]>0)) {
    selected = c(selected, data.frame(wmo=pair.1$id[i]))
    selected = c(selected, data.frame(wmo=pair.2$id[i]))
  }
}

selected = unique(selected)
selected = as.character(unlist(selected))

Mydata = c()
for(i in 1:dim(pair.1)[1])
{
  if(!is.nan(pair.1$area[i]) && !is.nan(pair.2$area[i]) && (pair.1$area[i]>0 || pair.2$area[i]>0)) {
    Mydata = rbind(Mydata, data.frame(pair=as.character(pair.1$id[i]), member=1, lat=pair.1$lat[i], lon=pair.1$lon[i]))
    Mydata = rbind(Mydata, data.frame(pair=as.character(pair.2$id[i]), member=2, lat=pair.2$lat[i], lon=pair.2$lon[i]))
  }
}

coordinates(Mydata)=~lon+lat
proj4string(Mydata)<- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(Mydata, "GlobalStationPairs.shp", overwrite=TRUE)

####### select pairs from final analysis
p.1 = c()
p.2 = c()
pairs = c()

synop.g = readOGR("SYNOP_pairs/greg", "SYNOPpoints_withAncillaryData")

synop.g$area.o=NA
for(i in 1:length(synop.g$POINT_I))
{
  which_ = c(which(abs(crds[i,1]-frac.lon)==min(abs(crds[i,1]-frac.lon)))[1], which(abs(crds[i,2]-frac.lat)==min(abs(crds[i,2]-frac.lat)))[1])
  synop.g$area.o[i] = frac.area(frac1, which_[1],which_[2],3)
}
synop.g.1 = synop.g[synop.g$lcl_wt_<0.05 & synop.g$lcl_tr_ > 0 & synop.g$avg_fc_ < 200 & synop.g$sd_fcl_v < 50,]
crds = coordinates(synop.g.1)


for(i in 1:dim(crds)[1])
{
  dists_ = distGeo(crds[i,], crds) / 1000
  w_ = which(dists_ > 0 & dists_ <= 100)
  
  if(length(w_)>0){
    
    for(j in 1:length(w_))
    {
      w_1 = which(pair.1$lon==crds[i,1]&pair.1$lat==crds[i,2])
      w_2 = which(pair.2$lon==crds[i,1]&pair.2$lat==crds[i,2])
      
      w_1 = which(pair.1$lon==crds[w_[j],1]&pair.1$lat==crds[w_[j],2]) 
      w_2 = which(pair.2$lon==crds[w_[j],1]&pair.2$lat==crds[w_[j],2])
      
      p.1 = rbind(p.1, data.frame(id= synop$sitename[synop$lon==crds[i,1]&synop$lat==crds[i,2]], lon=crds[i,1],lat=crds[i,2],area=synop.g.1$lcl_tr_[i], area.o=synop.g.1$area.o[i], dist=dists_[w_[j]]))
      p.2 = rbind(p.2, data.frame(id= synop$sitename[synop$lon==crds[w_[j],1]&synop$lat==crds[w_[j],2]], lon=crds[w_[j],1],lat=crds[w_[j],2],area=synop.g.1$lcl_tr_[w_[j]], area.o=synop.g.1$area.o[w_[j]], dist=dists_[w_[j]]))
    }
  }
}

# assess differences in cloudiness
paired = c()
for(i in 1022:dim(p.1)[1])
{
  if(!is.nan(p.1$area[i]) & !is.nan(p.2$area[i]) & file.exists(paste("SYNOP_pairs_global/", p.1[i,1], ".Rda", sep="")) & file.exists(paste("SYNOP_pairs_global/", p.2[i,1], ".Rda", sep="")))
  {
    load(paste("SYNOP_pairs_global/", p.1[i,1], ".Rda", sep=""))
    p1 = r
    load(paste("SYNOP_pairs_global/", p.2[i,1], ".Rda", sep=""))
    p2 = r
    
    p1$Time = local2Solar(p1$Time, lon=p.1$lon[i])
    p2$Time = local2Solar(p2$Time, lon=p.2$lon[i])
    p1 = cbind(p1, data.frame(year=as.integer(format(p1$Time, "%Y")),month=as.integer(format(p1$Time, "%m")),day=as.integer(format(p1$Time, "%d")),hour=as.integer(format(p1$Time, "%H"))))
    p2 = cbind(p2, data.frame(year=as.integer(format(p2$Time, "%Y")),month=as.integer(format(p2$Time, "%m")),day=as.integer(format(p2$Time, "%d")),hour=as.integer(format(p2$Time, "%H"))))
    
    p1$CA2 = NA
    
    area.h = max(p.1$area[i],p.2$area[i])
    area.l = min(p.1$area[i],p.2$area[i])
    if(p.2$area[i] > p.1$area[i]){
      H = 7
      L = 2
    }
    if(p.2$area[i] <= p.1$area[i]){
      H = 2
      L = 7
    }
    
    test=match(data.frame(t(p1[,3:6])), data.frame(t(p2[,3:6])))
    p1$CA2[which(!is.na(test))] = p2$CA[test[which(!is.na(test))]]
    
    nnas = which(!is.na(p1$CA) & !is.na(p1$CA2))
    p1 = p1[nnas,]
    
    if(dim(p1)[1] >0)
    {
      for(month in 1:12) {
        for(hour in 0:23) {
          which_ = which(p1$month==month & p1$hour==hour)
          if(length(which_) > 0)
            paired = rbind(paired, data.frame(pair=i, month=month, hour=hour, area.h=area.h, area.l=area.l, dist=p.1$dist[i], diff=mean(p1[which_,H]-p1[which_,L]) / 8 * 100, N=length(which_)))
        }
      }
    }    
    print(paste("Done pair", i, "out of ", dim(p.1)[1]))
  }
}
paired = paired[paired$N>=300,] # take only obsrevations with more than 300 reports
paired$hour = paired$hour-1 # adjust hour
paired$hl = paired$area.h-paired$area.l # difference between station forest areas

paired

##### functions
frac.area = function(frac, i, j, N)
{
  count = 0
  area = 0
  for(k in -N:N)
  {
    for(l in -N:N)
    {
      if(abs(k)!=N && abs(l)!=N)
      {
        area = area + ifelse(is.nan(frac[i+k,j+l]), 0, frac[i+k,j+l])
        count = count + 1
      }
    }
  }
  return(area / count)
}

