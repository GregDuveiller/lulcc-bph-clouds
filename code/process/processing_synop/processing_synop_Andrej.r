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

# this step for preliminary assessment of the number of stations pairs within predefined distance range
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
dists.25 = dists[which(dists$dist<100),]
pair.1 = c()
pair.2 = c()

for(i in 1:dim(dists.25)[1])
{
  lat=synop$lat[which(synop$num==dists.25$loc1[i])]
  lon=synop$lon[which(synop$num==dists.25$loc1[i])]
  which_ = c(which(abs(lon-frac.lon)==min(abs(lon-frac.lon)))[1], which(abs(lat-frac.lat)==min(abs(lat-frac.lat)))[1])
  pair.1 = rbind(pair.1, data.frame(id=dists.25$loc1[i], lat=lat, lon=lon, frac=frac1[which_[1],which_[2]]))
  
  lat=synop$lat[which(synop$num==dists.25$loc2[i])]
  lon=synop$lon[which(synop$num==dists.25$loc2[i])]
  which_ = c(which(abs(lon-frac.lon)==min(abs(lon-frac.lon)))[1], which(abs(lat-frac.lat)==min(abs(lat-frac.lat)))[1])
  pair.2 = rbind(pair.2, data.frame(id=dists.25$loc2[i], lat=lat, lon=lon, frac=frac1[which_[1],which_[2]]))
}

# create pairs for cloudiness comparison
for(p in 1:dim(pair.2)[1])
{
  load(paste("SYNOP_pairs/", pair.1$id[p], ".Rda", sep=""))
  p1 = r
  load(paste("SYNOP_pairs/", pair.2$id[p], ".Rda", sep=""))
  p2 = r
  
  paired = p1
  names(paired) = c("Time", "CA1")
  paired = cbind(paired, data.frame(CA2=NA))
  
  for(t in 1:length(paired[,1]))
    paired$CA2[t] = ifelse(length(which(p2$Time==paired$Time[t]))==1, p2$CA[which(p2$Time==paired$Time[t])], NA)
  
  paired = paired[!is.na(paired$CA1)&!is.na(paired$CA2),]
  paired = cbind(paired, data.frame(year=as.integer(format(paired$Time, "%Y")),month=as.integer(format(paired$Time, "%m")),day=as.integer(format(paired$Time, "%d")),hour=as.integer(format(paired$Time, "%H"))))
  
  clt = c()
  for(month in 1:12)
    for(hour in c(0:23))
      clt = rbind(clt, data.frame(month=month,hour=hour,diff=mean(paired$CA2[paired$month==month&paired$hour==hour]/8-paired$CA1[paired$month==month&paired$hour==hour]/8)*100))
}

p1 = cbind(p1, data.frame(year=as.integer(format(p1[,1], "%Y")), month=as.integer(format(p1[,1], "%m")), day=as.integer(format(p1[,1], "%d")), hour=as.integer(format(p1[,1], "%H"))))
p2 = cbind(p2, data.frame(year=as.integer(format(p2[,1], "%Y")), month=as.integer(format(p2[,1], "%m")), day=as.integer(format(p2[,1], "%d")), hour=as.integer(format(p2[,1], "%H"))))

year.min = min(p1$year, p2$year)
year.max = max(p1$year, p2$year)

paired = seq(as.Date(paste(year.min, "/1/1", sep="")), as.Date(paste(year.max, "/12/31", sep="")), by="")
paired = cbind(paired, data.frame(year=as.integer(format(paired, "%Y")),month=as.integer(format(paired, "%m")),day=as.integer(format(paired, "%d"))))

paired$c.0 = NA
paired$c.1 = NA

c.0 = pair.1$frac[which(pair.1$lat==synop$lat[synop$sitename==pairs[p,1]] & pair.1$lon==synop$lon[synop$sitename==pairs[p,1]])][1]
c.1 = pair.1$frac[which(pair.1$lat==synop$lat[synop$sitename==pairs[p,2]] & pair.1$lon==synop$lon[synop$sitename==pairs[p,2]])][1]

for(month in 1:12)
{
  which.1 = which(paired$month==month)
  for(w_ in which.1)
  {
    wp1_ = which(p1$year==paired$year[w_] & p1$month==paired$month[w_] & p1$day==paired$day[w_])
    if(length(wp1_)>0)
      paired$c.0[w_] = mean(p1$CA[wp1_])
    
    wp2_ = which(p2$year==paired$year[w_] & p2$month==paired$month[w_] & p2$day==paired$day[w_])
    if(length(wp2_)>0)
      paired$c.1[w_] = mean(p2$CA[wp2_])
  }
}

paired


