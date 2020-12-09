#!/usr/local/bin/Rscript
################################################################################
# Purpose: Select SYNOP pairs from final analysis 
# License: GPL v3
# Authors: Andrej Ceglar - Dec. 2020
################################################################################


library(rgdal)
library(solaR)
library(geosphere)


synop.g = readOGR("data/inter_data/synop_analysis",
                  "SYNOPpoints_withAncillaryData")
synop = read.table("data/input_data/ground_station/synop.csv",
                   header = TRUE,
                   sep = ",")
input_synop_path <- "data/input_data/ground_station/SYNOP_pairs_global"


# define empty array
p.1 = c()
p.2 = c()
pairs = c()

# apply selection criteria
synop.g.1 = synop.g[synop.g$lcl_wt_ < 0.05 &
                      synop.g$lcl_tr_ > 0 &
                      synop.g$avg_fc_ < 200 & synop.g$sd_fcl_v < 50, ]
crds = coordinates(synop.g.1)

#
points = readOGR("data/inter_data/synop_analysis", "GlobalStationPairs_rev")
crds.points = coordinates(points)
pair.1 = data.frame(cbind(points$pair[points$member == 1], 
                          crds.points[points$member == 1, ]))
pair.2 = data.frame(cbind(points$pair[points$member == 2], 
                          crds.points[points$member == 2, ]))
colnames(pair.1) = c("id", "lon", "lat")
colnames(pair.2) = c("id", "lon", "lat")

for (i in 1:dim(crds)[1])
{
  dists_ = distGeo(crds[i, ], crds) / 1000
  w_ = which(dists_ > 0 & dists_ <= 100)
  
  if (length(w_) > 0) {
    for (j in 1:length(w_))
    {
      w_1 = which(pair.1$lon == crds[i, 1] & pair.1$lat == crds[i, 2])
      w_2 = which(pair.2$lon == crds[i, 1] & pair.2$lat == crds[i, 2])
      
      w_1 = which(pair.1$lon == crds[w_[j], 1] &
                    pair.1$lat == crds[w_[j], 2])
      w_2 = which(pair.2$lon == crds[w_[j], 1] &
                    pair.2$lat == crds[w_[j], 2])
      
      p.1 = rbind(
        p.1,
        data.frame(
          id = synop$sitename[synop$lon == crds[i, 1] &
                                synop$lat == crds[i, 2]],
          lon = crds[i, 1],
          lat = crds[i, 2],
          area = synop.g.1$lcl_tr_[i],
          dist = dists_[w_[j]]
        )
      )
      p.2 = rbind(
        p.2,
        data.frame(
          id = synop$sitename[synop$lon == crds[w_[j], 1] &
                                synop$lat == crds[w_[j], 2]],
          lon = crds[w_[j], 1],
          lat = crds[w_[j], 2],
          area = synop.g.1$lcl_tr_[w_[j]],
          dist = dists_[w_[j]]
        )
      )
    }
  }
}

# assess differences in cloudiness
paired = c()
for (i in 1:dim(p.1)[1])
{
  if (!is.nan(p.1$area[i]) &
      !is.nan(p.2$area[i]) &
      file.exists(paste(input_synop_path, '/', p.1[i, 1], ".Rda", sep = "")) &
      file.exists(paste(input_synop_path, '/', p.2[i, 1], ".Rda", sep = "")))
  {
    load(paste(input_synop_path, '/', p.1[i, 1], ".Rda", sep = ""))
    p1 = r
    load(paste(input_synop_path, '/', p.2[i, 1], ".Rda", sep = ""))
    p2 = r
    
    p1$Time = local2Solar(p1$Time, lon = p.1$lon[i])
    p2$Time = local2Solar(p2$Time, lon = p.2$lon[i])
    p1 = cbind(
      p1,
      data.frame(
        year = as.integer(format(p1$Time, "%Y")),
        month = as.integer(format(p1$Time, "%m")),
        day = as.integer(format(p1$Time, "%d")),
        hour = as.integer(format(p1$Time, "%H"))
      )
    )
    p2 = cbind(
      p2,
      data.frame(
        year = as.integer(format(p2$Time, "%Y")),
        month = as.integer(format(p2$Time, "%m")),
        day = as.integer(format(p2$Time, "%d")),
        hour = as.integer(format(p2$Time, "%H"))
      )
    )
    
    p1$CA2 = NA
    
    area.h = max(p.1$area[i], p.2$area[i])
    area.l = min(p.1$area[i], p.2$area[i])
    if (p.2$area[i] > p.1$area[i]) {
      H = 7
      L = 2
    }
    if (p.2$area[i] <= p.1$area[i]) {
      H = 2
      L = 7
    }
    
    # consider period between 2004-2014 / match remote sensing temporal range
    p1 = p1[p1$year >= 2004 & p1$year <=2014,]
    p2 = p2[p2$year >= 2004 & p2$year <=2014,]
    
    test = match(data.frame(t(p1[, 3:6])), data.frame(t(p2[, 3:6])))
    p1$CA2[which(!is.na(test))] = p2$CA[test[which(!is.na(test))]]
    
    nnas = which(!is.na(p1$CA) & !is.na(p1$CA2))
    p1 = p1[nnas, ]
    
    if (dim(p1)[1] > 0)
    {
      for (month in 1:12) {
        for (hour in 0:23) {
          which_ = which(p1$month == month & p1$hour == hour)
          if (length(which_) > 0)
            paired = rbind(
              paired,
              data.frame(
                pair = i,
                month = month,
                hour = hour,
                area.h = area.h,
                area.l = area.l,
                dist = p.1$dist[i],
                diff = mean(p1[which_, H] - p1[which_, L]) / 8 * 100,
                nyrs = length(unique(p1$year[which_])),
                N = length(which_),
                lon = (p.1$lon[i] + p.2$lon[i]) * 0.5,
                lat = (p.1$lat[i] + p.2$lat[i]) * 0.5
              )
            )
        }
      }
    }
    print(paste("Done pair", i, "out of ", dim(p.1)[1]))
  }
}
paired.new = paired
paired.new$hl = paired.new$area.h - paired.new$area.l # difference between forest areas surrounding both stations

# export to final result folder

final_results_path <- 'data/final_data/study_results'
save('paired.new', file = paste0(final_results_path, '/groundstation_analysis/',
                                 'final_synop_paired_data.RData'))
