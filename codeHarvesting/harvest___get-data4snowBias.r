library(dplyr)
library(tidyr)
library(raster)
library(ncdf4)

load("dataResults/Results_from_Jed/satsite.Rda")

## to get data from MODIS delta results

# select the points
tbl.pts <- as.data.frame(satsite) %>%
  filter(time >= as.Date('2004-01-01'),
         time <= as.Date('2014-12-31')) %>%
  group_by(Kod) %>%
  summarize(x = signif(unique(lon), digits = 4), 
            y = signif(unique(lat), digits = 4))

# extract data from MODIS S4T values
dat0 <- raster::extract(x = brick('dataFigures/rstr_dCFC_MOD05_FOR.nc'), 
                        y = dplyr::select(tbl.pts, -Kod)) 
colnames(dat0) <- month.abb
dat1 <- cbind(dplyr::select(tbl.pts, Kod), dat0)
dat2 <- dat1[apply(is.na(dat0), 1, sum) < 5,]
dat3 <- pivot_longer(data = dat2, cols = month.abb, 
                     names_to = 'month', values_to = 'dCFC')

tbl.s4t <- dat3 %>%
  transmute(Kod = Kod, 
            month = factor(month, levels = month.abb),
            dCFC_afforestation = dCFC)

save('tbl.s4t', file = 'dataFigures/df.groundDataRS.snowbias.Rda')

