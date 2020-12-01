# definition of zones of interest

# function to delimit zones of interest (relevant for diff subplots)
mk.zone <- function(uid, lbl, xmn, xmx, ymn, ymx){
  zn <- data.frame(lbl = lbl, uid = uid, 
                   lon = c(xmn, xmn, xmx, xmx, xmn), 
                   lat = c(ymn, ymx, ymx, ymn, ymn))}

# make the actual zones
zn.eur <- mk.zone('eur','West/Central Europe',-10,20,38,62)
zn.nam <- mk.zone('nam','North America',-120,-60,40,60)
zn.crn <- mk.zone('crn','US corn belt',-96,-84,38,44)   # -95,-82,36,44
zn.ind <- mk.zone('ind','Indian subcontinent',66,90,5,29)
zn.aus <- mk.zone('aus','Eastern Australia',138,155,-44,-21)
zn.rus <- mk.zone('rus','Russia/East Europe',20,110,45,65)
zn.ama <- mk.zone('ama','Southern Amazon',-70,-45,-15,-5)
zn.afr <- mk.zone('afr','Southern Africa',10,42,-30,-5)
zn.chi <- mk.zone('chi','Eastern China',105,125,20,37)

zn <- bind_rows(zn.nam, zn.crn, zn.eur, zn.rus, zn.ama, zn.afr, zn.ind, zn.aus, zn.chi)


