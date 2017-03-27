library(ggplot2)
library(ggmap)
#https://www.darrinward.com
#lat and longs
#TGT001
tgt001 <- cruisesALL0[cruisesALL0$cruise == 'TGT001' & !is.na(cruisesALL0$cruise),]
long1 <- matrix(tgt001$long)
lat1 <- matrix(tgt001$lat)
longlat1 <- as.data.frame(cbind(long1, lat1))
online1 <- unique(longlat1)
View(online1)

#TGT37
tgt37 <- cruisesALL0[cruisesALL0$cruise == 'TGT37' & !is.na(cruisesALL0$cruise),]
long2 <- matrix(tgt37$long)
lat2 <- matrix(tgt37$lat)
longlat2 <- as.data.frame(cbind(long2, lat2))
online2 <- unique(longlat2)
View(online2)

#TGT66
tgt66 <- cruisesALL0[cruisesALL0$cruise == 'TGT66' & !is.na(cruisesALL0$cruise),]
long3 <- matrix(tgt66$long)
lat3 <- matrix(tgt66$lat)
longlat3 <- as.data.frame(cbind(lat3, long3)) #changed to lat, long bc of website formatting
online3 <- unique(longlat3)
View(online3)


#P18N_31DSCG94_3
p18n <- cruisesALL0[cruisesALL0$cruise == 'P18N_31DSCG94_3' & !is.na(cruisesALL0$cruise),]
long4 <- matrix(p18n$long)
lat4 <- matrix(p18n$lat)
longlat4 <- as.data.frame(cbind(lat4, long4))
online4 <- unique(longlat4)
View(online4)


# map <- get_map(location = c(lon = mean(longlat1$long), lat = mean(longlat1$lat)), zoom = 4, maptype = 'satellite', scale = 2)
# 
# ggmap(map) + geom_point(data = longlat1, aes(x = long, y = lat, fill = 'red', alpha = 0.8), size = 5, shape = 21) + guides(fill = F, alpha = F, size = F)
# not working


#EVEN BETTER MAPPING WEBSITE!!!!
#http://www.copypastemap.com/
cruisecoords <- subset(cruisesALL0, select = c(cruise, lat, long))
head(cruisecoords)
write.csv(cruisecoords, "cruisecoords.csv")
