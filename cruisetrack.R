#map
#assume bubble.R is run
library(maps)

p18n <- cruisesALLn.v2[cruisesALLn.v2$cruise == 'P18N_31DSCG94_3',]
p18nn2 <- na.omit(p18n)
p18nmap <- unique(p18nn2[, c('lat', 'long', 'datetime')])

tn278n <- cruisesALLn.v2[cruisesALLn.v2$cruise == 'TN278', ]
tn278map <- unique(tn278n[, c('lat', 'long', 'datetime')])


skq16n <- cruisesALLn.v2[cruisesALLn.v2$cruise == 'SKQ', ]
skqmap <- unique(skq16n[, c('lat', 'long', 'datetime')])
View(skq16n)


ggplot(skqmap, aes(x = long, y = lat, label = datetime)) + geom_label() + borders('world', fill = 'white') + coord_fixed(xlim = c(-120, -90), ylim = c(8, 40))


#
#

allmap <-  na.omit(unique(subset(cruisesALLn.v2, select = c('cruise', 'station', 'lat', 'long', 'datetime'))))
allmap <- allmap[order(allmap$station), ]

ggplot(allmap, aes(x = long, y = lat, shape = cruise, colour = cruise)) +
  geom_point(size = 4) +
  borders('world', fill = 'darkolivegreen4') +
  coord_fixed(xlim = c(-120, -100), ylim = c(8, 30))

cruisevec <- levels(allmap$cruise)
cruisevec <- cruisevec[-which(cruisevec == "")]

for(c in cruisevec){
  print (c)
  locCruise <- subset(allmap, cruise == c)
  locplot <- ggplot(locCruise, aes(x = long, y = lat, label = datetime)) +
    geom_path(arrow = arrow(angle = 15, ends = 'last')) + geom_label() + borders('world', fill = 'white') + coord_fixed(xlim = c(-120, -100), ylim = c(5, 30)) + ggtitle(c)
  quartz()
  print(locplot)
}




# function for interpolating cruise tracks
lld_interp <- function(locCruise, ndates){
  # lc is one cruise
  startTime <- min(as.POSIXct(locCruise$datetime))
  endTime <- max(as.POSIXct(locCruise$datetime))
  dateout <- round(seq(from = startTime, to = endTime, length = ndates), "day")
  latout <- approx(x = as.POSIXct(locCruise$datetime), y = locCruise$lat, xout = as.POSIXct(dateout))
  lonout <- approx(x = as.POSIXct(locCruise$datetime), y = locCruise$lon, xout = as.POSIXct(dateout))
  return(data.frame("date" = dateout, "lat" = latout$y, "long" = lonout$y))
}

lld_interp(locCruise, 10)


# plotting each cruise track
pdf('cruisetracks.pdf', width = 6, height = 10)
for (c in cruisevec){
  print (c)
  ## subset one cruise of interest
  locCruise <- subset(allmap, cruise == c)
  locCruise$datetime <- as.POSIXct(locCruise$datetime)
  ## interpolate down to x dates
  lldLabels <- lld_interp(locCruise, 7)
  locplot <- ggplot(locCruise, aes(x = long, y = lat, label = datetime)) +
    borders('world', fill = 'darkolivegreen4') + 
    geom_text(data = lldLabels, aes(x = long, y = lat, label = date, hjust = 1.2)) + 
    geom_path(arrow = arrow(angle = 15, ends = 'last'), alpha = 0.7) + 
    geom_point(alpha = 0.7) + 
    coord_fixed(xlim = c(-115, -100), ylim = c(8, 24)) + 
    ggtitle(c)
  # svg(filename = paste('transectmaps', c, '.svg'), width = 7, height = 7)
  plot(locplot)
  # dev.off()
  
}
dev.off()



locplot2 <- ggplot(data = allmap, aes(x = long, y = lat, label = datetime)) +
  facet_wrap(~cruise, nrow = 7) + 
  borders('world', fill = 'darkolivegreen4') + 
  geom_text(aes(x = long, y = lat, label = datetime, hjust = 1.1)) + 
  geom_path(arrow = arrow(angle = 15, ends = 'last'), alpha = 0.7) + 
  geom_point(alpha = 0.7) + 
  coord_fixed(xlim = c(-120, -100), ylim = c(10, 25)) 

locplot2


