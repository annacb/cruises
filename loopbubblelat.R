#for loop to plot plots!
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(directlabels)

#reading in the data
horakbakk_nuts <- read.csv('horakbakker_nutrients.csv')
horakbakk_nuts2 <- subset(horakbakk_nuts, select = -c(date, time, bottDepth))
skqCTDo <- read.csv('skq16CTDo.csv')
skqCTDo2 <- subset(skqCTDo, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(horakbakk_nuts2, skqCTDo2)  
skq16krognuts <- read.csv('krogslund_nuts.csv')
skq16krognuts2 <- subset(skq16krognuts, select = -c(date, time, bottDepth))
cruisesALL1 <- rbind(horakbakk_nuts2, skqCTDo2, skq16krognuts2)
cruisesALL2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, datetime, press))
cruisesALL2.v2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, press))
#melting data
cruisesALLm.v2 <- data.frame(melt(cruisesALL2.v2, id.vars = c('cruise', 'station', 'lat', 'long', 'datetime', 'depth')))
#omit na's
cruisesALLn.v2 <- na.omit(cruisesALLm.v2)

#interpolating
stdDepths2 <- c(seq(from = 0, to = 1250, by = 10))
#fixing time format..
cruisesALLn.v2$datetime <- as.POSIXct(as.character(cruisesALLn.v2$datetime, format = c('%Y-%m-%d %H:%M')))
#fixing negative nitrite values from homz
cruisesALLn.v2[(cruisesALLn.v2$cruise == "SKQ" & cruisesALLn.v2$variable == 'NO2' & cruisesALLn.v2$value < 0), ]$value <- 0
#NEED TO FIX LONGITUDE OF P18N... it used to say '250,' but now i want it to be -110...
cruisesALLn.v2[cruisesALLn.v2$cruise == 'P18N_31DSCG94_3', ]$long <- -110
#removing station 2 (on land..) and < -111 & > -107 W from TN278 since it's too noisy
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$station == 2), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$long < -111), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$long > -109.5), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$lat > 22.5), ]

#actually.. remove any lats above 22.5 degrees N
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$lat > 22.5), ]
#also i want to remove everything higher than station 16 from HOMZ, and station 6-7
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "SKQ" & cruisesALLn.v2$station > 10), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "SKQ" & cruisesALLn.v2$station == 8), ]

##

##

#ADD CODE HERE TO REMOVE NUTRIENTS THAT DONT HAVE DATA (NH4 for the first 5 crusies, NO2 for 2 cruises, ...)

##

##




#approx function for interpolating depths
approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths2, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}


cruisevec <- levels(cruisesALLn.v2$cruise)
cruisevec <- cruisevec[-which(cruisevec == "")]


# plotting each cruise track
for (c in cruisevec){
  print (c)
  ## subset one cruise of interest
  cruise <- subset(cruisesALLn.v2, cruise == c)
  ## interpolate depths
  cruisedinterpDep <- ddply(.data = cruise, .variables = c('cruise', 'station', 'lat', 'long', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)
  ## make hour vector..
  cruiseLatVec <- seq(min(cruisedinterpDep$lat) - 1, max(cruisedinterpDep$lat) + 1, by = 0.01)
  ## run this function
  approxL <- function(df){
    if (nrow(na.omit(df)) >= 2) {
      listResult <- approx(xy.coords(df$lat, df$value), xout = cruiseLatVec, method = 'linear', rule = 2)
      data.frame(lat = listResult$x, value = listResult$y)
    }
  } 
  ## interpolate dep AND time
  cruiseinterpDepTime <- ddply(.data = cruisedinterpDep, .variables = c('variable', 'DepthBin'), approxL, .inform = TRUE)
  ## make time sequences
  l0 <- min(cruiseLatVec) 
  l1 <- max(cruiseLatVec)
  lseq <- seq(from = l0, to = l1, by = 1)
  ## lats/longs for plot title
  lats <- unique(cruise$lat)
  longs <- unique(cruise$long)
  ## ODZ top and bottom depths
  ODZdepthfx <- function(df) {
    odzIdx <- which(df$V1 <= 10)
    firstIdx <- odzIdx[1]
    topODZ <- df$DepthBin[firstIdx]
    lastIdx <- odzIdx[length(odzIdx)]
    bottODZ <- df$DepthBin[lastIdx]
    data.frame(cruise = df[1, 1], topODZ, bottODZ)
  }
  O2data <- na.omit(cruisedinterpDep[cruisedinterpDep$variable == 'O2',])
  avgO2 <- ddply(.data = O2data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))
  ODZdepths <- ddply(.data = avgO2, .variables = c('cruise'), function(df) ODZdepthfx(df))
  hlineO2 <- data.frame(z = c(ODZdepths$topODZ, ODZdepths$bottODZ), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ', 'TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))
  nutvec <- levels(cruiseinterpDepTime$variable)
  nutvec <- nutvec[7] #if i want NH4, make this '5:9' instead of '5:8'
  for (n in nutvec){
    ##
    ## code for the plot
    # if (is.na(cruise[cruise$variable == n, ]$value)) {
    # print(n)
    # hmm... fix this...
    # }
    # else {
    CTDsamples <- unique(subset(cruise, variable == n, select = c(depth, lat)))
    #
    nutoxyplot <- ggplot(subset(cruiseinterpDepTime, variable == n), aes(lat, DepthBin)) +
    geom_tile(aes(fill = value)) + facet_wrap('cruise', nrow = 7) +
    scale_y_reverse(limits = c(1200, 0), 
                    breaks = seq(from = 0, to = 1200, by = 100), 
                    name = 'Depth (m)') + 
    scale_x_continuous(limits = c(l0, l1),                          
                       breaks = round(lseq),
                       name = 'Latitude (deg N)') + 
    geom_point(data = CTDsamples, aes(x = lat, y = depth), shape = ".") + 
    scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = paste('[', n, '] ', '(umol/kg)', sep = '')) + 
    geom_contour(data = subset(cruiseinterpDepTime, variable == 'O2'), aes(x = lat, y = DepthBin, z = log10(value), colour = ..level..)) + 
    geom_hline(aes(yintercept = z), hlineO2, lty = 5, lwd = 1.3) +
    labs(size = paste('[', n, '] ', '(umol/kg)', sep = ''), colour = 'log([O2]) (umol/kg)', 
         title = paste('ETNP', c, 'Cruise in', unique(format(cruise$datetime, '%Y'))),
         subtitle = paste('(', min(lats), 'N,', min(longs), 'W) to', '(', max(lats), 'N,', max(longs), 'W)')) +
    theme(axis.text.x = element_text(angle = 90, size = 16), 
          axis.text.y = element_text(size = 16),
          text = element_text(size=20), 
          plot.title = element_text(size = 25, hjust = 0),
          panel.background = element_blank())
    nutoxyplot <- direct.label(nutoxyplot, list(colour = 'dodgerblue4', 'top.bumptwice', cex = 1.2))
    # svg(filename = paste(unique(format(cruise$datetime, '%Y')), c, n, '.svg', sep = ''))
    pdf(paste(unique(format(cruise$datetime, '%Y')), c, n, '.pdf', sep = ''), width = 10, height = 20)
    print(nutoxyplot)
    dev.off()
  }
  # }
}
