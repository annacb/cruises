#for loop to plot plots!
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(directlabels)
library(cowplot)

#reading in the data
horakbakk_nuts <- read.csv('horakbakker_nutrients.csv')
horakbakk_nuts2 <- subset(horakbakk_nuts, select = -c(date, time, bottDepth))
skqCTDo <- read.csv('skq16CTDo.csv')
skqCTDo2 <- subset(skqCTDo, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(horakbakk_nuts2, skqCTDo2)  
skq16krognuts <- read.csv('krogslund_nutrients.csv')
skq16krognuts2 <- subset(skq16krognuts, select = -c(date, time, bottDepth))
cruisesALL1 <- rbind(horakbakk_nuts2, skqCTDo2, skq16krognuts2)
cruisesALL2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, datetime, lat, long, press))
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
cruisesALLn.v2[(cruisesALLn.v2$cruise == "HOMZ16" & cruisesALLn.v2$variable == 'NO2' & cruisesALLn.v2$value < 0), ]$value <- 0
#removing station 2 (on land..) and < -111 & > -107 W from TN278 since it's too noisy
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$station == 2), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$long < -111), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$long > -109), ]
cruisesALLn.v2 <- cruisesALLn.v2[!(cruisesALLn.v2$cruise == "TN278" & cruisesALLn.v2$lat > 22.5), ]




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
  cruisedinterpDep <- ddply(.data = cruise, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)
  ## make hour vector..
  cruiseHrVec <- seq(min(cruisedinterpDep$datetime) - 3*60*60, max(cruisedinterpDep$datetime) + 3*60*60, by = 'hour')
  ## run this function
  approxT <- function(df){
    if (nrow(na.omit(df)) >= 2) {
      listResult <- approx(xy.coords(df$datetime, df$value), xout = cruiseHrVec, method = 'linear', rule = 2)
      data.frame(datetime = listResult$x, value = listResult$y)
    }
  } 
  ## interpolate dep AND time
  cruiseinterpDepTime <- ddply(.data = cruisedinterpDep, .variables = c( 'variable', 'DepthBin'), approxT, .inform = TRUE)
  ## make time sequences
  t0 <- min(subset(cruise, variable == 'O2')$datetime) 
  t1 <- max(subset(cruise, variable == 'O2')$datetime)
  t0 <- as.POSIXct(round(min(subset(cruise, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
  tseq <- seq(from = round(t0, 'day'), to = round(t1, 'day'), by = 86400/2)
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
  hlineO2 <- data.frame(z = c(ODZdepths$topODZ, ODZdepths$bottODZ), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16', 'TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))
  nutvec <- levels(cruiseinterpDepTime$variable)
  nutvec <- nutvec[5:9]
  for (n in nutvec){
  ##
  ## code for the plot
    # if (is.na(cruise[cruise$variable == n, ]$value)) {
      # print(n)
      # hmm... fix this...
    # }
    # else {
    CTDsamples <- unique(subset(cruise, variable == n, select = c(depth, datetime)))
    #
      nutoxyplot <- ggplot(subset(cruiseinterpDepTime, variable == n), aes(datetime, DepthBin)) +
      geom_tile(aes(fill = value)) + 
      scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 100), name = 'Depth (m)') + 
      scale_x_datetime(limits = c(t0, t1), 
                       breaks = tseq,
                       labels = scales::date_format(format = '%m/%d\n%H:%M', tz = 'US/Pacific'),
                       name = 'Time (day, hour)') + 
      geom_point(data = CTDsamples, aes(x = datetime, y = depth), shape = ".") + 
      scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = paste('[', n, '] ', '(umol/kg)', sep = '')) + 
      geom_contour(data = subset(cruiseinterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) + 
      geom_hline(aes(yintercept = z), hlineO2, lty = 5) +
      labs(size = paste('[', n, '] ', '(umol/kg)', sep = ''), colour = 'log([O2]) (umol/kg)', 
           # title = paste('ETNP', c, 'Cruise in', unique(format(cruise$datetime, '%Y'))),
           subtitle = paste('(', min(lats), 'N,', min(longs), 'W) to', '(', max(lats), 'N,', max(longs), 'W)')) +
      theme(axis.text.x = element_text(angle = 90, size = 16),
            axis.text.y = element_text(size = 16),
            text = element_text(size=20),
            plot.title = element_text(size = 25, hjust = 0))
      nutoxyplot <- direct.label(nutoxyplot, list(colour = 'dodgerblue4', 'top.bumptwice', cex = 1.2))
      pdf(paste(unique(format(cruise$datetime, '%Y')), c, n, '.pdf', sep = ''), width = 10, height = 6)
      print(nutoxyplot)
      dev.off()
    }
  # }
}



  
  
