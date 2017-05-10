#LOAD PAGACKES: reshape2, ggplot2, plyr
library(reshape2)
library(ggplot2)
library(plyr)


#reading in the data
horakbakk_nuts <- read.csv('horakbakker_nutrients.csv')
horakbakk_nuts2 <- subset(horakbakk_nuts, select = -c(date, time, bottDepth))
skqCTDo <- read.csv('skq16CTDo.csv')
skqCTDo2 <- subset(skqCTDo, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(horakbakk_nuts2, skqCTDo2)  
skq16krognuts <- read.csv('krogslund_nutrients.csv')
skq16krognuts2 <- subset(skq16krognuts, select = -c(date, time, bottDepth))
cruisesALL1 <- rbind(horakbakk_nuts2, skqCTDo2, skq16krognuts2)
cruisesALL2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, datetime, press))

#melting data
cruisesALLm <- data.frame(melt(cruisesALL2, id.vars = c('cruise', 'station', 'depth', 'lat', 'long')))

#interpolating
#WOA standard depths
by5 <- seq(0, 100, 5)
by25 <- seq(125, 500, 25)
by50 <- seq(550, 2000, 50)
by100 <- seq(2100, 9000, 100)
stdDepths <- c(by5, by25, by50, by100)

cruisesALLn <- na.omit(cruisesALLm)


##
cruisesALLn <- cruisesALLn[!(cruisesALLn$cruise == "TN278" & cruisesALLn$station == 2), ]
cruisesALLn <- cruisesALLn[!(cruisesALLn$cruise == "TN278" & cruisesALLn$long < -111), ]
cruisesALLn <- cruisesALLn[!(cruisesALLn$cruise == "TN278" & cruisesALLn$long > -109), ]
cruisesALLn <- cruisesALLn[!(cruisesALLn$cruise == "TN278" & cruisesALLn$lat > 22.5), ]
##


#

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}

interp <- ddply(.data = cruisesALLn, .variables = c('cruise', 'station', 'variable'), function(df) approxR(df), .inform = TRUE)


#plotting
#making cruises in order by date
levels(interp$cruise)

interp$cruise <- factor(interp$cruise, levels = c('TGT001','TGT37', 'TGT66', 'P18N_31DSCG94_3', 'clivar', 'TN278', 'SKQ'), labels = c('TGT001','TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

#functions to generate horizontal line
#OXYGEN
ODZdepthfx <- function(df) {
  odzIdx <- which(df$V1 <= 10)
  firstIdx <- odzIdx[1]
  topODZ <- df$DepthBin[firstIdx]
  lastIdx <- odzIdx[length(odzIdx)]
  bottODZ <- df$DepthBin[lastIdx]
  data.frame(cruise = df[1, 1], topODZ, bottODZ)
}

#PHOSPHATE
PO4depthfx <- function(df) {
  Idx <- which(df$V1 >= 2.5)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}

#SILICATE
SiO4depthfx <- function(df) {
  Idx <- which(df$V1 >= 25)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}

#NITRITE
NO2depthfx <- function(df) {
  NO2Idx <- which(df$V1 >= 0.3)
  firstIdx <- NO2Idx[1]
  topNO2 <- df$DepthBin[firstIdx]
  lastIdx <- NO2Idx[length(NO2Idx)]
  bottNO2 <- df$DepthBin[lastIdx]
  data.frame(cruise = df[1, 1], topNO2, bottNO2)
}

#NITRATE
NO3depthfx <- function(df) {
  Idx <- which(df$V1 >= 15)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}

#AMMONIUM
NH4depthfx <- function(df) {
  Idx <- which(df$V1 >= 0.1)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}

#TEMP
tempdepthfx <- function(df) {
  Idx <- which(df$V1 <= 15)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}

#SALINITY
saldepthfx <- function(df) {
  Idx <- which(df$V1 >= 34.5)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}


#make a pdf with this code (comment and uncomment as needed...)
# pdf("depth profiles WITH AVG AND HLINE.pdf", width=7, height=5)

#OXYGEN
O2data <- na.omit(interp[interp$variable == 'O2',])

avgO2 <- ddply(.data = O2data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

ODZdepths <- ddply(.data = avgO2, .variables = c('cruise'), function(df) ODZdepthfx(df))

hlineO2 <- data.frame(z = c(ODZdepths$topODZ, ODZdepths$bottODZ), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ', 'TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

ggplot() + geom_line(aes(DepthBin, value, group = station), data = O2data, col = 'skyblue1') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgO2, aes(DepthBin, V1), color = 'darkblue', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineO2, lty = 5) + scale_x_reverse(limits = c(1250, 0), breaks = seq(from = 0, to = 1250, by = 250), name = 'Depth (m)') + scale_y_continuous(name = '[O2] umol/kg', limits = c(0, 225), breaks = seq(from = 0, to = 225, by = 75)) + coord_flip() + ggtitle("ETNP Oxygen Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#PHOSPHATE
PO4data <- na.omit(interp[interp$variable == 'PO4',])

avgPO4 <- ddply(.data = PO4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

PO4depth <- ddply(.data = avgPO4, .variables = c('cruise'), function(df) PO4depthfx(df))

hlinePO4 <- data.frame(z = c(PO4depth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

# need to use 'jacdummy' variable to keep the other cruises included in the facets even though there is no data for them...
# used for NO2 and NH4

#thank you jacob!!
jacdummy <- interp[1:length(levels(interp$cruise)),]
jacdummy[,] <- NA
jacdummy$cruise <- levels(interp$cruise)

ggplot() + geom_line(aes(DepthBin, value, group = station), data = PO4data, col = 'pink') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgPO4, aes(DepthBin, V1), color = 'violetred1', lwd = 0.75) + geom_vline(aes(xintercept = z), hlinePO4, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[PO4] umol/kg', limits = c(0, 4)) + ggtitle("ETNP Phosphate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#SILICATE
SiO4data <- na.omit(interp[interp$variable == 'SiO4',])

avgSiO4 <- ddply(.data = SiO4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

SiO4depth <- ddply(.data = avgSiO4, .variables = c('cruise'), function(df) SiO4depthfx(df))

hlineSiO4 <- data.frame(z = c(SiO4depth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

ggplot() + geom_line(aes(DepthBin, value, group = station), data = SiO4data, col = 'peachpuff2') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgSiO4, aes(DepthBin, V1), color = 'tan4', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineSiO4, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[SiO4] umol/kg', limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 25)) + ggtitle("ETNP Silicate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#NITRITE
NO2data <- na.omit(interp[interp$variable == 'NO2',])

avgNO2 <- ddply(.data = NO2data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

NO2depth <- ddply(.data = avgNO2, .variables = c('cruise'), function(df) NO2depthfx(df))

hlineNO2 <- data.frame(z = c(NO2depth$topNO2, NO2depth$bottNO2), cruise = c('TGT66', 'P18N', 'clivar', 'TN278', SKQ, 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

NO2data2 <- rbind(NO2data, jacdummy)

ggplot() + geom_line(aes(DepthBin, value, group = station), data = NO2data2, col = 'yellowgreen') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNO2, aes(DepthBin, V1), color = 'springgreen4', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineNO2, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO2] umol/kg', limits = c(0, 8)) + ggtitle("ETNP Nitrite Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#NITRATE
NO3data <- na.omit(interp[interp$variable == 'NO3',])

avgNO3 <- ddply(.data = NO3data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

NO3depth <- ddply(.data = avgNO3, .variables = c('cruise'), function(df) NO3depthfx(df))

hlineNO3 <- data.frame(z = c(NO3depth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

ggplot() + geom_line(aes(DepthBin, value, group = station), data = NO3data, col = 'thistle3') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNO3, aes(DepthBin, V1), color = 'thistle4', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineNO3, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO3] umol/kg', limits = c(0, 50)) + ggtitle("ETNP Nitrate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#AMMONIUM
NH4data <- na.omit(interp[interp$variable == 'NH4',])

avgNH4 <- ddply(.data = NH4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

NH4depth <- ddply(.data = avgNH4, .variables = c('cruise'), function(df) NH4depthfx(df))

hlineNH4 <- data.frame(z = c(NH4depth$topIdx), cruise = c('TN278', 'SKQ'))

NH4data2 <- rbind(NH4data, jacdummy)

ggplot() + geom_line(aes(DepthBin, value, group = station), data = NH4data2, col = 'sandybrown') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNH4, aes(DepthBin, V1), color = 'darkorange2', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineNH4, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NH4] umol/kg', limits = c(0, 2), breaks = seq(from = 0, to = 2, by = 1)) + ggtitle("ETNP Ammonium Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#TEMP
tempdata <- na.omit(interp[interp$variable == 'temp',])

avgtemp <- ddply(.data = tempdata, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

tempdepth <- ddply(.data = avgtemp, .variables = c('cruise'), function(df) tempdepthfx(df))

hlinetemp <- data.frame(z = c(tempdepth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

ggplot() + geom_line(aes(DepthBin, value, group = station), data = tempdata, col = 'tomato') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgtemp, aes(DepthBin, V1), color = 'darkred', lwd = 0.75) + geom_vline(aes(xintercept = z), hlinetemp, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Temp (˚C)', limits = c(0, 30)) + coord_flip() + ggtitle("ETNP Water Temperature 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#SALINITY
saldata <- na.omit(interp[interp$variable == 'sal',])

avgsal <- ddply(.data = saldata, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

saldepth <- ddply(.data = avgsal, .variables = c('cruise'), function(df) saldepthfx(df))

hlinesal <- data.frame(z = c(saldepth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'SKQ'))

ggplot() + geom_line(aes(DepthBin, value, group = station), data = saldata, col = 'grey') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgsal, aes(DepthBin, V1), color = 'grey33', lwd = 0.75) + geom_vline(aes(xintercept = z), hlinesal, lty = 5) + scale_x_reverse(limits = c(500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Salinity (PSU)', limits = c(33, 35), breaks = seq(from = 33, to = 35, by = 1)) + coord_flip() + ggtitle("ETNP Salinity 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#if making a pdf end with this code (comment and uncomment as needed...)
# dev.off()

