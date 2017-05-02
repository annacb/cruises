#LOAD PAGACKES: reshape2, ggplot2, plyr
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)


#reading in the data
cruisesDataFrame <- read.csv('allOMZcruises.csv')
cruises <- subset(cruisesDataFrame, select = -c(date, time, bottDepth))
skq2016combined <- read.csv('skq2016combined.csv')
cruises16 <- subset(skq2016combined, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(cruises, cruises16)  
skq2016KrogslundNuts <- read.csv('krogslund_nutrients.csv')
krogslund <- subset(skq2016KrogslundNuts, select = -c(date, time, bottDepth))
cruisesALL1 <- rbind(cruises, cruises16, krogslund)
cruisesALL2.v2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, press))
head(cruisesALL2.v2)

#melting data
cruisesALLm.v2 <- data.frame(melt(cruisesALL2.v2, id.vars = c('cruise', 'station', 'lat', 'long', 'datetime', 'depth')))


#interpolating
#WOA standard depths
stdDepths2 <- c(seq(from = 0, to = 1250, by = 10))



#clivar (2007)

clivar <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'clivar',]
clivarn <- na.omit(clivar)


clivarn$datetime <- as.POSIXct(as.character(clivarn$datetime, format = c('%Y-%m-%d %H:%M')))

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths2, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}

clivarinterp <- ddply(.data = clivarn, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

str(clivarinterp) #checking its in POSIXct format


clivarhourVec_AB <- seq(min(clivarinterp$datetime) - 3*60*60, max(clivarinterp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = clivarhourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

clivarinterpDepTime <- ddply(.data = clivarinterp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)


t0_AB <- min(subset(clivarn, variable == 'O2')$datetime) 
t1_AB <- max(subset(clivarn, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(clivarn, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(clivarn, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#clivar top ODZ = 150, bottom ODZ = 900
clivarhlineo2 <- data.frame(d = c(150, 900)) 

(lats <- unique(clivarn$lat))
(longs <- unique(clivarn$long))



#NITRATE
oxyno3Pclivar <- ggplot(subset(clivarinterpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = clivarn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(clivarinterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), clivarhlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP clivar Cruise Dec 17 to Dec 22 2007', subtitle = '(22.87˚N, -110˚W) to (13.833˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Pclivar



#PHOSPHATE
oxypo4Pclivar <- ggplot(subset(clivarinterpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = clivarn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(clivarinterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), clivarhlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP clivar Cruise Dec 17 to Dec 22 2007', subtitle = '(22.87˚N, -110˚W) to (13.833˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Pclivar


#SILICATE

oxysio4Pclivar <- ggplot(subset(clivarinterpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = clivarn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(clivarinterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), clivarhlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP clivar Cruise Dec 17 to Dec 22 2007', subtitle = '(22.87˚N, -110˚W) to (13.833˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Pclivar




#NITRITE
oxyno2Pclivar <- ggplot(subset(clivarinterpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = clivarn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(clivarinterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), clivarhlineo2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP clivar Cruise Dec 17 to Dec 22 2007', subtitle = '(22.87˚N, -110˚W) to (13.833˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno2Pclivar
