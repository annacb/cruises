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



#homz16 (2016/2017)

homz16 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'HOMZ16',]
homz16n <- na.omit(homz16)


homz16n$datetime <- as.POSIXct(as.character(homz16n$datetime, format = c('%Y-%m-%d %H:%M')))

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths2, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}

homz16interp <- ddply(.data = homz16n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

str(homz16interp) #checking its in POSIXct format


homz16hourVec_AB <- seq(min(homz16interp$datetime) - 3*60*60, max(homz16interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = homz16hourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

homz16interpDepTime <- ddply(.data = homz16interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)


t0_AB <- min(subset(homz16n, variable == 'O2')$datetime) 
t1_AB <- max(subset(homz16n, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(homz16n, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(homz16n, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#clivartn278 top ODZ = 85, bottom ODZ = 1050
homz16hlineo2 <- data.frame(d = c(85, 1050)) 

(lats <- unique(homz16n$lat))
(longs <- unique(homz16n$long))



#NITRATE
oxyno3Phomz16 <- ggplot(subset(homz16interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = homz16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(homz16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), homz16hlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP HOMZ16 Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Phomz16 



#NITRITE
oxyno2Phomz16 <- ggplot(subset(homz16interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = homz16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(homz16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), homz16hlineo2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP HOMZ16 Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno2Phomz16 




#AMMONIUM
oxynh4Phomz16  <- ggplot(subset(homz16interpDepTime, variable == 'NH4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = homz16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NH4] (umol/kg)") + 
  geom_contour(data = subset(homz16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), homz16hlineo2, lty = 5) +
  labs(size = '[NH4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP HOMZ16 Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxynh4Phomz16 





#PHOSPHATE
oxypo4Phomz16  <- ggplot(subset(homz16interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = homz16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(homz16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), homz16hlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP HOMZ16 Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Phomz16 


#SILICATE
oxysio4Phomz16  <- ggplot(subset(homz16interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = homz16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(homz16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), homz16hlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP HOMZ16 Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Phomz16 




