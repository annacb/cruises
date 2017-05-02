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



#TGT001 (1965)

tgt001 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TGT001',]
tgt001n <- na.omit(tgt001)


tgt001n$datetime <- as.POSIXct(as.character(tgt001n$datetime, format = c('%Y-%m-%d %H:%M')))

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths2, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}

tgt001interp <- ddply(.data = tgt001n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

str(tgt001interp) #checking its in POSIXct format


tgt001hourVec_AB <- seq(min(tgt001interp$datetime) - 3*60*60, max(tgt001interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = tgt001hourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

tgt001interpDepTime <- ddply(.data = tgt001interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)


t0_AB <- min(subset(tgt001n, variable == 'O2')$datetime) 
t1_AB <- max(subset(tgt001n, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(tgt001n, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(tgt001n, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#tgt001 top ODZ = 200, bottom ODZ = 850
tgt001hlineO2 <- data.frame(d = c(200, 850)) 

(lats <- unique(tgt001n$lat))
(longs <- unique(tgt001n$long))



#NITRATE
oxyno3Ptgt001 <- ggplot(subset(tgt001interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt001n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(tgt001interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt001hlineO2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT001 Cruise Dec 3 to Dec 6 1965', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Ptgt001


#PHOSPHATE
oxypo4Ptgt001 <- ggplot(subset(tgt001interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt001n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(tgt001interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt001hlineO2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT001 Cruise Dec 3 to Dec 6 1965', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Ptgt001



#SILICATE
oxysio4Ptgt001 <- ggplot(subset(tgt001interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt001n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(tgt001interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt001hlineO2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT001 Cruise Dec 3 to Dec 6 1965', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Ptgt001

