#LOAD PAGACKES: reshape2, ggplot2, plyr
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)


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


#interpolating
stdDepths2 <- c(seq(from = 0, to = 1250, by = 10))



#lets try to parse out the data and only look at cruise tgt001
# note for jacob:
## tgt66n == ctdamot 
## interp == ctdamotInt
## tgt66hourVec_AB == hourVec
## tgt66interpDepTime == ctdamotInt2

tgt66 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TGT66',]
tgt66n <- na.omit(tgt66)


tgt66n$datetime <- as.POSIXct(as.character(tgt66n$datetime, format = c('%Y-%m-%d %H:%M')))
str(tgt66n)

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths2, method = 'linear')
    data.frame(DepthBin = listResult$x, value = listResult$y)
  }
}

tgt66interp <- ddply(.data = tgt66n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

str(tgt66interp) #checking its in POSIXct format


tgt66hourVec_AB <- seq(min(tgt66interp$datetime) - 3*60*60, max(tgt66interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = tgt66hourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

tgt66interpDepTime <- ddply(.data = tgt66interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)



#heatmap of no2
# the bubble plots, downscript of here, appear to be a better way of doing this
# This type of heatmap is a bit undersampled and gives boxy artifacts
samplesNO2 <- unique(subset(tgt66n, variable == 'NO2', select = c(depth, datetime)))
head(samplesNO2)
head(tgt66interpDepTime)

ggplot(subset(tgt66interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + geom_tile(aes(fill = log10(value + 10))) + scale_y_reverse(limits = c(500, 0)) + scale_x_time(limits = c(min(subset(tgt66n, variable == 'NO2')$datetime - 60*60), max(subset(tgt66n, variable == 'NO2')$datetime + 60*60))) + geom_point(data = samplesNO2, aes(x = datetime, y = depth))


#YAY!!!!
# skipping the code to look at solar angle... (or should i add this in later?)

t0_AB <- min(subset(tgt66n, variable == 'NO2')$datetime) 
t1_AB <- max(subset(tgt66n, variable == 'NO2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(tgt66n, variable == 'NO2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(tgt66n, variable == 'O2', select = c(depth, datetime)))





tempvals <- tgt66n[tgt66n$variable == 'temp',]
(tempvals2 <- unique(sort(tempvals$value)))

denvals <- tgt66n[tgt66n$variable == 'density',]
head(denvals)

head(tgt66interpDepTime)
head(tgt66n)


oxyno2P <- ggplot(subset(tgt66interpDepTime, variable == 'temp'), aes(datetime, DepthBin)) + #look in tgt66interpDepTime$temp and plot x = datetime, y = DepthBin this will give you a heat map of temp
  geom_tile(aes(fill = value)) + #this code fills temp in with (blue toned) color.. ### got rid of log10(value) since i want non log 10'd values
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + #this code is for the x axis depth ticks
  scale_x_datetime(limits = c(t0_AB, t1_AB), #this code is for the y axis date ticks
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + #tgt66n is a data frame of depth, datetime, and other things i think this plots small vertical dots wherever there was a CTD cast... 
  
  ## this might be a source of error because my time and date aren't separated like in jacob's plot...
  
  scale_fill_gradientn(colours = c("white", "yellow", "orange", "tomato", "red"), values = rescale(c(4, 10, 16,  22, 28)), name = "Temperature (deg C)") + #this code makes the colors for the heat map of temp it decides what values belong to what color... then we name the key "temp (C)"
  geom_contour(data = subset(tgt66interpDepTime, variable == 'density'), aes(x = datetime, y = DepthBin, z = (value), colour = ..level..)) + #this code is adding countour lines of density. plotting x, y, and z.  colour = ..level.. will give me blue tones, but i can change that to whatever color i want! 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'oxygen'), aes(x = datetime, y = DepthBin, z = value), size = 2, colour = 'gray20') + #this will give me thick oxygen contour lines. plotting x, y, and z. breaks = 1, 3 means ??????, size = 2 is the thickness of the line, and colour is the british version of color
  
  ##this might be a source of error because i didn't really change anything, also confused about the breaks.. we'll see
  
  geom_point(data = subset(tgt66n, variable == 'NO2'), aes(datetime, depth), shape = 1) + #here tgt66n is used instead of interpolated data probably because we don't need to plot the interpolated data? that was only for making the heat map/contours... here we are plotting x = DepthBin, y = datetime, and shape = 1 is an OPEN CIRCLE
  geom_point(data = subset(tgt66n, variable == 'NO2'), aes(datetime, depth, size = log10(value))) + #this adds extra circles to the vertical CTD cast circles. the size of the circle varies depending on the log10 value of the [NO2]
  labs(size = "log10([NO2])", colour = "Density",
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') + #this added to the key to have circles and log10 values... so i think it's plotting a label for the dots (categorized by size) as "log10[NO2]" and a label for the heat map (categorized by colors) as "Density"
  theme(axis.text.x = element_text(angle = 0, size = 16), #and this just gives some edits to the font size and stuff
        axis.text.y = element_text(size = 16),
        text = element_text(size = 20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno2P




#
#


(lats <- unique(tgt66n$lat))
(longs <- unique(tgt66n$long))
(lats[1])

#taken from "clean data.R"
#tgt66 top ODZ = 175, bottom ODZ = 800
tgt66hlineO2 <- data.frame(d = c(175, 800)) 

oxyno2P2 <- ggplot(subset(tgt66interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%m/%d %H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) + 
  # geom_contour(data = subset(tgt66interpDepTime, variable == 'temp'), aes(x = datetime, y = DepthBin, z = value), breaks = seq(3, 27, by = 6), colour = 'red', linetype = 'dotted', size = 1) +
  # geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value)), breaks = seq(0, 3, by = 1), colour = 'gray20', size = 1.5, linetype = 'longdash') +
  geom_hline(aes(yintercept = d), tgt66hlineO2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') +
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0),
        plot.margin = unit(c(0, 0, 0, 0) , "in"))


oxyno2P2


library(directlabels)
(oxyno2p2c <- direct.label(oxyno2P2, list(colour = 'dodgerblue4', 'top.bumptwice', cex = 1.2)))
(oxyno2p2c <- direct.label(oxyno2P2, list(colour='black', 'angled.boxes', cex = 1)))
(oxyno2p2c <- direct.label(oxyno2P2, list(colour='black', 'last.bumpup', cex = 1)))









# okay.. here we go, lets try to do this for nitrate in 1972

oxyno3P <- ggplot(subset(tgt66interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt66hlineO2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno3P


#PHOSPHATE
oxypo4P <- ggplot(subset(tgt66interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt66hlineO2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4P



#SILICATE
oxysio4P <- ggplot(subset(tgt66interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt66hlineO2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4P



# pdf("2012 TN278 silicate", width=10, height=6)
# USE THESE DIMENSIONS!! :)







#
#
#






#TGT001 (1965)

tgt001 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TGT001',]
tgt001n <- na.omit(tgt001)
tgt001n$datetime <- as.POSIXct(as.character(tgt001n$datetime, format = c('%Y-%m-%d %H:%M')))

tgt001interp <- ddply(.data = tgt001n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

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








#
#
#





#TGT37 (1969)

tgt37 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TGT37',]
tgt37n <- na.omit(tgt37)

tgt37n$datetime <- as.POSIXct(as.character(tgt37n$datetime, format = c('%Y-%m-%d %H:%M')))

tgt37interp <- ddply(.data = tgt37n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

tgt37hourVec_AB <- seq(min(tgt37interp$datetime) - 3*60*60, max(tgt37interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = tgt37hourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

tgt37interpDepTime <- ddply(.data = tgt37interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

t0_AB <- min(subset(tgt37n, variable == 'O2')$datetime) 
t1_AB <- max(subset(tgt37n, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(tgt37n, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(tgt37n, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#tgt37 top ODZ = 125, bottom ODZ = 950
tgt37hlineo2 <- data.frame(d = c(125, 950)) 

(lats <- unique(tgt37n$lat))
(longs <- unique(tgt37n$long))



#NITRATE
oxyno3Ptgt37 <- ggplot(subset(tgt37interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt37n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(tgt37interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt37hlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT37 Cruise May 16 to May 19 1969', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Ptgt37



#PHOSPHATE
oxypo4Ptgt37 <- ggplot(subset(tgt37interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt37n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(tgt37interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt37hlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT37 Cruise May 16 to May 19 1969', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Ptgt37


#SILICATE
oxysio4Ptgt37 <- ggplot(subset(tgt37interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt37n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(tgt37interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tgt37hlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT37 Cruise May 16 to May 19 1969', subtitle = '(16.45˚N, -101.75˚W) to (23.02˚N, -110.82˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Ptgt37








#
#
#








#P18N (1994)

p18n <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'P18N_31DSCG94_3',]
p18nn <- na.omit(p18n)

p18nn$datetime <- as.POSIXct(as.character(p18nn$datetime, format = c('%Y-%m-%d %H:%M')))

p18ninterp <- ddply(.data = p18nn, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

p18nhourVec_AB <- seq(min(p18ninterp$datetime) - 3*60*60, max(p18ninterp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = p18nhourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

p18ninterpDepTime <- ddply(.data = p18ninterp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

t0_AB <- min(subset(p18nn, variable == 'O2')$datetime) 
t1_AB <- max(subset(p18nn, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(p18nn, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(p18nn, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#p18n top ODZ = 150, bottom ODZ = 950
p18nhlineo2 <- data.frame(d = c(150, 950)) 

(lats <- unique(p18nn$lat))
(longs <- unique(p18nn$long))



#NITRATE
oxyno3Pp18n <- ggplot(subset(p18ninterpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = p18nn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(p18ninterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), p18nhlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP p18n Cruise April 21 to April 25 1994', subtitle = '(14˚N, 250˚W) to (22.85˚N, 250˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Pp18n



#PHOSPHATE
oxypo4Pp18n <- ggplot(subset(p18ninterpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = p18nn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(p18ninterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), p18nhlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP p18n Cruise April 21 to April 25 1994', subtitle = '(14˚N, 250˚W) to (22.85˚N, 250˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Pp18n


#SILICATE
oxysio4Pp18n <- ggplot(subset(p18ninterpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = p18nn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(p18ninterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), p18nhlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP p18n Cruise April 21 to April 25 1994', subtitle = '(14˚N, 250˚W) to (22.85˚N, 250˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Pp18n




#NITRITE
oxyno2Pp18n <- ggplot(subset(p18ninterpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = p18nn, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(p18ninterpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), p18nhlineo2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP p18n Cruise April 21 to April 25 1994', subtitle = '(14˚N, 250˚W) to (22.85˚N, 250˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno2Pp18n








#
#
#









#clivar (2007)

clivar <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'clivar',]
clivarn <- na.omit(clivar)

clivarn$datetime <- as.POSIXct(as.character(clivarn$datetime, format = c('%Y-%m-%d %H:%M')))

clivarinterp <- ddply(.data = clivarn, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

clivarhourVec_AB <- seq(min(clivarinterp$datetime) - 3*60*60, max(clivarinterp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = clivarhourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

clivarinterpDepTime <- ddply(.data = clivarinterp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

head(clivarinterpDepTime)

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









#
#
#









#tn278 (2012)

tn278 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TN278',]
tn278n <- na.omit(tn278)

tn278n$datetime <- as.POSIXct(as.character(tn278n$datetime, format = c('%Y-%m-%d %H:%M')))

tn278interp <- ddply(.data = tn278n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

tn278hourVec_AB <- seq(min(tn278interp$datetime) - 3*60*60, max(tn278interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = tn278hourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

tn278interpDepTime <- ddply(.data = tn278interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

t0_AB <- min(subset(tn278n, variable == 'O2')$datetime) 
t1_AB <- max(subset(tn278n, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(tn278n, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(tn278n, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#clivartn278 top ODZ = 150, bottom ODZ = 1000
tn278hlineo2 <- data.frame(d = c(150, 1000)) 

(lats <- unique(tn278n$lat))
(longs <- unique(tn278n$long))



#NITRATE
oxyno3Ptn278 <- ggplot(subset(tn278interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tn278n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(tn278interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tn278hlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TN278 Cruise March 17 to April 20 2012', subtitle = '(32.61˚N, -117.48˚W) to (22.81˚N, -111.80˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Ptn278 



#NITRITE
oxyno2Ptn278 <- ggplot(subset(tn278interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tn278n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(tn278interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tn278hlineo2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TN278 Cruise March 17 to April 20 2012', subtitle = '(32.61˚N, -117.48˚W) to (22.81˚N, -111.80˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno2Ptn278 




#AMMONIUM
oxynh4Ptn278  <- ggplot(subset(tn278interpDepTime, variable == 'NH4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tn278n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NH4] (umol/kg)") + 
  geom_contour(data = subset(tn278interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tn278hlineo2, lty = 5) +
  labs(size = '[NH4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TN278 Cruise March 17 to April 20 2012', subtitle = '(32.61˚N, -117.48˚W) to (22.81˚N, -111.80˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxynh4Ptn278 





#PHOSPHATE
oxypo4Ptn278  <- ggplot(subset(tn278interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tn278n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(tn278interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tn278hlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TN278 Cruise March 17 to April 20 2012', subtitle = '(32.61˚N, -117.48˚W) to (22.81˚N, -111.80˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Ptn278 


#SILICATE
oxysio4Ptn278  <- ggplot(subset(tn278interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tn278n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(tn278interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), tn278hlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TN278 Cruise March 17 to April 20 2012', subtitle = '(32.61˚N, -117.48˚W) to (22.81˚N, -111.80˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Ptn278 











#
#
#









#skq (2016/2017)

skq16 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'HOMZ16',]
skq16n <- na.omit(skq16)

skq16n$datetime <- as.POSIXct(as.character(skq16n$datetime, format = c('%Y-%m-%d %H:%M')))
str(skq16n)

skq16interp <- ddply(.data = skq16n, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

skqhourVec_AB <- seq(min(skq16interp$datetime) - 3*60*60, max(skq16interp$datetime) + 3*60*60, by = 'hour')

approxTime_AB <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$datetime, df$value), xout = skqhourVec_AB, method = 'linear', rule = 2)
    data.frame(datetime = listResult$x, value = listResult$y)
  }
} 

skq16interpDepTime <- ddply(.data = skq16interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

t0_AB <- min(subset(skq16n, variable == 'O2')$datetime) 
t1_AB <- max(subset(skq16n, variable == 'O2')$datetime)
t0_AB <- as.POSIXct(round(min(subset(skq16n, variable == 'O2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(skq16n, variable == 'O2', select = c(depth, datetime)))







#
#


#taken from "clean data.R"
#clivartn278 top ODZ = 85, bottom ODZ = 1050
skqhlineo2 <- data.frame(d = c(85, 1050)) 

(lats <- unique(skq16n$lat))
(longs <- unique(skq16n$long))



#NITRATE
oxyno3Pskq16 <- ggplot(subset(skq16interpDepTime, variable == 'NO3'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = skq16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO3] (umol/kg)") + 
  geom_contour(data = subset(skq16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), skqhlineo2, lty = 5) +
  labs(size = '[NO3] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP SKQ Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))


oxyno3Pskq16 



#NITRITE
oxyno2Pskq16 <- ggplot(subset(skq16interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = skq16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(skq16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), skqhlineo2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP SKQ Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxyno2Pskq16 




#AMMONIUM
oxynh4Pskq16  <- ggplot(subset(skq16interpDepTime, variable == 'NH4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = skq16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NH4] (umol/kg)") + 
  geom_contour(data = subset(skq16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), skqhlineo2, lty = 5) +
  labs(size = '[NH4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP SKQ Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxynh4Pskq16 





#PHOSPHATE
oxypo4Pskq16  <- ggplot(subset(skq16interpDepTime, variable == 'PO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = skq16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[PO4] (umol/kg)") + 
  geom_contour(data = subset(skq16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), skqhlineo2, lty = 5) +
  labs(size = '[PO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP SKQ Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxypo4Pskq16 


#SILICATE
oxysio4Pskq16  <- ggplot(subset(skq16interpDepTime, variable == 'SiO4'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = (value))) + 
  scale_y_reverse(limits = c(1100, 0), breaks = seq(from = 0, to = 1100, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = skq16n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[SiO4] (umol/kg)") + 
  geom_contour(data = subset(skq16interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) +
  geom_hline(aes(yintercept = d), skqhlineo2, lty = 5) +
  labs(size = '[SiO4] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP SKQ Cruise Dec 30 2016 to Jan 15 2017', subtitle = '(17.60˚N, -107.39˚W) to (16.34˚N, -106.58˚W)') + 
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0))

oxysio4Pskq16 





