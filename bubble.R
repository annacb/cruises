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
by5 <- seq(0, 100, 5)
by25 <- seq(125, 500, 25)
by50 <- seq(550, 2000, 50)
by100 <- seq(2100, 9000, 100)
stdDepths <- c(by5, by25, by50, by100)

stdDepths2 <- c(seq(from = 0, to = 1250, by = 10))


# TO JACOB: maybe my problem is here? because stdDepths aren't evenly spaced?



################################################################
################################################################
# this is all old! from when i tried to plot ALL the seven cruise data...

# cruisesALLn.v2 <- na.omit(cruisesALLm.v2)
# head(cruisesALLn.v2)
#

# approxR <- function(df) {
  # if (nrow(df) >= 2) {
    # listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths, method = 'linear')
    # data.frame(DepthBin = listResult$x, value = listResult$y)
  # }
# }

# interp <- ddply(.data = cruisesALLn.v2, .variables = c('cruise', 'station', 'datetime', 'variable'), function(df) approxR(df), .inform = TRUE)

# str(interp)
# interp$datetime <- as.POSIXct(as.character(interp$datetime, format = c('%Y-%m-%d %H:%M')))
# str(interp)

###################### cruisesALLn.v2 == ctdamot in jacob's ex #################### 
###################### interp == ctdamotInt in jacob's ex ######################### 
###################### except used stdDepths not 1:110 ############################

#also need to interpolate by TIME
# hourVec_AB <- seq(min(interp$datetime) - 3*60*60, max(interp$datetime) + 3*60*60, by = 'hour')

# head(hourVec_AB)

###################### YAYYY hourVec and hourVec_AB are the same!!! ######################
##########################################################################################
#################################### so far so good... ###################################


# approxTime_AB <- function(df){
  # if (nrow(na.omit(df)) >= 2) {
    # listResult <-  approx(xy.coords(df$datetime, df$value), xout = hourVec_AB, method = 'linear', rule = 2)
    # data.frame(datetime = listResult$x, value = listResult$y)
  # }
# } 

# interpolating each stdDepth with respect to time
# interpDepTime has exactly one value at each stdDepth and each hour from start to end for each variable

# interpDepTime <- ddply(.data = interp, .variables = c( 'variable', 'DepthBin'), approxTime_AB, .inform = TRUE)

#NOT WORKING...... FATAL ERROR!!!




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


#################### it works!!
#################### tgt66interpDepTime == ctdamotInt2 in jacob's ex #################### 


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


no2vals <- tgt66n[tgt66n$variable == 'NO2',]
(no2vals2 <- unique(sort(no2vals$value)))
(logno2vals2 <- unique(sort(log10(no2vals$value))))

(lats <- unique(tgt66n$lat))
(longs <- unique(tgt66n$long))

tempvals <- tgt66interpDepTime[tgt66interpDepTime$variable == 'temp',]
(tempvals2 <- unique(sort(tempvals$value)))
max(tempvals2)


o2vals <- tgt66interpDepTime[tgt66interpDepTime$variable == 'O2',]
(o2vals2 <- unique(sort(o2vals$value)))
(logo2vals2 <- unique(sort(log10(o2vals$value))))


denvals <- tgt66interpDepTime[tgt66interpDepTime$variable == 'density',]
denvals2 <- unique(sort(denvals$value))
max(denvals2)

#taken from "clean data.R"
#tgt66 top ODZ = 175, bottom ODZ = 800
tgt66hlineO2 <- data.frame(d = c(175, 800)) 


oxyno2P2 <- ggplot(subset(tgt66interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = 'US/Pacific'),
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
        plot.title = element_text(size = 25, hjust = 0))


oxyno2P2


# library(directlabels)
# (oxyno2p2c <- direct.label(oxyno2P2, list(colour='black', 'angled.endpoints')))










# okay.. here we go, lets try to do this for nitrate in 1972

no3vals <- tgt66interpDepTime[tgt66interpDepTime$variable == 'NO3',]
no3vals2 <- unique(sort(no3vals$value))
min(no3vals2)


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