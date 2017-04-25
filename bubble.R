#LOAD PAGACKES: reshape2, ggplot2, plyr
library(reshape2)
library(ggplot2)
library(plyr)


#reading in the data
cruisesDataFrame <- read.csv('allOMZcruises.csv')
cruises <- subset(cruisesDataFrame, select = -c(date, time, bottDepth))
skq2016combined <- read.csv('skq2016combined.csv')
cruises16 <- subset(skq2016combined, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(cruises, cruises16)  
skq2016KrogslundNuts <- read.csv('krogslund_nutrients.csv')
krogslund <- subset(skq2016KrogslundNuts, select = -c(date, time, bottDepth))
cruisesALL1 <- rbind(cruises, cruises16, krogslund)
cruisesALL2.v2 <- subset(cruisesALL1, select = -c(dateNew, timeNew, lat, long, press))
head(cruisesALL2.v2)

#melting data
cruisesALLm.v2 <- data.frame(melt(cruisesALL2.v2, id.vars = c('cruise', 'station', 'datetime', 'depth')))


#interpolating
#WOA standard depths
by5 <- seq(0, 100, 5)
by25 <- seq(125, 500, 25)
by50 <- seq(550, 2000, 50)
by100 <- seq(2100, 9000, 100)
stdDepths <- c(by5, by25, by50, by100)

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
## tgtn == ctdamot 
## interp == ctdamotInt
## tgt66hourVec_AB == hourVec
## tgt66interpDepTime == ctdamotInt2

tgt66 <- cruisesALLm.v2[cruisesALLm.v2$cruise == 'TGT66',]
tgt66n <- na.omit(tgt66)


tgt66n$datetime <- as.POSIXct(as.character(tgt66n$datetime, format = c('%Y-%m-%d %H:%M')))
str(tgt66n)

approxR <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = stdDepths, method = 'linear')
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

t0_AB <- min(subset(tgt66n, variable == 'NO2')$datetime) - 1 * 60^2
t1_AB <- max(subset(tgt66n, variable == 'NO2')$datetime) + 1 * 60^2
t0_AB <- as.POSIXct(round(min(subset(tgt66n, variable == 'NO2')$datetime) - 2 * 60^2, 'day'))
tseq_AB <- seq(from = round(t0_AB, 'day'), to = round(t1_AB, 'day'), by = 86400/2)

samplesCTD_AB <- unique(subset(tgt66n, variable == 'O2', select = c(depth, datetime)))

oxyno2P <- ggplot(subset(tgt66interpDepTime, variable == 'O2'), aes(datetime, DepthBin)) +
  geom_tile(aes(fill = log10(value))) +
  scale_y_reverse(limits = c(500, 0), breaks = seq(from = 0, to = 500, by = 50), name = 'Depth (m)') +
  scale_x_datetime(limits = c(t0_AB ,t1_AB),
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%H:%M', tz = "US/Pacific"),
                   name = 'Feb 23 to 28 1972 (hour)') +
  geom_contour(data = subset(tgt66interpDepTime, variable == 'temp'), aes(x = datetime, y = DepthBin, z = (value), colour = ..level..)) +
  geom_contour(data = subset(tgt66interpDepTime, variable == 'oxygen'), aes(x = datetime, y = DepthBin, z = value),  breaks =  c(0, 3), size = 2, colour = 'gray20') +
  geom_point(data = subset(tgt66n, variable == 'NO2'), aes(datetime, depth), shape = 1) +
  geom_point(data = subset(tgt66n, variable == 'NO2'), aes(datetime, depth, size = log10(value))) +
  labs(size = "log10([NO2])", colour = "temp") +
  theme(axis.text.x = element_text(angle = 0, size = 13),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20))

oxyno2P

