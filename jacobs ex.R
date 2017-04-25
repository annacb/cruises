#jacob's data


# pull in data

rawdata <- read.csv("POMZ_NH4_Leg_2.csv")
ctddata <- read.csv("skq201617s-combinedctd.csv")
castdata <- read.csv("skq201617s-castinfo.csv")
# seperate samples from standards
samples = rawdata[,1:which(names(rawdata)=="bad_ii")]
# remove flagged samples
samples[samples$bad_i == "flag", "Fluo_i"] = NA;
names(samples)[names(samples)=='AM.'] = 'AM'

# process standards
pre_standards = rawdata[,which(names(rawdata)=="StConc"):dim(rawdata)[2]];
standards = pre_standards[!is.na(pre_standards[,"StConc"]) & pre_standards[,"badstd_i"]!="flag" & pre_standards[,"badstd_ii"]!="flag",1:4]
names(standards)[names(standards)=="Run.1"] = "Run"

swp2_samples = samples[samples$Type=="sw" & samples$Station=="P2",]
swp2_samples = subset(samples, Type=="sw"& Station=="P2", select = -c(AM, Type, Station, bad_i, bad_ii))

## Lets look at all of the standards
# Load in libraries for processing and plotting data
library('ggplot2')
library('reshape2')
library('gridExtra')
library('hms')
library('oce')
library('plyr')
library('cowplot')
standards.m = melt(standards, id.vars = c("StConc", "Run"))
## ggplot(standards, aes(StConc, StFluo_i)) + facet_wrap(~Run, nrow = 3) + geom_point() + xlim(c(0, 250)) + ylim(c(0, 750)) #+ scale_x_log10() + scale_y_log10()
## ggplot(standards.m, aes(StConc, value)) + facet_wrap(~Run, nrow = 3) + geom_point() + xlim(c(0, 250)) + ylim(c(0, 750)) #+ scale_x_log10() + scale_y_log10()

# plan for fitting curves
# for any samples < 62.5nM use a regression of all points up to 62.5nM
# for samples above that use the curve up through the point of next higher value than that one's fluorescence.

samples.m = melt(samples, meas = c("Fluo_i", "Fluo_ii"))

## one point example
loc.run = samples.m[636,"Run"]
loc.fluo = samples.m[636,"value"]
loc.run = samples.m[1,"Run"]
loc.fluo = samples.m[1,"value"]

loc.st = subset(standards, Run == loc.run)
loc.stsort = loc.st[order(loc.st[,"StConc"]),]
loc.meanfluo = apply(subset(loc.stsort, select = c(StFluo_i,StFluo_ii)), 1, mean)
loc.st2 = cbind(loc.stsort, meanfluo = loc.meanfluo)

# the concentration corresponding to (one higher than) the observed fluorescence
topconc_a = loc.st2$StConc[which(loc.st2$meanfluo >= loc.fluo)[1]]
# the concentration corresponding more or less to 62.5nM
topconc_62 = loc.st2$StConc[which(loc.st2$StConc >= 60)[1]]
topconc = max(topconc_a, topconc_62)


loc.st3 = subset(standards.m, Run == loc.run & StConc <= topconc)
loc.mod = lm(value ~ StConc, loc.st3)
loc.est = (loc.fluo - loc.mod$coefficients[1])/loc.mod$coefficients[2]

# convert ammonia fluorescence to concenrration for every element
# fluo2conc takes fluorescence values associated with a given sample run (set of samples incubated at the same time with one set of standards) and figures out what their concentraiton should be
fluo2conc <- function(fluo, run){
  if(is.na(fluo)){
    loc.est = NA
  }else{
    loc.fluo = fluo
    loc.run = run
    loc.st = subset(standards, Run == loc.run)
    loc.stsort = loc.st[order(loc.st[,"StConc"]),]
    loc.meanfluo = apply(subset(loc.stsort, select = c(StFluo_i,StFluo_ii)), 1, mean)
    loc.st2 = cbind(loc.stsort, meanfluo = loc.meanfluo)
    
    # the concentration corresponding to (one higher than) the observed fluorescence
    topconc_a = loc.st2$StConc[which(loc.st2$meanfluo >= loc.fluo)[1]]
    # if topconc_a is NA, its probably because meanfluo is higher than the highest standard
    if(is.na(topconc_a)){
      topconc_a = max(loc.st$StConc)
    }
    # the concentration corresponding more or less to 62.5nM
    topconc_62 = loc.st2$StConc[which(loc.st2$StConc >= 60)[1]]
    topconc = max(topconc_a, topconc_62)
    
    
    loc.st3 = subset(standards.m, Run == loc.run & StConc <= topconc)
    loc.mod = lm(value ~ StConc, loc.st3)
    loc.est = (loc.fluo - loc.mod$coefficients[1])/loc.mod$coefficients[2]
  }
  return(loc.est)
}
# we apply this function to every run
estemates <- mapply(fluo2conc, samples.m$value, samples.m$Run)
estemates <- unname(estemates)

samples.2 = data.frame(samples.m, conc = estemates)

#### Plotting
## Plots of ammonia concentraiton by cast. Here we plot both replecates for different samples. Sometimes values are < 10 or negaitve
swp2 <- subset(samples.2, Type == 'sw' & Station == 'P2', select = -c(AM, Type, Station, bad_i, bad_ii))
names(swp2)[names(swp2)=='Cast.Trap'] = 'Cast'
ggplot(swp2, aes(conc, Depth)) + facet_wrap('Cast', nrow = 3) + geom_point() + scale_y_reverse() + stat_summary(fun.y = "mean")

## Some more processing
# 1. make a new column where values less than 10nM are replaced by 0nM or 10nM
# 2. reshape the data, so the replecates are their own columns. Plot the average of those and the two actual points as the error bar.

swp2.c = dcast(swp2, Cast + Depth  ~ variable, value.var = 'conc')

meanvals = apply(subset(swp2.c, select= c('Fluo_i', 'Fluo_ii')), 1, mean, na.rm = TRUE)
minvals = apply(subset(swp2.c, select= c('Fluo_i', 'Fluo_ii')), 1, min, na.rm = TRUE)
cullsmall = function(x, xmin){
  if(is.na(x) | is.na(xmin)){
    NA} else if (xmin<10){
      0}else{
        x}
}
corvals = mapply(cullsmall, meanvals, minvals)
swp2.2 = data.frame(swp2.c, meanvals, minvals, corvals)

# this new corvals column in swp2 is an estemate of fluorescence. If either of the two fluorescence values fall below a threshold (10uM) corvals is set to zero. Otherwise, it is the mean of the two replicates

# Plot Ammonia concentratoin vs depth for each cast, with these corrections.
# Here each sample only gets one point.
ggplot(swp2.2, aes(Depth, corvals)) + facet_wrap('Cast', nrow = 3) + geom_point()  + geom_line() + coord_flip() + xlim(110,0)

### Lets bring in CTD data here

# note that P1 is cast 18-30 and P2 is cast 31-43
# This tells us if a cast is from stations P1 or P2, which are the only ones that I care about right now.
pstationator <- function(cast){
  if(cast >=  18 & cast <= 30){
    "P1"
  } else if (cast >= 31){
    "P2"
  } else {
    NA
  }
}

pstation <- sapply(castdata$cast, pstationator)
cast2 <- data.frame(cast = castdata$cast, time = as.POSIXct(castdata$startTime), latitude = castdata$latitude, longitude = castdata$longitude, pstation = pstation)

ctd.m = melt(subset(ctddata, select = -X), id = c('cast', 'depth'))

##merge the CTD and the ammonia data
nh4 <- data.frame(subset(rename(swp2.2, c("Cast" = "cast", "Depth" = "depth", "corvals" = "value")), select = c(cast, depth, value)), variable = "nh4")

ctdnh4 <- rbind(ctd.m, nh4)

ctdamot <- merge(ctdnh4, subset(cast2, select = c(cast, time)))

head(ctdamot)
str(ctdamot)


# here are each individual variable, note that flu and oxy only have values when I also sample ammonia
amo <-  subset(ctdamot, (variable == 'nh4') & cast >= 31)
flu <-  subset(ctdamot, (variable == 'fluorescence') & cast %in% unique(amo$cast))
oxy <-  subset(ctdamot, (variable == 'oxygen') & cast %in% unique(amo$cast))

grid.arrange(amoP, fluP, oxyP, ncol = 3)

# Here is a detailed look at station 37 so we have a better idea what is goin on
fluP37 <- ggplot(subset(flu, cast == 37), aes(depth, value))  + geom_point()  + geom_line() + coord_flip() + scale_x_reverse(limits = c(110, 0), breaks = seq(110, 0, by = -20), minor_breaks = seq(110, 0, by = -5))
amoP37 <- ggplot(subset(amo, cast == 37), aes(depth, value))  + geom_point()  + geom_line() + coord_flip() + scale_x_reverse(limits = c(110, 0), breaks = seq(110, 0, by = -20), minor_breaks = seq(110, 0, by = -5))
oxyP37 <-  ggplot(subset(oxy, cast == 37), aes(depth, value)) + geom_point()  + geom_line() + coord_flip() + scale_x_reverse(limits = c(110, 0), breaks = seq(110, 0, by = -20), minor_breaks = seq(110, 0, by = -5))
grid.arrange(amoP37, fluP37, oxyP37, ncol = 3)

## Expermenting with heatmaps
# Here, I interpolate the values so I have a regularly spaced matrix, which is necessary for the heatmap function

# This function interpolates one water column with respect to depth. It gives us values at every meter from 1 to 110m
approxDep <- function(df) {
  if (nrow(df) >= 2) {
    listResult <-  approx(xy.coords(df$depth, df$value), xout = 1:110, method = 'linear', rule = 2)
    data.frame(depth = listResult$x, value = listResult$y)
  }
}

# I apply that function to each cast and variable combination
ctdamotInt <- ddply(.data = ctdamot, .variables = c('cast','time', 'variable'), approxDep, .inform = TRUE)

head(ctdamotInt)

# every hour from three hours before we started at P2 untill three hours after we finished
hourVec <- seq(min(ctdamot$time)-3*60*60, max(ctdamot$time) + 3*60*60, by = "hour")

head(hourVec)

# This interpolates samples taken at different times with respect to time, so we have an estemate for each hour in hourVec
approxTime <- function(df){
  if (nrow(na.omit(df)) >= 2) {
    listResult <-  approx(xy.coords(df$time, df$value), xout = hourVec, method = 'linear', rule = 2)
    data.frame(time = listResult$x, value = listResult$y)
  }
} 

# I then interpolate each depth with respect to time over all of P2
# ctdamontInt2 then has exactly one value at every depth (1:110m) and every hour (from start to end of P2) for each variable
ctdamotInt2 <- ddply(.data = ctdamotInt, .variables = c( 'variable', 'depth'), approxTime, .inform = TRUE)


#heatmap of ammonia
# the bubble plots, downscript of here, appear to be a better way of doing this
# This type of heatmap is a bit undersampled and gives boxy artifacts
samplesNH4 <- unique(subset(ctdamot, variable == 'nh4', select = c(depth, time)))

ggplot(subset(ctdamotInt2, variable == 'nh4'), aes(time, depth)) + geom_tile(aes(fill = log10(value + 10))) + scale_y_reverse(limits = c(110, 0)) + scale_x_time(limits = c(min(subset(ctdamot, variable == 'nh4')$time - 60*60), max(subset(ctdamot, variable == 'nh4')$time + 60*60))) + geom_point(data = samplesNH4 ,aes( x = time, y = depth))




################################################################
################### TOOK OUT SOME CODE #########################
################################################################

t0 = min(subset(ctdamot, variable == 'nh4')$time) - 1 * 60^2
t1 = max(subset(ctdamot, variable == 'nh4')$time) + 1 * 60^2
t0 = as.POSIXct(round(min(subset(ctdamot, variable == 'nh4')$time) - 2 * 60^2, 'day'))
tseq = seq(from = round(t0, 'day'), to = round(t1, 'day'), by = 86400/2)

samplesCTD <- unique(subset(ctdamot, variable == 'fluorescence', select = c(depth, time)))


# Plot ammonia, fluorescence, density and oxygen together.
# Where ammonia is zero, an empty circle is desplayed
# Whre ammonia > 10uM a dot is displayed whose size corresponds with NH4 concentraiton
# Chlorophyl fluorescence (not actual concentration at this point) is displayed as a colour map.
# Isopicnals are fine lines.
# The oxycline, specifically O2 < 3uM and O2< 1uM are displayed as thick lines
# Dotted lines indicate CTD transect.

fluamstoxyP <- ggplot(subset(ctdamotInt2, variable == 'fluorescence'), aes(time, depth)) +
  geom_tile(aes(fill = log10(value))) +
  scale_y_reverse(limits = c(110, 0), breaks = seq(from = 0, to = 110, by = 20)) +
  scale_x_datetime(limits = c(t0,t1),
                   breaks = tseq,
                   labels = scales::date_format(format = '%b-%d: %H%M', tz = "US/Pacific")
  ) +
  geom_point(data = samplesCTD, aes( x = time, y = depth), shape = ".") +
  scale_fill_gradientn(colours = c("blue4", "blue1", "white", "green1", "green4"), values = rescale(c(-1, -.5, -.2 ,  .05, .5)), name = "Chlorophyll Fluorescence") +
  geom_contour(data = subset(ctdamotInt2, variable == 'sigmaT'), aes(x = time, y = depth, z = (value), colour = ..level..)) +
  geom_contour(data = subset(ctdamotInt2, variable == 'oxygen'), aes(x = time, y = depth, z = value),  breaks =  c(1, 3), size = 2, colour = 'gray20') +
  geom_point(data = subset(ctdamot, variable == 'nh4'), aes(time, depth), shape = 1) +
  geom_point(data = subset(ctdamot, variable == 'nh4'), aes(time, depth, size = log10(value))) +
  labs(size = "log10([NH4])", colour = "sigmaTheta") +
  theme(axis.text.x = element_text(angle = 0, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20))
fluamstoxyP

