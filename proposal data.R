cruise <- read.csv(file = "TN278_CTD_Bottle.csv", header = T)

depths <- sort(unique(cruise$Bot..Depth..m.))
graphDepths <- depths * -1
ndepth <- length(depths)

#right now i am sorting by BOTTOM depth and subsetting all the nutrients by bottom depth and taking the average of measurements from a bottom depth
#but what I want to be doing is sorting it by PRESSURE and then subsetting each nutrient by their pressure and average


#O2
O2levels <- NULL
for(i in 1:ndepth) {
  O2s <- cruise[cruise$Bot..Depth..m. == depths[i],]$OXYGEN..umol.l.
  O2levels[i] <- mean(O2s, na.rm = T)
}
O2levels

O2data <- data.frame(graphDepths, O2levels)
O2data
plot(O2data$O2levels, O2data$graphDepths, xlim = c(0, 1000))

#O2 sal
O2levels <- NULL
for(i in 1:ndepthSal) {
  O2s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$OXYGEN..umol.l.
  O2levels[i] <- mean(O2s)
}
O2levels

O2data <- data.frame(graphDepthsSal, O2levels)
O2data
plot(O2data$O2levels, O2data$graphDepthsSal, xlim = c(0, 1000))

#NO3
NO3levels <- NULL
for(i in 1:ndepth) {
  NO3s <- cruise[cruise$Bot..Depth..m. == depths[i],]$NITRATE..umol.L.
  NO3levels[i] <- mean(NO3s, na.rm = T)
}
NO3levels

NO3data <- data.frame(graphDepths, NO3levels)
plot(NO3data$NO3levels, NO3data$graphDepths, xlim = c(0, 100))

#NO2
NO2levels <- NULL
for(i in 1:ndepth) {
  NO2s <- cruise[cruise$Bot..Depth..m. == depths[i],]$NITRITE..umol.L.
  NO2levels[i] <- mean(NO2s, na.rm = T)
}
NO2levels

NO2data <- data.frame(graphDepths, NO2levels)
plot(NO2data$NO2levels, NO2data$graphDepths, xlim = c(0, 10))

#PO4
PO4levels <- NULL
for(i in 1:ndepth) {
  PO4s <- cruise[cruise$Bot..Depth..m. == depths[i],]$PHOSPHATE..umol.L.
  PO4levels[i] <- mean(PO4s, na.rm = T)
}
PO4levels

PO4data <- data.frame(graphDepths, PO4levels)
plot(PO4data$PO4levels, PO4data$graphDepths, xlim = c(0, 10))

#NH4
NH4levels <- NULL
for(i in 1:ndepth) {
  NH4s <- cruise[cruise$Bot..Depth..m. == depths[i],]$AMMONIUM..umol.L.
  NH4levels[i] <- mean(NH4s, na.rm = T)
}
NH4levels

NH4data <- data.frame(graphDepths, NH4levels)
plot(NH4data$NH4levels, NH4data$graphDepths, xlim = c(0, 0.5))

#SiO4
SiO4levels <- NULL
for(i in 1:ndepth) {
  SiO4s <- cruise[cruise$Bot..Depth..m. == depths[i],]$SILICATE..umol.L.
  SiO4levels[i] <- mean(SiO4s, na.rm = T)
}
SiO4levels

SiO4data <- data.frame(graphDepths, SiO4levels)
plot(SiO4data$SiO4levels, SiO4data$graphDepths, xlim = c(0, 150))

##


#par
par(mfcol = c(1,7))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2data$O2levels, O2data$graphDepths, xlim = c(0, 300), yaxs="i", col=1, pch=19, cex=0.2)
points(O2data$O2levels, O2data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="Depth (m)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] umol", side=1, line=2.9, cex = 1.2)

plot(NO3data$NO3levels, NO3data$graphDepths, xlim = c(0, 50), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
points(NO3data$NO3levels, NO3data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NO3] umol", side=1, line=2.9, cex = 1.2)

plot(NO2data$NO2levels, NO2data$graphDepths, xlim = c(0, 10), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
points(NO2data$NO2levels, NO2data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NO2] umol", side=1, line=2.9, cex = 1.2)

plot(NH4data$NH4levels, NH4data$graphDepths, xlim = c(0, 2.5), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
points(NH4data$NH4levels, NH4data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NH4] umol", side=1, line=2.9, cex = 1.2)

plot(SiO4data$SiO4levels, SiO4data$graphDepths, xlim = c(0, 100), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
points(SiO4data$SiO4levels, SiO4data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[SiO4] umol", side=1, line=2.9, cex = 1.2)

plot(PO4data$PO4levels, PO4data$graphDepths, xlim = c(0, 4), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
points(PO4data$PO4levels, PO4data$graphDepths, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[PO4] umol", side=1, line=2.9, cex = 1.2)

title("Element Depth Profiles", outer=TRUE, cex.main = 2.5, line = 1.5)

graphics.off()






newO2levels <- c(mean(O2data[O2data$graphDepths < 0 & O2data$graphDepths >= -200.0,]$O2levels), mean(O2data[O2data$graphDepths < -200.0 & O2data$graphDepths >= -400.0,]$O2levels), mean(O2data[O2data$graphDepths < -400.0 & O2data$graphDepths >= -600.0,]$O2levels), mean(O2data[O2data$graphDepths < -600.0 & O2data$graphDepths >= -800.0,]$O2levels), mean(O2data[O2data$graphDepths < -800.0 & O2data$graphDepths >= -1000.0,]$O2levels), mean(O2data[O2data$graphDepths < -1000.0 & O2data$graphDepths >= -1200.0,]$O2levels), mean(O2data[O2data$graphDepths < -1200.0 & O2data$graphDepths >= -1400.0,]$O2levels), mean(O2data[O2data$graphDepths < -1400.0 & O2data$graphDepths >= -1600.0,]$O2levels), mean(O2data[O2data$graphDepths < -1600.0 & O2data$graphDepths >= -1800.0,]$O2levels), mean(O2data[O2data$graphDepths < -1800.0 & O2data$graphDepths >= -2000.0,]$O2levels), mean(O2data[O2data$graphDepths < -2000.0 & O2data$graphDepths >= -2200.0,]$O2levels), mean(O2data[O2data$graphDepths < -2200.0 & O2data$graphDepths >= -2400.0,]$O2levels), mean(O2data[O2data$graphDepths < -2400.0 & O2data$graphDepths >= -2600.0,]$O2levels), mean(O2data[O2data$graphDepths < -2600.0 & O2data$graphDepths >= -2800.0,]$O2levels), mean(O2data[O2data$graphDepths < -2800.0 & O2data$graphDepths >= -3000.0,]$O2levels), mean(O2data[O2data$graphDepths < -3000.0 & O2data$graphDepths >= -3200.0,]$O2levels), mean(O2data[O2data$graphDepths < -3200.0 & O2data$graphDepths >= -3400.0,]$O2levels), mean(O2data[O2data$graphDepths < -3400.0 & O2data$graphDepths >= -3600.0,]$O2levels), mean(O2data[O2data$graphDepths < -3600.0 & O2data$graphDepths >= -3800.0,]$O2levels), mean(O2data[O2data$graphDepths < -3800.0 & O2data$graphDepths >= -4000.0,]$O2levels), mean(O2data[O2data$graphDepths < -4000.0 & O2data$graphDepths >= -4200.0,]$O2levels), mean(O2data[O2data$graphDepths < -4200.0 & O2data$graphDepths >= -4400.0,]$O2levels), mean(O2data[O2data$graphDepths < -4400.0 & O2data$graphDepths >= -4600.0,]$O2levels))

newdepths <- seq(from = -100, to = -4500, by = -200)
newO2data <- data.frame(newdepths, newO2levels)
plot(newO2data$newO2levels, newO2data$newdepths, xlim = c(0,500))
