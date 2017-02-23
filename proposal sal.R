cruise <- read.csv(file = "TN278_CTD_Bottle.csv", header = T)


salDepths <- sort(unique(cruise$SALINITY..PSU.))
ndepthSal <- length(salDepths)


#O2 sal
O2levels <- NULL
for(i in 1:ndepthSal) {
  O2s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$OXYGEN..umol.l.
  O2levels[i] <- mean(O2s)
}
O2levels

O2data <- data.frame(salDepths, O2levels)
plot(O2data$O2levels, O2data$salDepths, xlim = c(0, 1000))

#NO3 sal
NO3levels <- NULL
for(i in 1:ndepthSal) {
  NO3s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$NITRATE..umol.L.
  NO3levels[i] <- mean(NO3s, na.rm = T)
}
NO3levels

NO3data <- data.frame(salDepths, NO3levels)
plot(NO3data$NO3levels, NO3data$salDepths, xlim = c(0, 100))

#NO2
NO2levels <- NULL
for(i in 1:ndepthSal) {
  NO2s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$NITRITE..umol.L.
  NO2levels[i] <- mean(NO2s, na.rm = T)
}
NO2levels

NO2data <- data.frame(salDepths, NO2levels)
plot(NO2data$NO2levels, NO2data$salDepths, xlim = c(0, 10))

#PO4
PO4levels <- NULL
for(i in 1:ndepthSal) {
  PO4s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$PHOSPHATE..umol.L.
  PO4levels[i] <- mean(PO4s, na.rm = T)
}
PO4levels

PO4data <- data.frame(salDepths, PO4levels)
plot(PO4data$PO4levels, PO4data$salDepths, xlim = c(0, 10))

#NH4
NH4levels <- NULL
for(i in 1:ndepthSal) {
  NH4s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$AMMONIUM..umol.L.
  NH4levels[i] <- mean(NH4s, na.rm = T)
}
NH4levels

NH4data <- data.frame(salDepths, NH4levels)
plot(NH4data$NH4levels, NH4data$salDepths, xlim = c(0, 0.5))

#SiO4
SiO4levels <- NULL
for(i in 1:ndepthSal) {
  SiO4s <- cruise[cruise$SALINITY..PSU. == salDepths[i],]$SILICATE..umol.L.
  SiO4levels[i] <- mean(SiO4s, na.rm = T)
}
SiO4levels

SiO4data <- data.frame(salDepths, SiO4levels)
plot(SiO4data$SiO4levels, SiO4data$salDepths, xlim = c(0, 150))

##
par(mfcol = c(1,7))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2data$O2levels, O2data$salDepths, xlim = c(0, 300), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9)
mtext(text="Depth (m)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] umol", side=1, line=2.9, cex = 1.2)

plot(NO3data$NO3levels, NO3data$salDepths, xlim = c(0, 50), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9, yaxt = 'n')
mtext(text="[NO3] umol", side=1, line=2.9, cex = 1.2)

plot(NO2data$NO2levels, NO2data$salDepths, xlim = c(0, 10), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9, yaxt = 'n')
mtext(text="[NO2] umol", side=1, line=2.9, cex = 1.2)

plot(NH4data$NH4levels, NH4data$salDepths, xlim = c(0, 2.5), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9, yaxt = 'n')
mtext(text="[NH4] umol", side=1, line=2.9, cex = 1.2)

plot(SiO4data$SiO4levels, SiO4data$salDepths, xlim = c(0, 200), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9, yaxt = 'n')
mtext(text="[SiO4] umol", side=1, line=2.9, cex = 1.2)

plot(PO4data$PO4levels, PO4data$salDepths, xlim = c(0, 4), yaxs='i', xaxs='i', col=8, pch=4, cex=0.9, yaxt = 'n')
mtext(text="[PO4] umol", side=1, line=2.9, cex = 1.2)

title("Element Depth Profiles", outer=TRUE, cex.main = 2.5, line = 1.5)

graphics.off()


lm.out <- lm(O2levels ~ salDepths, data=O2data)
plot(lm.out)


loess.fit <- loess(NO3levels ~ salDepths, data = NO3data)


xyplot(salDepths ~ NO3levels, type=c("smooth", "p"), lwd = 5, col = 1, cex = 0.2)
?xyplot
?panel.loess
