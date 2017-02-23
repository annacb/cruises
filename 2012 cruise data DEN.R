cruise <- read.csv(file = "TN278_CTD_Bottle.csv", header = T)

pressures <- sort(unique(cruise$PRESSURE..DB.))
graphPressures <- pressures * -1
npress <- length(pressures)


#O2
O2lvlDbar <- NULL
for(i in 1:npress) {
  O2Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$OXYGEN..umol.l.
  O2lvlDbar[i] <- mean(O2Dbar, na.rm = T)
}
O2lvlDbar

O2dataDbar <- data.frame(graphPressures, O2lvlDbar)
O2dataDbar
plot(O2dataDbar$O2lvlDbar, O2dataDbar$graphPressures, xlim = c(0, 1000))


#NO3
NO3lvlDbar <- NULL
for(i in 1:npress) {
  NO3Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$NITRATE..umol.L.
  NO3lvlDbar[i] <- mean(NO3Dbar, na.rm = T)
}
NO3lvlDbar

NO3dataDbar <- data.frame(graphPressures, NO3lvlDbar)
plot(NO3dataDbar$NO3lvlDbar, NO3dataDbar$graphPressures, xlim = c(0, 100))

#NO2
NO2lvlDbar <- NULL
for(i in 1:npress) {
  NO2Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$NITRITE..umol.L.
  NO2lvlDbar[i] <- mean(NO2Dbar, na.rm = T)
}
NO2lvlDbar

NO2dataDbar <- data.frame(graphPressures, NO2lvlDbar)
plot(NO2dataDbar$NO2lvlDbar, NO2dataDbar$graphPressures, xlim = c(0, 10))

#PO4
PO4lvlDbar <- NULL
for(i in 1:npress) {
  PO4Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$PHOSPHATE..umol.L.
  PO4lvlDbar[i] <- mean(PO4Dbar, na.rm = T)
}
PO4lvlDbar

PO4dataDbar <- data.frame(graphPressures, PO4lvlDbar)
plot(PO4dataDbar$PO4lvlDbar, PO4dataDbar$graphPressures, xlim = c(0, 10))

#NH4
NH4lvlDbar <- NULL
for(i in 1:npress) {
  NH4Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$AMMONIUM..umol.L.
  NH4lvlDbar[i] <- mean(NH4Dbar, na.rm = T)
}
NH4lvlDbar

NH4dataDbar <- data.frame(graphPressures, NH4lvlDbar)
plot(NH4dataDbar$NH4lvlDbar, NH4dataDbar$graphPressures, xlim = c(0, 0.5))

#SiO4
SiO4lvlDbar <- NULL
for(i in 1:npress) {
  SiO4Dbar <- cruise[cruise$PRESSURE..DB. == pressures[i],]$SILICATE..umol.L.
  SiO4lvlDbar[i] <- mean(SiO4Dbar, na.rm = T)
}
SiO4lvlDbar

SiO4dataDbar <- data.frame(graphPressures, SiO4lvlDbar)
plot(SiO4dataDbar$SiO4lvlDbar, SiO4dataDbar$graphPressuress, xlim = c(0, 150))

##


#par
par(mfcol = c(1,7))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2dataDbar$O2lvlDbar, O2dataDbar$graphPressures, xlim = c(0, 300), yaxs="i", col=1, pch=19, cex=0.2)
lines(O2dataDbar$O2lvlDbar, O2dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="Pressure (dBar)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] umol", side=1, line=2.9, cex = 1.2)

plot(NO3dataDbar$NO3lvlDbar, NO3dataDbar$graphPressures, xlim = c(0, 75), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
lines(NO3dataDbar$NO3lvlDbar, NO3dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NO3] umol", side=1, line=2.9, cex = 1.2)

plot(NO2dataDbar$NO2lvlDbar, NO2dataDbar$graphPressures, xlim = c(0, 10), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
lines(NO2dataDbar$NO2lvlDbar, NO2dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NO2] umol", side=1, line=2.9, cex = 1.2)

plot(NH4dataDbar$NH4lvlDbar, NH4dataDbar$graphPressures, xlim = c(0, 2.5), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
lines(NH4dataDbar$NH4lvlDbar, NH4dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[NH4] umol", side=1, line=2.9, cex = 1.2)

plot(SiO4dataDbar$SiO4lvlDbar, SiO4dataDbar$graphPressures, xlim = c(0, 300), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
lines(SiO4dataDbar$SiO4lvlDbar, SiO4dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[SiO4] umol", side=1, line=2.9, cex = 1.2)

plot(PO4dataDbar$PO4lvlDbar, PO4dataDbar$graphPressuress, xlim = c(0, 4000), yaxs="i", col=1, pch=19, cex=0.2, yaxt = 'n')
lines(PO4dataDbar$PO4lvlDbar, PO4dataDbar$graphPressures, col=1, pch=17, cex=1, lwd = 1.5)
mtext(text="[PO4] umol", side=1, line=2.9, cex = 1.2)

title("Element Pressure Profiles", outer=TRUE, cex.main = 2.5, line = 1.5)

graphics.off()
