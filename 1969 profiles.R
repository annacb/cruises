cruise69 <- read.csv(file = "1969 cruise.csv", header = T)
attach(cruise69)
head(cruise69)
O2 <- Oxygen....m..mol.kg. 
PO4 <- Phosphate....m..mol.kg. 
SiO4 <- Silicate....m..mol.kg.
#NO2 <- Nitrite....m..mol.kg. ## NO DATA ##
NO3 <- Nitrate....m..mol.kg. 
#NH4 <- NH4....m..mol.kg.  ## NO DATA ##


sort(unique(Station))


## O2 ####################################################################
O2.24 <- cruise69[Station == 24,]$Oxygen....m..mol.kg.
depth.O2.24 <- (cruise69[Station == 24,]$Depth..m.) * -1

O2.25 <- cruise69[Station == 25,]$Oxygen....m..mol.kg.
depth.O2.25 <- (cruise69[Station == 25,]$Depth..m.) * -1

O2.26 <- cruise69[Station == 26,]$Oxygen....m..mol.kg.
depth.O2.26 <- (cruise69[Station == 26,]$Depth..m.) * -1

O2.27 <- cruise69[Station == 27,]$Oxygen....m..mol.kg.
depth.02.27 <- (cruise69[Station == 27,]$Depth..m.) * -1

O2.28 <- cruise69[Station == 28,]$Oxygen....m..mol.kg.
depth.O2.28 <- (cruise69[Station == 28,]$Depth..m.) * -1

O2.29 <- cruise69[Station == 29,]$Oxygen....m..mol.kg.
depth.O2.29 <- (cruise69[Station == 29,]$Depth..m.) * -1

O2.30 <- cruise69[Station == 30,]$Oxygen....m..mol.kg.
depth.O2.30 <- (cruise69[Station == 30,]$Depth..m.) * -1
##########################################################################


## PO4 ###################################################################
PO4.24 <- cruise69[Station == 24,]$Phosphate....m..mol.kg.
depth.PO4.24 <- (cruise69[Station == 24,]$Depth..m.) * -1

PO4.25 <- cruise69[Station == 25,]$Phosphate....m..mol.kg.
depth.PO4.25 <- (cruise69[Station == 25,]$Depth..m.) * -1

PO4.26 <- cruise69[Station == 26,]$Phosphate....m..mol.kg.
depth.PO4.26 <- (cruise69[Station == 26,]$Depth..m.) * -1

PO4.27 <- cruise69[Station == 27,]$Phosphate....m..mol.kg.
depth.PO4.27 <- (cruise69[Station == 27,]$Depth..m.) * -1

PO4.28 <- cruise69[Station == 28,]$Phosphate....m..mol.kg.
depth.PO4.28 <- (cruise69[Station == 28,]$Depth..m.) * -1

PO4.29 <- cruise69[Station == 29,]$Phosphate....m..mol.kg.
depth.PO4.29 <- (cruise69[Station == 29,]$Depth..m.) * -1

PO4.30 <- cruise69[Station == 30,]$Phosphate....m..mol.kg.
depth.PO4.30 <- (cruise69[Station == 30,]$Depth..m.) * -1
##########################################################################


## SiO4 ##################################################################
SiO4.24 <- cruise69[Station == 24,]$Silicate....m..mol.kg.
depth.SiO4.24 <- (cruise69[Station == 24,]$Depth..m.) * -1

SiO4.25 <- cruise69[Station == 25,]$Silicate....m..mol.kg.
depth.SiO4.25 <- (cruise69[Station == 25,]$Depth..m.) * -1

SiO4.26 <- cruise69[Station == 26,]$Silicate....m..mol.kg.
depth.SiO4.26 <- (cruise69[Station == 26,]$Depth..m.) * -1

SiO4.27 <- cruise69[Station == 27,]$Silicate....m..mol.kg.
depth.SiO4.27 <- (cruise69[Station == 27,]$Depth..m.) * -1

SiO4.28 <- cruise69[Station == 28,]$Silicate....m..mol.kg.
depth.SiO4.28 <- (cruise69[Station == 28,]$Depth..m.) * -1

SiO4.29 <- cruise69[Station == 29,]$Silicate....m..mol.kg.
depth.SiO4.29 <- (cruise69[Station == 29,]$Depth..m.) * -1

SiO4.30 <- cruise69[Station == 30,]$Silicate....m..mol.kg.
depth.SiO4.30 <- (cruise69[Station == 30,]$Depth..m.) * -1
##########################################################################


## NO3 ###################################################################
NO3.24 <- cruise69[Station == 24,]$Nitrate....m..mol.kg.
depth.NO3.24 <- (cruise69[Station == 24,]$Depth..m.) * -1

NO3.25 <- cruise69[Station == 25,]$Nitrate....m..mol.kg.
depth.NO3.25 <- (cruise69[Station == 25,]$Depth..m.) * -1

NO3.26 <- cruise69[Station == 26,]$Nitrate....m..mol.kg.
depth.NO3.26 <- (cruise69[Station == 26,]$Depth..m.) * -1

NO3.27 <- cruise69[Station == 27,]$Nitrate....m..mol.kg.
depth.NO3.27 <- (cruise69[Station == 27,]$Depth..m.) * -1

NO3.28 <- cruise69[Station == 28,]$Nitrate....m..mol.kg.
depth.NO3.28 <- (cruise69[Station == 28,]$Depth..m.) * -1

NO3.29 <- cruise69[Station == 29,]$Nitrate....m..mol.kg.
depth.NO3.29 <- (cruise69[Station == 29,]$Depth..m.) * -1

NO3.30 <- cruise69[Station == 30,]$Nitrate....m..mol.kg.
depth.NO3.30 <- (cruise69[Station == 30,]$Depth..m.) * -1
##########################################################################














length(depth.O2.)



#graphics.off()

par(mfcol = c(1,4))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2.24, depth.O2.24, xlim = c(0, 250), yaxs="i", xaxs='i', col=1, type = 'l')
lines(O2.25, depth.O2.25, col = 2)
lines(O2.26, depth.O2.26, col = 3)
lines(O2.27, depth.O2.27, col = 4)
lines(O2.28, depth.O2.28, col = 5)
lines(O2.29, depth.O2.29, col = 6)
lines(O2.30, depth.O2.30, col = 7)
mtext(text="Depth (m)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] ?", side=1, line=2.9, cex = 1.2)

plot(PO4.24, depth.PO4.24, xlim = c(0, 4), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(PO4.25, depth.PO4.25, col = 2)
lines(PO4.26, depth.PO4.26, col = 3)
lines(PO4.27, depth.PO4.27, col = 4)
lines(PO4.28, depth.PO4.28, col = 5)
lines(PO4.29, depth.PO4.29, col = 6)
lines(PO4.30, depth.PO4.30, col = 7)
mtext(text="[PO4] ?", side=1, line=2.9, cex = 1.2)

plot(SiO4.24, depth.SiO4.24, xlim = c(0, 40), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(SiO4.25, depth.SiO4.25, col = 2)
lines(SiO4.26, depth.SiO4.26, col = 3)
lines(SiO4.27, depth.SiO4.27, col = 4)
lines(SiO4.28, depth.SiO4.28, col = 5)
lines(SiO4.29, depth.SiO4.29, col = 6)
lines(SiO4.30, depth.SiO4.30, col = 7)
mtext(text="[SiO4] ?", side=1, line=2.9, cex = 1.2)

plot(NO3.24, depth.NO3.24, xlim = c(0, 40), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(NO3.25, depth.NO3.25, col = 2)
lines(NO3.26, depth.NO3.26, col = 3)
lines(NO3.27, depth.NO3.27, col = 4)
lines(NO3.28, depth.NO3.28, col = 5)
lines(NO3.29, depth.NO3.29, col = 6)
lines(NO3.30, depth.NO3.30, col = 7)
mtext(text="[NO3] ?", side=1, line=2.9, cex = 1.2)

title("Nutrient Profiles ETNP 1969", outer=TRUE, cex.main = 2.5, line = 1.5)
