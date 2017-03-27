cruise65 <- read.csv(file = "1965 cruise.csv", header = T)
attach(cruise65)
head(cruise65)
O2 <- Oxygen....m..mol.kg. 
PO4 <- Phosphate....m..mol.kg. 
SiO4 <- Silicate....m..mol.kg.
#NO2 <- Nitrite....m..mol.kg. ## NO DATA ##
NO3 <- Nitrate....m..mol.kg. 
#NH4 <- NH4....m..mol.kg.  ## NO DATA ##


(stations <- sort(unique(Station)))

rnorm()
#OXYGEN DATA
#how can i do this fasterrrrr :( 


#for(i in 1 + 64:70) {
  #02[i] <- cruise65[Station == stations[i],]$Oxygen....m..mol.kg.
  #depth[i] <- (cruise65[Station == stations[i],]$Depth..m.) * -1
#}

## O2 ####################################################################
O2.65 <- cruise65[Station == 65,]$Oxygen....m..mol.kg.
depth.O2.65 <- (cruise65[Station == 65,]$Depth..m.) * -1

O2.66 <- cruise65[Station == 66,]$Oxygen....m..mol.kg.
depth.O2.66 <- (cruise65[Station == 66,]$Depth..m.) * -1

O2.67 <- cruise65[Station == 67,]$Oxygen....m..mol.kg.
depth.O2.67 <- (cruise65[Station == 67,]$Depth..m.) * -1

O2.68 <- cruise65[Station == 68,]$Oxygen....m..mol.kg.
depth.02.68 <- (cruise65[Station == 68,]$Depth..m.) * -1

O2.69 <- cruise65[Station == 69,]$Oxygen....m..mol.kg.
depth.O2.69 <- (cruise65[Station == 69,]$Depth..m.) * -1

O2.70 <- cruise65[Station == 70,]$Oxygen....m..mol.kg.
depth.O2.70 <- (cruise65[Station == 70,]$Depth..m.) * -1
##########################################################################


## PO4 ###################################################################
PO4.65 <- cruise65[Station == 65,]$Phosphate....m..mol.kg.
depth.PO4.65 <- (cruise65[Station == 65,]$Depth..m.) * -1

PO4.66 <- cruise65[Station == 66,]$Phosphate....m..mol.kg.
depth.PO4.66 <- (cruise65[Station == 66,]$Depth..m.) * -1

PO4.67 <- cruise65[Station == 67,]$Phosphate....m..mol.kg.
depth.PO4.67 <- (cruise65[Station == 67,]$Depth..m.) * -1

PO4.68 <- cruise65[Station == 68,]$Phosphate....m..mol.kg.
depth.PO4.68 <- (cruise65[Station == 68,]$Depth..m.) * -1

PO4.69 <- cruise65[Station == 69,]$Phosphate....m..mol.kg.
depth.PO4.69 <- (cruise65[Station == 69,]$Depth..m.) * -1

PO4.70 <- cruise65[Station == 70,]$Phosphate....m..mol.kg.
depth.PO4.70 <- (cruise65[Station == 70,]$Depth..m.) * -1
##########################################################################


## SiO4 ##################################################################
SiO4.65 <- cruise65[Station == 65,]$Silicate....m..mol.kg.
depth.SiO4.65 <- (cruise65[Station == 65,]$Depth..m.) * -1

SiO4.66 <- cruise65[Station == 66,]$Silicate....m..mol.kg.
depth.SiO4.66 <- (cruise65[Station == 66,]$Depth..m.) * -1

SiO4.67 <- cruise65[Station == 67,]$Silicate....m..mol.kg.
depth.SiO4.67 <- (cruise65[Station == 67,]$Depth..m.) * -1

SiO4.68 <- cruise65[Station == 68,]$Silicate....m..mol.kg.
depth.SiO4.68 <- (cruise65[Station == 68,]$Depth..m.) * -1

SiO4.69 <- cruise65[Station == 69,]$Silicate....m..mol.kg.
depth.SiO4.69 <- (cruise65[Station == 69,]$Depth..m.) * -1

SiO4.70 <- cruise65[Station == 70,]$Silicate....m..mol.kg.
depth.SiO4.70 <- (cruise65[Station == 70,]$Depth..m.) * -1
##########################################################################


## NO3 ###################################################################
NO3.65 <- cruise65[Station == 65,]$Nitrate....m..mol.kg.
depth.NO3.65 <- (cruise65[Station == 65,]$Depth..m.) * -1

NO3.66 <- cruise65[Station == 66,]$Nitrate....m..mol.kg.
depth.NO3.66 <- (cruise65[Station == 66,]$Depth..m.) * -1

NO3.67 <- cruise65[Station == 67,]$Nitrate....m..mol.kg.
depth.NO3.67 <- (cruise65[Station == 67,]$Depth..m.) * -1

NO3.68 <- cruise65[Station == 68,]$Nitrate....m..mol.kg.
depth.NO3.68 <- (cruise65[Station == 68,]$Depth..m.) * -1

NO3.69 <- cruise65[Station == 69,]$Nitrate....m..mol.kg.
depth.NO3.69 <- (cruise65[Station == 69,]$Depth..m.) * -1

NO3.70 <- cruise65[Station == 70,]$Nitrate....m..mol.kg.
depth.NO3.70 <- (cruise65[Station == 70,]$Depth..m.) * -1
##########################################################################


















#graphics.off()

par(mfcol = c(1,4))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2.65, depth.O2.65, xlim = c(0, 250), yaxs="i", xaxs='i', col=1, type = 'l')
lines(O2.66, depth.O2.66, col = 2)
lines(O2.67, depth.O2.67, col = 3)
lines(O2.68, depth.O2.68, col = 4)
lines(O2.69, depth.O2.69, col = 5)
lines(O2.70, depth.O2.70, col = 6)
mtext(text="Depth (m)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] ?", side=1, line=2.9, cex = 1.2)

plot(PO4.65, depth.PO4.65, xlim = c(0, 4), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(PO4.66, depth.PO4.66, col = 2)
lines(PO4.67, depth.PO4.67, col = 3)
lines(PO4.68, depth.PO4.68, col = 4)
lines(PO4.69, depth.PO4.69, col = 5)
lines(PO4.70, depth.PO4.70, col = 6)
mtext(text="[PO4] ?", side=1, line=2.9, cex = 1.2)

plot(SiO4.65, depth.SiO4.65, xlim = c(0, 140), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(SiO4.66, depth.SiO4.66, col = 2)
lines(SiO4.67, depth.SiO4.67, col = 3)
lines(SiO4.68, depth.SiO4.68, col = 4)
lines(SiO4.69, depth.SiO4.69, col = 5)
lines(SiO4.70, depth.SiO4.70, col = 6)
mtext(text="[SiO4] ?", side=1, line=2.9, cex = 1.2)

plot(NO3.65, depth.NO3.65, xlim = c(0, 140), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(NO3.66, depth.NO3.66, col = 2)
lines(NO3.67, depth.NO3.67, col = 3)
lines(NO3.68, depth.NO3.68, col = 4)
lines(NO3.69, depth.NO3.69, col = 5)
lines(NO3.70, depth.NO3.70, col = 6)

mtext(text="[NO3] ?", side=1, line=2.9, cex = 1.2)

title("Nutrient Profiles ETNP 1965", outer=TRUE, cex.main = 2.5, line = 1.5)
