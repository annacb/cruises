cruise94 <- read.csv(file = "1994 cruise.csv", header = T)
attach(cruise94)
head(cruise94)
O2 <- Oxygen....m..mol.kg. 
PO4 <- Phosphate....m..mol.kg. 
SiO4 <- Silicate....m..mol.kg.
NO2 <- Nitrite....m..mol.kg.
NO3 <- Nitrate....m..mol.kg. 
#NH4 <- NH4....m..mol.kg.  ## NO DATA ##


sort(unique(Station))


## O2 ####################################################################
O2.174 <- cruise94[Station == 174,]$Oxygen....m..mol.kg.
depth.O2.034a <- (cruise94[Station == 174,]$Depth..m.) * -1

O2.175 <- cruise94[Station == 35,]$Oxygen....m..mol.kg.
depth.O2.35 <- (cruise94[Station == 35,]$Depth..m.) * -1

O2.176 <- cruise94[Station == 36,]$Oxygen....m..mol.kg.
depth.O2.36 <- (cruise94[Station == 36,]$Depth..m.) * -1

O2.177 <- cruise94[Station == 37,]$Oxygen....m..mol.kg.
depth.02.37 <- (cruise94[Station == 37,]$Depth..m.) * -1

O2.178 <- cruise94[Station == 38,]$Oxygen....m..mol.kg.
depth.O2.38 <- (cruise94[Station == 38,]$Depth..m.) * -1

O2.179 <- cruise94[Station == 39,]$Oxygen....m..mol.kg.
depth.O2.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

O2.180 <- cruise94[Station == 40,]$Oxygen....m..mol.kg.
depth.O2.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

O2.181 <- cruise94[Station == 41,]$Oxygen....m..mol.kg.
depth.O2.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

O2.182 <- cruise94[Station == 42,]$Oxygen....m..mol.kg.
depth.O2.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

O2.183 <- cruise94[Station == 43,]$Oxygen....m..mol.kg.
depth.O2.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

O2.184 <- cruise94[Station == 44,]$Oxygen....m..mol.kg.
depth.02.44 <- (cruise94[Station == 44,]$Depth..m.) * -1

O2.185 <- cruise94[Station == 45,]$Oxygen....m..mol.kg.
depth.O2.45 <- (cruise94[Station == 45,]$Depth..m.) * -1

O2.186 <- cruise94[Station == 46,]$Oxygen....m..mol.kg.
depth.O2.46 <- (cruise94[Station == 46,]$Depth..m.) * -1

O2.187 <- cruise94[Station == 47,]$Oxygen....m..mol.kg.
depth.O2.47 <- (cruise94[Station == 47,]$Depth..m.) * -1

O2.188 <- cruise94[Station == 48,]$Oxygen....m..mol.kg.
depth.O2.48 <- (cruise94[Station == 48,]$Depth..m.) * -1

O2.189 <- cruise94[Station == 39,]$Oxygen....m..mol.kg.
depth.O2.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

O2.190 <- cruise94[Station == 40,]$Oxygen....m..mol.kg.
depth.O2.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

O2.191 <- cruise94[Station == 41,]$Oxygen....m..mol.kg.
depth.O2.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

O2.192 <- cruise94[Station == 42,]$Oxygen....m..mol.kg.
depth.O2.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

O2.193 <- cruise94[Station == 43,]$Oxygen....m..mol.kg.
depth.O2.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

O2.194 <- cruise94[Station == 44,]$Oxygen....m..mol.kg.
depth.02.44 <- (cruise94[Station == 44,]$Depth..m.) * -1
##########################################################################


## PO4 ###################################################################
PO4.034a <- cruise94[Station == 174,]$Phosphate....m..mol.kg.
depth.PO4.034a <- (cruise94[Station == 174,]$Depth..m.) * -1

PO4.35 <- cruise94[Station == 35,]$Phosphate....m..mol.kg.
depth.PO4.35 <- (cruise94[Station == 35,]$Depth..m.) * -1

PO4.36 <- cruise94[Station == 36,]$Phosphate....m..mol.kg.
depth.PO4.36 <- (cruise94[Station == 36,]$Depth..m.) * -1

PO4.37 <- cruise94[Station == 37,]$Phosphate....m..mol.kg.
depth.PO4.37 <- (cruise94[Station == 37,]$Depth..m.) * -1

PO4.38 <- cruise94[Station == 38,]$Phosphate....m..mol.kg.
depth.PO4.38 <- (cruise94[Station == 38,]$Depth..m.) * -1

PO4.39 <- cruise94[Station == 39,]$Phosphate....m..mol.kg.
depth.PO4.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

PO4.40 <- cruise94[Station == 40,]$Phosphate....m..mol.kg.
depth.PO4.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

PO4.41 <- cruise94[Station == 41,]$Phosphate....m..mol.kg.
depth.PO4.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

PO4.42 <- cruise94[Station == 42,]$Phosphate....m..mol.kg.
depth.PO4.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

PO4.43 <- cruise94[Station == 43,]$Phosphate....m..mol.kg.
depth.PO4.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

PO4.44 <- cruise94[Station == 44,]$Phosphate....m..mol.kg.
depth.PO4.44 <- (cruise94[Station == 44,]$Depth..m.) * -1

PO4.45 <- cruise94[Station == 45,]$Phosphate....m..mol.kg.
depth.PO4.45 <- (cruise94[Station == 45,]$Depth..m.) * -1

PO4.46 <- cruise94[Station == 46,]$Phosphate....m..mol.kg.
depth.PO4.46 <- (cruise94[Station == 46,]$Depth..m.) * -1

PO4.47 <- cruise94[Station == 47,]$Phosphate....m..mol.kg.
depth.PO4.47 <- (cruise94[Station == 47,]$Depth..m.) * -1

PO4.48 <- cruise94[Station == 48,]$Phosphate....m..mol.kg.
depth.PO4.48 <- (cruise94[Station == 48,]$Depth..m.) * -1
##########################################################################


## SiO4 ##################################################################
SiO4.034a <- cruise94[Station == 174,]$Silicate....m..mol.kg.
depth.SiO4.034a <- (cruise94[Station == 174,]$Depth..m.) * -1

SiO4.35 <- cruise94[Station == 35,]$Silicate....m..mol.kg.
depth.SiO4.35 <- (cruise94[Station == 35,]$Depth..m.) * -1

SiO4.36 <- cruise94[Station == 36,]$Silicate....m..mol.kg.
depth.SiO4.36 <- (cruise94[Station == 36,]$Depth..m.) * -1

SiO4.37 <- cruise94[Station == 37,]$Silicate....m..mol.kg.
depth.SiO4.37 <- (cruise94[Station == 37,]$Depth..m.) * -1

SiO4.38 <- cruise94[Station == 38,]$Silicate....m..mol.kg.
depth.SiO4.38 <- (cruise94[Station == 38,]$Depth..m.) * -1

SiO4.39 <- cruise94[Station == 39,]$Silicate....m..mol.kg.
depth.SiO4.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

SiO4.40 <- cruise94[Station == 40,]$Silicate....m..mol.kg.
depth.SiO4.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

SiO4.41 <- cruise94[Station == 41,]$Silicate....m..mol.kg.
depth.SiO4.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

SiO4.42 <- cruise94[Station == 42,]$Silicate....m..mol.kg.
depth.SiO4.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

SiO4.43 <- cruise94[Station == 43,]$Silicate....m..mol.kg.
depth.SiO4.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

SiO4.44 <- cruise94[Station == 44,]$Silicate....m..mol.kg.
depth.SiO4.44 <- (cruise94[Station == 44,]$Depth..m.) * -1

SiO4.45 <- cruise94[Station == 45,]$Silicate....m..mol.kg.
depth.SiO4.45 <- (cruise94[Station == 45,]$Depth..m.) * -1

SiO4.46 <- cruise94[Station == 46,]$Silicate....m..mol.kg.
depth.SiO4.46 <- (cruise94[Station == 46,]$Depth..m.) * -1

SiO4.47 <- cruise94[Station == 47,]$Silicate....m..mol.kg.
depth.SiO4.47 <- (cruise94[Station == 47,]$Depth..m.) * -1

SiO4.48 <- cruise94[Station == 48,]$Silicate....m..mol.kg.
depth.SiO4.48 <- (cruise94[Station == 48,]$Depth..m.) * -1
##########################################################################


## NO2 ###################################################################
NO2.034a <- cruise94[Station == 174,]$Nitrite....m..mol.kg.
depth.NO2.034a <- (cruise94[Station == 174,]$Depth..m.) * -1

NO2.35 <- cruise94[Station == 35,]$Nitrite....m..mol.kg.
depth.NO2.35 <- (cruise94[Station == 35,]$Depth..m.) * -1

NO2.36 <- cruise94[Station == 36,]$Nitrite....m..mol.kg.
depth.NO2.36 <- (cruise94[Station == 36,]$Depth..m.) * -1

NO2.37 <- cruise94[Station == 37,]$Nitrite....m..mol.kg.
depth.NO2.37 <- (cruise94[Station == 37,]$Depth..m.) * -1

NO2.38 <- cruise94[Station == 38,]$Nitrite....m..mol.kg.
depth.NO2.38 <- (cruise94[Station == 38,]$Depth..m.) * -1

NO2.39 <- cruise94[Station == 39,]$Nitrite....m..mol.kg.
depth.NO2.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

NO2.40 <- cruise94[Station == 40,]$Nitrite....m..mol.kg.
depth.NO2.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

NO2.41 <- cruise94[Station == 41,]$Nitrite....m..mol.kg.
depth.NO2.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

NO2.42 <- cruise94[Station == 42,]$Nitrite....m..mol.kg.
depth.NO2.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

NO2.43 <- cruise94[Station == 43,]$Nitrite....m..mol.kg.
depth.NO2.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

NO2.44 <- cruise94[Station == 44,]$Nitrite....m..mol.kg.
depth.NO2.44 <- (cruise94[Station == 44,]$Depth..m.) * -1

NO2.45 <- cruise94[Station == 45,]$Nitrite....m..mol.kg.
depth.NO2.45 <- (cruise94[Station == 45,]$Depth..m.) * -1

NO2.46 <- cruise94[Station == 46,]$Nitrite....m..mol.kg.
depth.NO2.46 <- (cruise94[Station == 46,]$Depth..m.) * -1

NO2.47 <- cruise94[Station == 47,]$Nitrite....m..mol.kg.
depth.NO2.47 <- (cruise94[Station == 47,]$Depth..m.) * -1

NO2.48 <- cruise94[Station == 48,]$Nitrite....m..mol.kg.
depth.NO2.48 <- (cruise94[Station == 48,]$Depth..m.) * -1
##########################################################################


## NO3 ###################################################################
NO3.034a <- cruise94[Station == 174,]$Nitrate....m..mol.kg.
depth.NO3.034a <- (cruise94[Station == 174,]$Depth..m.) * -1

NO3.35 <- cruise94[Station == 35,]$Nitrate....m..mol.kg.
depth.NO3.35 <- (cruise94[Station == 35,]$Depth..m.) * -1

NO3.36 <- cruise94[Station == 36,]$Nitrate....m..mol.kg.
depth.NO3.36 <- (cruise94[Station == 36,]$Depth..m.) * -1

NO3.37 <- cruise94[Station == 37,]$Nitrate....m..mol.kg.
depth.NO3.37 <- (cruise94[Station == 37,]$Depth..m.) * -1

NO3.38 <- cruise94[Station == 38,]$Nitrate....m..mol.kg.
depth.NO3.38 <- (cruise94[Station == 38,]$Depth..m.) * -1

NO3.39 <- cruise94[Station == 39,]$Nitrate....m..mol.kg.
depth.NO3.39 <- (cruise94[Station == 39,]$Depth..m.) * -1

NO3.40 <- cruise94[Station == 40,]$Nitrate....m..mol.kg.
depth.NO3.40 <- (cruise94[Station == 40,]$Depth..m.) * -1

NO3.41 <- cruise94[Station == 41,]$Nitrate....m..mol.kg.
depth.NO3.41 <- (cruise94[Station == 41,]$Depth..m.) * -1

NO3.42 <- cruise94[Station == 42,]$Nitrate....m..mol.kg.
depth.NO3.42 <- (cruise94[Station == 42,]$Depth..m.) * -1

NO3.43 <- cruise94[Station == 43,]$Nitrate....m..mol.kg.
depth.NO3.43 <- (cruise94[Station == 43,]$Depth..m.) * -1

NO3.44 <- cruise94[Station == 44,]$Nitrate....m..mol.kg.
depth.NO3.44 <- (cruise94[Station == 44,]$Depth..m.) * -1

NO3.45 <- cruise94[Station == 45,]$Nitrate....m..mol.kg.
depth.NO3.45 <- (cruise94[Station == 45,]$Depth..m.) * -1

NO3.46 <- cruise94[Station == 46,]$Nitrate....m..mol.kg.
depth.NO3.46 <- (cruise94[Station == 46,]$Depth..m.) * -1

NO3.47 <- cruise94[Station == 47,]$Nitrate....m..mol.kg.
depth.NO3.47 <- (cruise94[Station == 47,]$Depth..m.) * -1

NO3.48 <- cruise94[Station == 48,]$Nitrate....m..mol.kg.
depth.NO3.48 <- (cruise94[Station == 48,]$Depth..m.) * -1
##########################################################################














length(depth.O2.)



#graphics.off()

par(mfcol = c(1,5))
par(mar=c(1,1,1,1), oma=c(10,8,5,5))
plot(O2.034a, depth.O2.034a, xlim = c(0, 250), yaxs="i", xaxs='i', col=1, type = 'l')
lines(O2.35, depth.O2.35, col = 2)
lines(O2.36, depth.O2.36, col = 3)
lines(O2.37, depth.O2.37, col = 4)
lines(O2.38, depth.O2.38, col = 5)
lines(O2.39, depth.O2.39, col = 6)
lines(O2.40, depth.O2.40, col = 7)
lines(O2.41, depth.O2.41, col = 8)
lines(O2.42, depth.O2.42, col = 9)
lines(O2.43, depth.O2.43, col = 10)
lines(O2.44, depth.O2.44, col = 11)
lines(O2.45, depth.O2.45, col = 12)
lines(O2.46, depth.O2.46, col = 13)
lines(O2.47, depth.O2.47, col = 14)
lines(O2.48, depth.O2.48, col = 15)
mtext(text="Depth (m)", side=2, line=2.9, cex = 1.2)
mtext(text="[O2] ?", side=1, line=2.9, cex = 1.2)

plot(PO4.034a, depth.PO4.034a, xlim = c(0, 4), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(PO4.35, depth.PO4.35, col = 2)
lines(PO4.36, depth.PO4.36, col = 3)
lines(PO4.37, depth.PO4.37, col = 4)
lines(PO4.38, depth.PO4.38, col = 5)
lines(PO4.39, depth.PO4.39, col = 6)
lines(PO4.40, depth.PO4.40, col = 7)
lines(PO4.41, depth.PO4.41, col = 8)
lines(PO4.42, depth.PO4.42, col = 9)
lines(PO4.43, depth.PO4.43, col = 10)
lines(PO4.44, depth.PO4.44, col = 11)
lines(PO4.45, depth.PO4.45, col = 12)
lines(PO4.46, depth.PO4.46, col = 13)
lines(PO4.47, depth.PO4.47, col = 14)
lines(PO4.48, depth.PO4.48, col = 15)
mtext(text="[PO4] ?", side=1, line=2.9, cex = 1.2)

plot(SiO4.034a, depth.SiO4.034a, xlim = c(0, 140), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(SiO4.35, depth.SiO4.35, col = 2)
lines(SiO4.36, depth.SiO4.36, col = 3)
lines(SiO4.37, depth.SiO4.37, col = 4)
lines(SiO4.38, depth.SiO4.38, col = 5)
lines(SiO4.39, depth.SiO4.39, col = 6)
lines(SiO4.40, depth.SiO4.40, col = 7)
lines(SiO4.41, depth.SiO4.41, col = 8)
lines(SiO4.42, depth.SiO4.42, col = 9)
lines(SiO4.43, depth.SiO4.43, col = 10)
lines(SiO4.44, depth.SiO4.44, col = 11)
lines(SiO4.45, depth.SiO4.45, col = 12)
lines(SiO4.46, depth.SiO4.46, col = 13)
lines(SiO4.47, depth.SiO4.47, col = 14)
lines(SiO4.48, depth.SiO4.48, col = 15)
mtext(text="[SiO4] ?", side=1, line=2.9, cex = 1.2)

plot(NO2.034a, depth.NO2.034a, xlim = c(0, 4), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(NO2.35, depth.NO2.35, col = 2)
lines(NO2.36, depth.NO2.36, col = 3)
lines(NO2.37, depth.NO2.37, col = 4)
lines(NO2.38, depth.NO2.38, col = 5)
lines(NO2.39, depth.NO2.39, col = 6)
lines(NO2.40, depth.NO2.40, col = 7)
lines(NO2.41, depth.NO2.41, col = 8)
lines(NO2.42, depth.NO2.42, col = 9)
lines(NO2.43, depth.NO2.43, col = 10)
lines(NO2.44, depth.NO2.44, col = 11)
lines(NO2.45, depth.NO2.45, col = 12)
lines(NO2.46, depth.NO2.46, col = 13)
lines(NO2.47, depth.NO2.47, col = 14)
lines(NO2.48, depth.NO2.48, col = 15)
mtext(text="[NO2] ?", side=1, line=2.9, cex = 1.2)

plot(NO3.034a, depth.NO3.034a, xlim = c(0, 50), yaxs="i", xaxs='i', yaxt = 'n', col=1, type = 'l')
lines(NO3.35, depth.NO3.35, col = 2)
lines(NO3.36, depth.NO3.36, col = 3)
lines(NO3.37, depth.NO3.37, col = 4)
lines(NO3.38, depth.NO3.38, col = 5)
lines(NO3.39, depth.NO3.39, col = 6)
lines(NO3.40, depth.NO3.40, col = 7)
lines(NO3.41, depth.NO3.41, col = 8)
lines(NO3.42, depth.NO3.42, col = 9)
lines(NO3.43, depth.NO3.43, col = 10)
lines(NO3.44, depth.NO3.44, col = 11)
lines(NO3.45, depth.NO3.45, col = 12)
lines(NO3.46, depth.NO3.46, col = 13)
lines(NO3.47, depth.NO3.47, col = 14)
lines(NO3.48, depth.NO3.48, col = 15)
mtext(text="[NO3] ?", side=1, line=2.9, cex = 1.2)

title("Nutrient Profiles ETNP 1972", outer=TRUE, cex.main = 2.5, line = 1.5)
