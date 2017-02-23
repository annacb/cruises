nitrite2012 <- read.csv(file = "TN278_CTD_Bottle.csv", header = T)
head(nitrite2012)
str(nitrite2012)

#CAST 137 - 146 [A]
#16.5˚N 107.2˚W
    #USE CAST 141 DATA

#CAST 147 - 151 [B]
#16.5˚N 107.1˚W
   #USE CAST 149 DATA

#CAST 152 - 159 [C]
#16.6˚N 107.2˚W
    #USE CAST 157 DATA

#CAST 160 [D]
#16.6˚N 107.1˚W
    #none...


nitrite2012$NITRITE..umol.L.
nitrite2012A <- subset(nitrite2012, select = c("mon.day.yy", "hh.mm", "station", "PRESSURE..DB.", "NITRITE..umol.L."))
head(nitrite2012A)
names(nitrite2012A) <- c("date", "time", "station", "press", "concentration")
head(nitrite2012A)




#STATION 141 [A]
nitrite2012.A <- nitrite2012A[nitrite2012A$station == 141,]
nitrite2012.A
stat141press <- nitrite2012.A$press *-1
plot(nitrite2012.A$concentration, stat141press)

#STATION 149 [B]
nitrite2012.B <- nitrite2012A[nitrite2012A$station == 149,]
stat149press <- nitrite2012.B$press *-1
plot(nitrite2012.B$concentration, stat149press)

#STATION 157 [C]
nitrite2012.C <- nitrite2012A[nitrite2012A$station == 157,]
stat157press <- nitrite2012.C$press *-1
plot(nitrite2012.C$concentration, stat157press)

#plot......
plot(nitrite2012.A$concentration, stat141press, type = 'l', lwd = 2)
lines(nitrite2012.B$concentration, stat149press)



ggplot(nitrite2012B, aes(press, concentration)) + geom_line(color = 'salmon2') + coord_flip() + facet_wrap('station', nrow = 5) + scale_x_reverse()


pdf("2012 all casts.pdf",width=7,height=5) 
ggplot(nitrite2012A, aes(press, concentration)) + geom_line(color = 'salmon2') + coord_flip() + facet_wrap('station', nrow = 5) + scale_x_reverse()
dev.off()


#trying things..........
avg137.146 <- subset(nitrite2012A, station = 137:146, select=c("press", "concentration", 'station'))
n.A <- nrow(avg137.146)
press.A <- sort(unique(avg137.146[,1]))
length(press.A)
?unique
graphPressures <- avg137.146$press * -1
head(avg137.146)
tail(avg137.146)

ndata <- length(nitrite2012A$date)
ndata
stations <- sort(unique(nitrite2012A$cast))

NO2.A <- NULL
for(i in 1:n.A) {
  NO2.Acons <- avg137.146[avg137.146$press == press.A[i],]$concentration
  NO2.A[i] <- mean(NO2.Acons, na.rm = T)
}
NO2.A

NO2data <- data.frame(graphPressures, NO2.A)
plot(NO2data$NO2.A, NO2data$graphPressures, xlim = c(0, 9))
