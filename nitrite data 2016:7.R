nitrite <- read.csv("All.csv", header = T)
nitrite
str(nitrite)

nitrite$Date.and.time
nitrite$Concentration.._mol.L.
nitrite2 <- subset(nitrite, select = c("Date.and.time", "Station", "Cast", "depth..m.", "Concentration.._mol.L."))
head(nitrite2)
names(nitrite2) <- c("date", "station", "cast", "depth", "concentration")
head(nitrite2)

nitrite2$date <- as.POSIXct(nitrite$Date.and.time,format='%d/%m/%y %H:%M',tz = "America/Chicago")
nitrite2
str(nitrite2)



pdf("all casts.pdf",width=7,height=5) 
ggplot(nitrite2, aes(depth, concentration)) + geom_line(color = 'salmon2') + coord_flip() + facet_wrap('cast', nrow = 3) + scale_x_reverse()
dev.off()

pdf("cast 42.pdf",width=7,height=5) 
ggplot(subset(nitrite2, cast == 42), aes(depth, concentration)) + geom_line(color = 'salmon2') + coord_flip() + facet_wrap('cast', nrow = 3) + scale_x_reverse()
dev.off()
