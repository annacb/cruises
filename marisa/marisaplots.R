marisa <- read.csv(file = "marisadata2.csv", header = T)

par(mfcol = c(5,1))
par(mar=c(0,0,0,0), oma=c(5,8,5,5))
plot(marisa$Year, marisa$Cruises, col=4, pch=19, cex=0, xlim=c(1951, 2016), ylim=c(0, 12), xaxt = 'n', cex.axis = 1.5, font.axis = 2, xaxs="i")
lines(marisa$Year, marisa$Cruises, col=4, pch=17, cex=1, lwd = 4)
axis(side = 1, at = seq(1951, 2016, by = 5), labels = F, tcl = -0.5)
grid(0, NULL, lty = 2, lwd = 1)
mtext(text="Cruises", side=2, line=2.9, cex = 1.2)

plot(marisa$Year, marisa$Publications, col=1, pch=1, cex=0, xlim=c(1951, 2016), ylim=c(0, 27), xaxt = 'n', cex.axis = 1.5, font.axis = 2, xaxs="i")
lines(marisa$Year, marisa$Publications, col='darkolivegreen3', pch=17, cex=1, lwd = 4)
axis(side = 1, at = seq(1951, 2016, by = 5), labels = F, tcl = -0.5)
grid(0, NULL, lty = 2, lwd = 1)
mtext(text="Publications", side=2, line=2.9, cex = 1.2)

plot(marisa$Year, marisa$Sediment.Cores, col=6, pch=19, cex=0, xlim=c(1951, 2016), ylim=c(0, 109), cex.axis = 1.5, font.axis = 2, xaxt = 'n', xaxs="i")
axis(side = 1, at = seq(1951, 2016, by = 5), labels = F, tcl = -0.5, font.axis = 2, cex.axis = 1.5)
lines(marisa$Year, marisa$Sediment.Cores, col='9', pch=17, cex=1, lwd = 4)
grid(0, NULL, lty = 2, lwd = 1)
mtext(text="Sediment\nCores", side=2, line=2.9, cex = 1.2)

plot(marisa$Year, marisa$Age.Model.Count, col=6, pch=19, cex=0, xlim=c(1951, 2016), ylim=c(0, 24), cex.axis = 1.5, font.axis = 2, xaxt = 'n', xaxs="i")
axis(side = 1, at = seq(1951, 2016, by = 5), labels = F, tcl = -0.5, font.axis = 2, cex.axis = 1.5)
lines(marisa$Year, marisa$Age.Model.Count, col='darkorchid2', pch=17, cex=1, lwd = 4)
grid(0, NULL, lty = 2, lwd = 1)
mtext(text="Age Model\nCount", side=2, line=2.9, cex = 1.2)

plot(marisa$Year, marisa$Radiocarbon.Count, col=6, pch=19, cex=0, xlim=c(1951, 2016), ylim=c(0, 14), cex.axis = 1.5, font.axis = 2, xaxt = 'n', xaxs="i")
axis(side = 1, at = seq(1951, 2016, by = 5), labels = T, tcl = -0.5, font.axis = 2, cex.axis = 1.5)
lines(marisa$Year, marisa$Radiocarbon.Count, col='salmon', pch=17, cex=1, lwd = 4)
grid(0, NULL, lty = 2, lwd = 1)
mtext(text="Radiocarbon\nCount", side=2, line=2.9, cex = 1.2)

mtext(text="Year", side=1, line=2.9, cex = 1.2)
title("Research Effort, 1951 to 2016", outer=TRUE, cex.main = 2.5, line = 1.5)

graphics.off()
