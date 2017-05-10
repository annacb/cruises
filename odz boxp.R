# depths <- c(seq(from = 1200, to = 0, by = -150))
cruise <- c(1:7, 1:7)
odz <- data.frame('cruise' = cruise, 'topbott' = c(200, 125, 175, 150, 150, 150, 85, 850, 950, 800, 950, 900, 1000, 1050))
colors <- c('red', 'mistyrose', 'lightcyan', 'grey90', 'lightcyan', 'lightcyan', 'lightcyan')

pdf('odzthickness.pdf', width = 10, height = 6)
# par(oma = c(1, 1, 1, 1))
boxplot(odz$topbott ~ odz$cruise, ylim = c(1100, 0), yaxs = 'i', medcol = colors, col = colors, boxcol = 'white', staplecol = 'white', names = c('TGT001\n 1965','TGT37\n 1969', 'TGT66\n 1972', 'P18N\n 1994', 'Clivar\n 2007', 'TN278\n 2012', 'SKQ\n 2016-17'), ylab = 'Depth (m)', xlab = 'Cruise', main = 'ODZ Thickness and ENSO State', text(paste("z = ", 5, sep = "")))
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend('bottom', c('Strong El Nino', 'Moderate El Nino', 'Weak El Nino', 
                     # 'Strong La Nina', 'Moderate La Nina', 'Weak La Nina', 'Neither'),
       # fill = c('red', 'plum1', 'mistyrose', 'royalblue1', 'skyblue1', 'lightcyan', 'grey90'), 
       # bty = 'n', xpd = TRUE, horiz = F, 
       # inset = c(0, 0, 0, 0, 0, 0, 0), text.width=c(0, 0, 0, 0, 0, 0, 0))
dev.off()

