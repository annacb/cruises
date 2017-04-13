ODZdepthfx <- function(df) {
  odzIdx <- which(df$V1 <= 10)
  firstIdx <- odzIdx[1]
  topODZ <- df$DepthBin[firstIdx]
  lastIdx <- odzIdx[length(odzIdx)]
  bottODZ <- df$DepthBin[lastIdx]
  data.frame(cruise = df[1, 1], topODZ, bottODZ)
}

avgPO4

PO4depthfx <- function(df) {
  Idx <- which(df$V1 >= 2.5)
  firstIdx <- Idx[1]
  topIdx <- df$DepthBin[firstIdx]
  data.frame(cruise = df[1, 1], topIdx)
}


NO2depth <- ddply(.data = avgNO2, .variables = c('cruise'), function(df) NO2depthfx(df))




#OXYGEN
avgO2 <- ddply(.data = O2data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

ODZdepths <- ddply(.data = avgO2, .variables = c('cruise'), function(df) ODZdepthfx(df))

hlineO2 <- data.frame(z = c(ODZdepths$topODZ, ODZdepths$bottODZ), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16', 'TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))


ggplot() + geom_line(aes(DepthBin, value, group = station), data = O2data, col = 'skyblue1') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgO2, aes(DepthBin, V1), color = 'darkblue', lwd = 0.75) + geom_vline(aes(xintercept = z), hlineO2, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[O2] umol', limits = c(0, 250)) + coord_flip() + ggtitle("ETNP Oxygen Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 



#PHOSPHATE
avgPO4 <- ddply(.data = PO4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlinePO4 <- data.frame(z = c(PO4depth$topIdx), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot(rbind(PO4data, jacdummy), aes(DepthBin, value, group = 'station')) + geom_line(color = 'pink') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgPO4, aes(DepthBin, V1), color = 'violetred1', lwd = 0.75) + geom_vline(aes(xintercept = z), vlinePO4, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[PO4] umol', limits = c(0, 4)) + ggtitle("ETNP Phosphate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 


#SILICATE
avgSiO4 <- ddply(.data = SiO4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlineSiO4 <- data.frame(z = c(300, 130, 120, 135, 110, 95), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot(rbind(SiO4data, jacdummy), aes(DepthBin, value, group = 'station')) + geom_line(color = 'peachpuff2') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgSiO4, aes(DepthBin, V1), color = 'tan4', lwd = 0.75) + geom_vline(aes(xintercept = z), vlineSiO4, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[SiO4] umol', limits = c(0, 175)) + ggtitle("ETNP Silicate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 


#NITRITE
avgNO2 <- ddply(.data = NO2data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlineNO2 <- data.frame(z = c(NO2depth$topNO2, NO2depth$bottNO2), cruise = c('TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot(rbind(NO2data, jacdummy), aes(DepthBin, value, group = 'station')) + geom_line(color = 'yellowgreen') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNO2, aes(DepthBin, V1), color = 'springgreen4', lwd = 0.75) + geom_vline(aes(xintercept = z), vlineNO2, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO2] umol', limits = c(0, 8)) + ggtitle("ETNP Nitrite Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 


#NITRATE
avgNO3 <- ddply(.data = NO3data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlineNO3 <- data.frame(z = c(115, 125, 115, 135, 150, 300), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot(rbind(NO3data, jacdummy), aes(DepthBin, value, group = 'station')) + geom_line(color = 'thistle3') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNO3, aes(DepthBin, V1), color = 'thistle4', lwd = 0.75) + geom_vline(aes(xintercept = z), vlineNO3, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO3] umol', limits = c(0, 50)) + ggtitle("ETNP Nitrate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 


#AMMONIUM
avgNH4 <- ddply(.data = NH4data, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlineNH4 <- data.frame(z = c(30), cruise = c('TN278', 'HOMZ16'))

ggplot(rbind(NH4data, jacdummy), aes(DepthBin, value, group = 'station')) + geom_line(color = 'sandybrown') + coord_flip() + facet_wrap('cruise', nrow = 2) + geom_line(data = avgNH4, aes(DepthBin, V1), color = 'darkorange2', lwd = 0.75) + geom_vline(aes(xintercept = z), vlineNH4, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NH4] umol', limits = c(0, 1.5)) + ggtitle("ETNP Ammonium Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 


#TEMP
avgtemp <- ddply(.data = tempdata, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlinetemp <- data.frame(z = c(105, 115, 100, 105, 95, 90, 100), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot((tempdata), aes(DepthBin, value, group = 'station')) + geom_line(color = 'tomato') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgtemp, aes(DepthBin, V1), color = 'darkred', lwd = 0.75) + geom_vline(aes(xintercept = z), vlinetemp, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Temp (˚C)', limits = c(0, 30)) + coord_flip() + ggtitle("ETNP Water Temperature 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#SAL
avgsal <- ddply(.data = saldata, .variables = c('cruise', 'variable', 'DepthBin'), function(df) mean(df$value))

vlinesal <- data.frame(z = c(95, 100, 55, 110, 95, 80, 70), cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N', 'clivar', 'TN278', 'HOMZ16'))

ggplot((saldata), aes(DepthBin, value, group = 'station')) + geom_line(color = 'grey') + facet_wrap('cruise', nrow = 2) + geom_line(data = avgsal, aes(DepthBin, V1), color = 'grey33', lwd = 0.75) + geom_vline(aes(xintercept = z), vlinesal, lty = 5) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Salinity (PSU)', limits = c(33, 35)) + coord_flip() + ggtitle("ETNP Salinity 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


