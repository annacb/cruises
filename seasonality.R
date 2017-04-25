#seasonality for O2..
#first subset out the "avg__" data
#then select the cruises for winter/summer
#winter: TGT37, P18N
#summer: TGT001, TGT66, clivar, TN278, HOMZ16


winterO2 <- subset(avgO2, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerO2 <- subset(avgO2, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

#now we need to average all the values for winter and then for summer

avgwinterO2 <- ddply(.data = winterO2, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerO2 <- ddply(.data = summerO2, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterO2, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerO2, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[O2] umol', limits = c(0, 250)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Oxygen Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#PHOSPHATE
winterPO4 <- subset(avgPO4, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerPO4 <- subset(avgPO4, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwinterPO4 <- ddply(.data = winterPO4, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerPO4 <- ddply(.data = summerPO4, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterPO4, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerPO4, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[PO4] umol', limits = c(0, 4)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Phosphate Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#SILICATE
winterSiO4 <- subset(avgSiO4, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerSiO4 <- subset(avgSiO4, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwinterSiO4 <- ddply(.data = winterSiO4, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerSiO4 <- ddply(.data = summerSiO4, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterSiO4, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerSiO4, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[SiO4] umol', limits = c(0, 175)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Silicate Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#NITRITE
winterNO2 <- subset(avgNO2, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerNO2 <- subset(avgNO2, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwinterNO2 <- ddply(.data = winterNO2, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerNO2 <- ddply(.data = summerNO2, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterNO2, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerNO2, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO2] umol', limits = c(0, 8)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Nitrite Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#NITRATE
winterNO3 <- subset(avgNO3, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerNO3 <- subset(avgNO3, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwinterNO3 <- ddply(.data = winterNO3, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerNO3 <- ddply(.data = summerNO3, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterNO3, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerNO3, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO3] umol', limits = c(0, 50)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Nitrate Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#AMMONIUM
winterNH4 <- subset(avgNH4, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summerNH4 <- subset(avgNH4, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwinterNH4 <- ddply(.data = winterNH4, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummerNH4 <- ddply(.data = summerNH4, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwinterNH4, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummerNH4, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NH4] umol', limits = c(0, 1.5)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Ammonium Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#TEMP
wintertemp <- subset(avgtemp, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summertemp <- subset(avgtemp, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwintertemp <- ddply(.data = wintertemp, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummertemp <- ddply(.data = summertemp, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwintertemp, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummertemp, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Temp (˚C)', limits = c(0, 30)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Temperature Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#SAL
wintersal <- subset(avgsal, cruise == 'TGT001' | cruise == 'TGT66' | cruise == 'clivar' | cruise == 'TN278' | cruise == 'HOMZ16', select = c(cruise, variable, DepthBin, V1))

summersal <- subset(avgsal, cruise == 'TGT27' | cruise == 'P18N', select = c(cruise, variable, DepthBin, V1))

avgwintersal <- ddply(.data = wintersal, .variables = c('DepthBin'), function(df) mean(df$V1))

avgsummersal <- ddply(.data = summersal, .variables = c('DepthBin'), function(df) mean(df$V1))

ggplot() + geom_line(data = avgwintersal, aes(DepthBin, V1), color = 'blue', lwd = 0.75) + geom_line(data = avgsummersal, aes(DepthBin, V1), color = 'hotpink', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Salinity (PSU)', limits = c(34, 35)) + coord_flip() + ggtitle("ETNP Avgerage Seasonal Temperature Profile") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))




#
#
#
#
#
#
#
#


#now i need to combine winter and summer into a data frame, then create a new column that has the difference of winter and summer values

#OXYGEN
wsO2 <- data.frame(avgwinterO2[3:84, ], avgsummerO2$V1)
diffO2 <- transform(wsO2, new.col = V1 - avgsummerO2.V1)

ggplot() + geom_line(data = diffO2, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[O2] (umol)', limits = c(-5, 5)) + coord_flip() + ggtitle("Winter vs Summer Differences in Oxygen") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#PHOSPHATE
wsPO4 <- data.frame(avgwinterPO4[3:84, ], avgsummerPO4$V1)
diffPO4 <- transform(wsPO4, new.col = V1 - avgsummerPO4.V1)

ggplot() + geom_line(data = diffPO4, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[PO4] (umol)', limits = c(-0.25, 0.75)) + coord_flip() + ggtitle("Winter vs Summer Differences in Phosphate") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#SILICATE
wsSiO4 <- data.frame(avgwinterSiO4[3:84, ], avgsummerSiO4$V1)
diffSiO4 <- transform(wsSiO4, new.col = V1 - avgsummerSiO4.V1)

ggplot() + geom_line(data = diffSiO4, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[SiO4] (umol)', limits = c(-10, 10)) + coord_flip() + ggtitle("Winter vs Summer Differences in Silicate") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#NITRITE
wsNO2 <- data.frame(avgwinterNO2[3:84, ], avgsummerNO2$V1)
diffNO2 <- transform(wsNO2, new.col = V1 - avgsummerNO2.V1)

ggplot() + geom_line(data = diffNO2, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO2] (umol)', limits = c(-1, 2)) + coord_flip() + ggtitle("Winter vs Summer Differences in Nitrite") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#NITRATE
wsNO3 <- data.frame(avgwinterNO3[3:84, ], avgsummerNO3$V1)
diffNO3 <- transform(wsNO3, new.col = V1 - avgsummerNO3.V1)

ggplot() + geom_line(data = diffNO3, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = '[NO3] (umol)', limits = c(-5, 10)) + coord_flip() + ggtitle("Winter vs Summer Differences in Nitrate") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#AMMONIUM

#not doing ammonium because SUMMER ammonium data DOESNT EXIST! all cruises happened during 'winter' months (oct - march) (TN278 and HOMZ16)


#TEMP
wstemp <- data.frame(avgwintertemp[3:84, ], avgsummertemp$V1)
difftemp <- transform(wstemp, new.col = V1 - avgsummertemp.V1)

ggplot() + geom_line(data = difftemp, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Temp (˚C)', limits = c(-3, 2)) + coord_flip() + ggtitle("Winter vs Summer Differences in Temperature") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))


#SALINITY
wssal <- data.frame(avgwintersal[3:84, ], avgsummersal$V1)
diffsal <- transform(wssal, new.col = V1 - avgsummersal.V1)

ggplot() + geom_line(data = diffsal, aes(DepthBin, new.col), color = 'black', lwd = 0.75) + scale_x_reverse(limits = c(1500, 0), name = 'Depth (m)') + scale_y_continuous(name = 'Salinity (PSU)', limits = c(-0.2, 0.3)) + coord_flip() + ggtitle("Winter vs Summer Differences in Salinity") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))




#
#
#
#
#
#
#
#


#now i need to go back to original summer and winter depth profiles and subtract out this new "difference" that i calculated
#RS = removed seasonality

diffO2
RSwinterO2 <- subset(transform(diffO2, V1 = V1 - new.col, avgsummerO2.V1 = avgsummerO2.V1 - new.col), select = -c(new.col))

