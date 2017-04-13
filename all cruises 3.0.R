### (OLD) CLEANED UP VERSION OF PLOTS

head(allOMZcruises)
cruisesDataFrame <- data.frame(allOMZcruises)
cruises <- subset(cruisesDataFrame, select = -c(date, time, bottDepth))
head(skq2016combined)
cruises16 <- subset(skq2016combined, select = -c(date, time, bottDepth))
cruisesALL0 <- rbind(cruises, cruises16)  
head(cruisesALL0)
attach(cruisesALL0)
cruisesALL0$cruise <- factor(cruisesALL0$cruise, levels=unique(cruisesALL0$cruise))
cruises$cruise <- factor(cruises$cruise, levels=unique(cruises$cruise))


#adding horizontal line........
O2min <- data.frame(cruise = c('TGT001', 'TGT37', 'TGT66', 'P18N_31DSCG94_3', 'clivar', 'TN278', 'HOMZ16'), O2 = c(200, 180, 195, 200, 200, 200, 170))
p + geom_vline(aes(xintercept = O2), O2min)
######################################

pdf("all but 2016 v6.pdf", width=7, height=5) 
#OXYGEN 
ggplot(subset(cruisesALL0, !is.na(cruise)), aes(depth, O2, group = station)) + geom_line(color = 'lightslateblue') + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[O2] umol', limits = c(0, 250)) + coord_flip() + ggtitle("ETNP Oxygen Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#PHOSPHATE
cruisesALL0$PO4 <- as.numeric(cruisesALL0$PO4) 
ggplot(rbind(subset(cruisesALL0, !is.na(cruise) & !is.na(PO4)), jacdummy), aes(depth, PO4, group = station)) + geom_line(color = 'darkorchid1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[PO4] umol', limits = c(0, 4)) + ggtitle("ETNP Phosphate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) 

#SILICATE
cruisesALL0$SiO4 <- as.numeric(cruisesALL0$SiO4) 
ggplot(rbind(subset(cruisesALL0, !is.na(cruise) & !is.na(SiO4)), jacdummy), aes(depth, SiO4, group = station)) + geom_line(color = 'azure3') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[SiO4] umol', limits = c(0, 175)) + ggtitle("ETNP Silicate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#NITRITE
cruisesALL0$NO2 <- as.numeric(cruisesALL0$NO2) 
ggplot(rbind(subset(cruisesALL0, !is.na(cruise) & !is.na(NO2)), jacdummy), aes(depth, NO2, group = station)) + geom_line(color = 'darkorange3') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[N02] umol', limits = c(0, 8)) + ggtitle("ETNP Nitrite Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#NITRATE
cruisesALL0$NO3 <- as.numeric(cruisesALL0$NO3) 
ggplot(subset(cruisesALL0, !is.na(cruise)), aes(depth, NO3, group = station)) + geom_line(color = 'goldenrod1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[N03] umol', limits = c(0, 50)) + ggtitle("ETNP Nitrate Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#AMMONIUM
cruisesALL0$NH4 <- as.numeric(cruisesALL0$NH4) 
ggplot(rbind(subset(cruisesALL0, !is.na(cruise) & !is.na(NH4)), jacdummy), aes(depth, NH4, group = station)) + geom_line(color = 'chartreuse1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + scale_y_continuous(name = '[NH4] umol', limits = c(0, 1.5)) + ggtitle("ETNP Ammonium Concentration 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15))

#### DONT PLOT THIS ### AMMONIUM, just stations with stuff
# ggplot(subset(cruisesALL0, !is.na(cruise) & !is.na(NH4)), aes(depth, NH4, group = station)) + geom_line(color = 'chartreuse1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() 

#TEMP
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, temp, group = station)) + geom_line(color = 'yellowgreen') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + ggtitle("ETNP Water Temperature 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) + scale_y_continuous(name = 'temp (˚C)', limits = c(0, 30))

#SALINITY
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, sal, group = station)) + geom_line(color = 'slateblue') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + ggtitle("ETNP Salinity 1965 to 2017") + theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 15)) + scale_y_continuous(name = 'salinity (PSU)', limits = c(33, 35)) 

dev.off()

#thank you jacob
jacdummy <- cruisesALL0[1:length(levels(cruisesALL0$cruise)),]
jacdummy[,] <- NA
jacdummy$cruise <- levels(cruisesALL0$cruise)

