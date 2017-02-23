# library(readr)
# allOMZcruises <- read_csv("~/R files/plots/allOMZcruises.csv", 
                         # col_types = cols(dateNew = col_date(format = "%d-%b-%Y"),
                                           # timeNew = col_time(format = "%H:%M")))
# View(allOMZcruises)

head(allOMZcruises)
cruisesDataFrame <- data.frame(allOMZcruises)
cruises <- subset(cruisesDataFrame, select = -c(date, time, bottDepth))
head(cruises)
View(cruises)


#FIXED allOMZcruises TO INCLUDE 2012 DEPTH!!! 1 db = 1 m ###########
cruises[1828,] #looking for start of TN278 cruise (2012 cruise)
cruises2 <- cruises[1:1827, ] #getting rid of 2012 cruise since i want to plot against depth, but there is no depth for that cruise :( 
nrow(cruises2) == 4936 - 3109
head(cruises2)

cruises[4336,]
cruises[4475,] #looking for 2016 cruise
cruises3 <- cruises[4336:4475, ]
cruises4 <- rbind(cruises2, cruises3)

View(cruises4)
nrow(cruises3) # =140
length(cruises4$cruise) == (4936 - 3109) + 140
##################################################################


# ADDING IN 2016 TEMP/SAL/... DATA ###############################
cruises2016df <- data.frame(skq2016combined)
cruises16 <- subset(cruises2016df, select = -c(date, time, bottDepth))
cruisesALL <- rbind(cruises, cruises16)  
attach(cruisesALL)
View(cruises16)
View(cruisesALL)
##################################################################


#convert from press -> meters
?teos

#trying to make ggplot plot in the order i want... YAY! it works :)
cruisesALL$cruise <- factor(cruisesALL$cruise, levels=unique(cruisesALL$cruise))
head(cruisesALL)

str(cruises)
levels(cruises$cruise)
pdf("all but 2016 v2.pdf", width=7, height=5) 
#OXYGEN
#O2log <- log10(O2) # + scale_y_log10() +

# ggplot(cruisesALL, aes(depth, O2, group = station)) + geom_line(color = 'lightslateblue') + coord_flip() + facet_wrap('cruise', nrow = 5) + scale_x_reverse() + xlim(1500, 0)

#this is now you get rid of NA plot
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, O2, group = station)) + geom_line(color = 'lightslateblue') + coord_flip() + facet_wrap('cruise', nrow = 5) + scale_x_reverse() + xlim(1500, 0)

#PHOSPHATE
ggplot(subset(cruises, !is.na(cruise)), aes(depth, PO4, group = station)) + geom_line(color = 'darkorchid1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse()

#SILICATE
ggplot(subset(cruises, !is.na(cruise)), aes(depth, SiO4, group = station)) + geom_line(color = 'azure3') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse()

#NITRITE
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, NO2, group = station)) + geom_line(color = 'darkorange3') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + xlim(1000, 0)

#NITRATE
ggplot(subset(cruises, !is.na(cruise)), aes(depth, NO3, group = station)) + geom_line(color = 'goldenrod1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse() + xlim(1000, 0)

#AMMONIUM
ggplot(subset(cruises, !is.na(cruise)), aes(depth, NH4, group = station)) + geom_line(color = 'chartreuse1') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse()

#TEMP
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, temp, group = station)) + geom_line(color = 'yellowgreen') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse()

#SALINITY
ggplot(subset(cruisesALL, !is.na(cruise)), aes(depth, sal, group = station)) + geom_line(color = 'slateblue') + coord_flip() + facet_wrap('cruise', nrow = 2) + scale_x_reverse()

dev.off()


#bit bucket or github respository
#if not working, put in zip file and send to jacob!
