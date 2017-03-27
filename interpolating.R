#package: reshape2
?melt
cruisesALL1 <- subset(cruisesALL0, select = -c(dateNew, timeNew, datetime, lat, long, press))
head(cruisesALL1)

cruisesALLm <- data.frame(melt(cruisesALL1, id.vars = c('cruise', 'station', 'depth')))
head(cruisesALLm)
#undo melt
cruisesALLc <- dcast(cruisesALLm, cruise + station + depth ~ variable, mean)
head(cruisesALLc)

?plyr
#subset out a practice cast
#try approx()
#ddply()

practice <- cruisesALLm[cruisesALLm$station == '65' & cruisesALLm$cruise == 'TGT001',]
head(practice)
by5 <- seq(0, 100, 5)
by25 <- seq(125, 500, 25)
by50 <- seq(550, 2000, 50)
by100 <- seq(2100, 9000, 100)
stdDepths <- c(by5, by25, by50, by100)
stdDepths

length(cruisesALL1$cruise)
?approx
approx(xy.coords(cruisesALL1$depth, cruisesALL1$O2), xout = stdDepths, method = 'linear')
#i think its working..
#package: plyr
?approx
?ddply
head(cruisesALLm)

interp <- ddply(.data = cruisesALLn, .variables = c('cruise', 'station', 'variable'), .fun = approx(xy.coords(df$depth, df$value)), .inform = TRUE)

cruisesALLn <- na.omit(cruisesALLm)

interp <- ddply(.data = cruisesALLn, .variables = c('cruise', 'station', 'variable'), function(df) approx(xy.coords(df$depth, df$value)), .inform = TRUE)

?approx
interp
?try

options(error = NULL)
nrow(cruisesALLm)

approxR <- function(df) {
  if (nrow(df) >= 2) {
    approx(xy.coords(df$depth, df$value))
  }
  else { 
    return('err')
  }
}

interp <- ddply(.data = cruisesALLn, .variables = c('cruise', 'station', 'variable'), function(df) approxR(df), .inform = TRUE)


unique(cruisesALL0$station)
#ugh what is this error...
#renaming 034a to 034.. jk idk how
head(cruisesALLm)

interp <- ddply(.data = cruisesALLm, .variables = c(cruise, station, depth), .fun = approx(xy.coords(depth, value)))
#AHHH :(

write.csv(cruisesALLm, "cruisesALLm.csv")

