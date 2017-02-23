cruisesRaw <- read.csv("allOMZcruises.csv")
head(cruisesRaw)
cruises <- subset(cruisesRaw, select = -c(date, time, bottDepth))
head(cruises)


timeinator <- function (date, time) {
  if(grepl("/", date)) {
    if(grepl(":", time)) {
    type <- "slash colon"
    datetime <- as.POSIXct(paste(date, time, sep = 'T'), format = '%m/%d/%yT%I:%M:%S')
    }
    else if(is.na(time)){
      type <- "slash NA"
      datetime <- as.POSIXct(date, format = '%m/%d/%y')
    }
    else {
      type <- "slash decimal day"
      #? datetime <- as.POSIXct(paste(date, time, sep = 'T'), format = '%m/%d/%yT%I')
    }

  }
  else{
    if(grepl("-", date)){
      type <- "dash colon"
      datetime <- as.POSIXct(paste(date, time, sep = 'T'), format = '%Y-%m-%dT%H:%M:%S')
    }
    else{
      type <- "no slash dash"
      datetime <- as.POSIXct(paste(date, time, sep = 'T'), format = '%Y%m%dT%H%M')
    }
  }
  return(type)
  return(datetime)
}


#take datetime <- as.POSIXct(paste(date, time, sep = 'T'), format = '%Y%m%dT%H%M') and change the format of %.. for each if/else statement


date <- "2005-05-01T05:05:05.000"

date <- "20010101"
time <- "0101"

date <- "2/23/72"
time <- "0.55833"

date <- "12/6/65"

date <- '2/24/72'
time <- '1:24:00' #maybe i should just change these all to the format that i want.....
datetime


?as.POSIXct

timeinator("05/05/2000", NA)
timeinator("05/05/2000", 0)
timeinator("05/05/2000", "05:05:05")
timeinator("05-05-2000", NA)
timeinator("05052000", 050505)

grepl("/", cruises$date)


