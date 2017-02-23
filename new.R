cruise <- read.csv(file = "TN278_CTD_Bottle.csv", header = T)


lengths <- data[data$Gender == gender,]$Lengths #THIS IS HOW YOU SUBSET
