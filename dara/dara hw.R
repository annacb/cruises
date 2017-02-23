Dara<-read.csv(file="darasData.csv", header = T)
head(Dara)

#spp different for Carbon?
(sppC <- aov(data=Dara, d13C~Genus_Species))
summary(sppC)
tukeyAllData <- TukeyHSD(sppC)
tukeySpecies <- data.frame(tukeyAllData$Genus_Species)
tukeySpeciesP <- tukeySpecies["p.adj"]
tukeyLogical <- tukeySpeciesP$p.adj < 0.05 
tukeySpecies[tukeyLogical,] 

#does C vary by spp AND season? additive
sppCandS<-aov(data=Dara, d13C~Genus_Species+Season)
summary(sppCandS)

tukeyAllDataCA <- TukeyHSD(sppCandS)
tukeySpeciesCA <- data.frame(tukeyAllDataCA$Genus_Species)
tukeySpeciesCAP <- tukeySpecies["p.adj"]
tukeyLogicalCA <- tukeySpeciesCAP$p.adj < 0.05 
tukeySpeciesCA[tukeyLogicalCA,] 

#does C vary by spp AND season? interaction
sppCandSinteraction<-aov(data=Dara, d13C~Genus_Species*Season)
summary(sppCandSinteraction)

tukeyAllDataCI <- TukeyHSD(sppCandSinteraction)
tukeySpeciesCI <- data.frame(tukeyAllDataCI$Genus_Species)
tukeySpeciesCIP <- tukeySpecies["p.adj"]
tukeyLogicalCI <- tukeySpeciesCIP$p.adj < 0.05 
tukeySpeciesCI[tukeyLogicalCI,] 


#spp different for Nitrogen?
sppN<-aov(data=Dara, d15N~Genus_Species)
summary(sppN)

tukeyAllDataN <- TukeyHSD(sppN)
tukeySpeciesN <- data.frame(tukeyAllDataN$Genus_Species)
tukeySpeciesNP <- tukeySpecies["p.adj"]
tukeyLogicalN <- tukeySpeciesNP$p.adj < 0.05 
tukeySpeciesN[tukeyLogicalN,]

#N different for season?
seasonN<-aov(data=Dara, d15N~Season)
summary(seasonN)

TukeyHSD(seasonN)


#spp different for N, by season?
sppNandS<-aov(data=Dara, d15N~Genus_Species+Season)
summary(sppNandS)

TukeyHSD(sppNandS)



#MANOVA by season
isotopes <- cbind(Dara$d13C, Dara$d15N)
NandCSeason <- manova(isotopes ~ Dara$Season)
summary(NandCSeason, test="Pillai")


summary.aov(NandCSeason)

#MANOVA by speceis
isotopes <- cbind(Dara$d13C, Dara$d15N)
NandCSpp <- manova(isotopes ~ Dara$Genus_Species)
summary(NandCSpp, test="Pillai")


summary.aov(NandCSpp)



#create boxplot



boxplot(data=Dara, d13C~Genus_Species, col = 0:9) 
legendNames <- levels(Dara$Genus_Species)
legend(x="bottomleft", legend=legendNames, pt.bg=0:9, pch=22, bty = "n")



boxplot(data=Dara, d15N~Genus_Species, col = 0:9) 
legendNames <- levels(Dara$Genus_Species)
legend(x="bottomleft", legend=legendNames, pt.bg=0:9, pch=22, bty = "n")





nitrogenWetLogical <- Dara$Season == "Wet"
nitrogenWetData <- Dara[nitrogenWetLogical,]

nitrogenDryLogical <- Dara$Season == "Dry"
nitrogenDryData <- Dara[nitrogenDryLogical,]

par(mfcol = c(1,2))
par(mar=c(0,0,0,0),oma=c(5,5,1,1))
boxplot(nitrogenWetData$d15N~nitrogenWetData$Genus_Species , col = 0:10, xaxt = "n")
mtext(text="Nitrogen",side=2,line=3) 
mtext(text="Species during Wet Season",side=1,line=3)
boxplot(nitrogenDryData$d15N~nitrogenDryData$Genus_Species, col = 0:10, xaxt ='n', yaxt = "n") 
mtext(text="Species during Dry Season",side=1,line=3)
legend(x="bottomleft", legend=legendNames, pt.bg = 0:10, pch=22, bty = "n")
title("Nitrogen in Species for Wet and Dry Seasons", outer=TRUE)

?plot
