##Convert data to .gdx-files, ready for GAMS optimization
rm(list=ls())
#dev.off()

setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/")

#Loading data 
EMAV <- read.csv("EMAV.csv")

#Packages and GAMS directory
library(gdxrrw)
library(reshape2)
library(plyr)
library(readxl)
igdx("C:/GAMS/win64/24.7/")

#Loop through 3 seeds (cfr Coupling_Data_Locations)

for (i in c(1:3)) {
        print(i)

Filename <- paste("StablesS", as.character(i),".csv", sep="")
#Filename <- paste("RandomStablesS", as.character(i),".csv", sep="")
Stables <- read.csv(Filename)


#pSourceLocation(sSource, sCoordinates)
Sources <- Stables[c("Exploitation","NIS", "Farmer", "StableType", "X", "Y",
                       "ADS", "SS")]
Sources$Nr <- paste("s", c(1:nrow(Sources)), sep="")
Sources$Exploitation <- paste("ex", Sources$Exploitation, sep="")
Sources$Farmer <- paste("f", Sources$Farmer, sep="")
Sources$NIS <- paste("n", Sources$NIS, sep="")
Sources$StableType <- gsub(" ", "_", Sources$StableType)
Sources$StableType <- gsub(".", "_", Sources$StableType, fixed=TRUE)
Sources$StableType <- gsub("-", "_", Sources$StableType, fixed=TRUE)
Sources[Sources$StableType == "",'StableType'] <- "GEEN"

#Recode exploitations and farmers
ExploitationOld <- as.character(unique(Sources$Exploitation))
ExploitationNew<- paste("e", c(1:length(ExploitationOld)), sep="")

FarmersOld<- as.character(unique(Sources$Farmer))
FarmersNew <- paste("f", c(1:length(FarmersOld)), sep="")

Sources$Exploitation <- mapvalues(Sources$Exploitation, from = ExploitationOld, to =
                                  ExploitationNew)
Sources$Farmer <- mapvalues(Sources$Farmer, from = FarmersOld, to = FarmersNew)

Location <- Sources[c("Exploitation", "X", "Y")]
Location <- unique(Location)
Location <- melt(Location, id="Exploitation")
colnames(Location) <- c("i", "j", "value")
Location$i <- as.factor(Location$i)
Location$j <- as.factor(Location$j)
attr(Location, "symName") <- "pSourceLocation"
attr(Location, "domains") <- c("sExploitation", "sCoordinates")

#pLocationImpact(sSource, sImpactScores)
LocationImpact <- Sources[c("Exploitation", "ADS", "SS")]
LocationImpact <- unique(LocationImpact)
LocationImpact <- melt(LocationImpact, id="Exploitation")
colnames(LocationImpact) <- c("i", "j", "value")
LocationImpact$i <- as.factor(LocationImpact$i)
LocationImpact$j <- as.factor(LocationImpact$j)
attr(LocationImpact, "symName") <- "pLocationImpact"
attr(LocationImpact, "domains") <- c("sExploitation", "sImpactScores")

#Multidimensional set with all id characteristics
sID <- Sources[c("Nr", "Exploitation", "NIS", "Farmer", "StableType")]
colnames(sID) <- c("i", "j", "k", "l", "m")
sID <- as.data.frame(apply(sID, 2, function(x){
        as.factor(x)
}))
attr(sID, "symName") <- "sID"
attr(sID, "domains") <- c("sStable", "sExploitation", "sNIS", "sFarmer", "sStableType")

#individual sets
Stable <- as.data.frame(unique(sID$i))
colnames(Stable) <- "i"
attr(Stable, "symName") <- "sStable"

Exploitation <- as.data.frame(unique(sID$j))
colnames(Exploitation) <- "i"
attr(Exploitation, "symName") <- "sExploitation"

NIS <- as.data.frame(unique(sID$k))
colnames(NIS) <- "i"
attr(NIS, "symName") <- "sNIS"

Farmer <- as.data.frame(unique(sID$l))
colnames(Farmer) <- "i"
attr(Farmer, "symName") <- "sFarmer"

StableType <- as.data.frame(unique(sID$m))
colnames(StableType) <- "i"
attr(StableType, "symName") <- "sStableType"

#pAnimals(sStable, sAnimalCategory)
Animals <- Stables[,6:43]
Animals$Nr <- Sources$Nr
Animals <- melt(Animals, id=c("Nr"))
colnames(Animals) <- c("i", "j","value")
Animals$i <- as.factor(Animals$i)
Animals$j <- as.factor(Animals$j)
attr(Animals, "symName") <- "pAnimals"
attr(Animals, "domains") <- c("sStable, sAnimalCategory")

AnimalCategory <- as.data.frame(unique(Animals$j))
colnames(AnimalCategory) <- "i"
attr(AnimalCategory, "symName") <- "sAnimalCategory"

#pEmissionFactor(sStableType, sAnimalCategory)
EmissionFactor<- EMAV[c("DIERCODE", "STALCODE", "Emissiefactor")]
EmissionFactor$STALCODE <- gsub(" ", "_", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- gsub(".", "_", EmissionFactor$STALCODE,  fixed = TRUE)
EmissionFactor$STALCODE <- gsub("-", "_", EmissionFactor$STALCODE, fixed= TRUE)

#Air Scrubber and "GEEN"
EF_Other <- read.csv(file = "EmissieOverig.csv")
EF_Other$Opmerking <- NULL
EmissionFactor <- rbind(EmissionFactor, EF_Other)
colnames(EmissionFactor) <- c("i", "j", "value")
EmissionFactor$i <- as.factor(EmissionFactor$i)
EmissionFactor$j <- as.factor(EmissionFactor$j)
EmissionFactor$value <- as.numeric(as.character((EmissionFactor$value)))
attr(EmissionFactor, "symName") <- "pEmissionFactor"
attr(EmissionFactor, "domains") <- c("sAnimalCategory", "sStableType")
#EmissionFactor$value[is.na(EmissionFactor$value)] <- 5.5

#Multidimensional Set sector-animalcategory
Sector <- unique(EMAV[,1:2])
colnames(Sector) <- c("i", "j")
Sector <- as.data.frame(apply(Sector, 2, function(x){
        as.factor(x)
}))
attr(Sector, "symName") <- "sSectors_AnimalCategory"
attr(Sector, "domains") <- c("sSectors", "sAnimalCategory")

#Multidimensional set with all id characteristics
sID <- Sources[c("Nr", "Exploitation", "NIS", "Farmer", "StableType")]
colnames(sID) <- c("i", "j", "k", "l", "m")
sID <- as.data.frame(apply(sID, 2, function(x){
        as.factor(x)
}))
attr(sID, "symName") <- "sID"
attr(sID, "domains") <- c("sStable", "sExploitation", "sNIS", "sFarmer", "sStableType")

#AEA-list (Ammoniak-emissie arme stalsystemen)
AEA <- data.frame(read_excel("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataEconomic/DataKWIN_Stalsystemen_PASlijst_AEAlijst.xlsx",
                    sheet=1))

AEA_Ref <- subset(AEA, Ref=="Referentie ")
AEA_Ref <- AEA_Ref[,c("VLM", "Diercat", "InvPerDP", "JaarKostDP")]
AEA_Ref <- AEA_Ref[-1,]
AEA <- AEA[-which(AEA$Ref == "Referentie "),]
AEA <- AEA[,4:9]
AEA[,3] <- NULL
AEA[,4] <- NULL
AEA <- na.omit(AEA) 

AEA_Ref <- melt(AEA_Ref, id.vars=c("VLM", "Diercat"))
AEA_Ref[is.na(AEA_Ref)] <- 0
colnames(AEA_Ref) <- c("i", "j", "k", "value")

AEA <- melt(AEA, id.vars=c("VLM", "Diercat"))
colnames(AEA) <- c("i", "j", "k", "value")
AEA$i <- as.factor(AEA$i)
AEA$j <- as.factor(AEA$j)
AEA$k <- as.factor(AEA$k)
AEA$value <- as.numeric(AEA$value)

attr(AEA, "symName") <- "pAEA"
attr(AEA, "domains") <- c("sStableType", "sAnimalCategory", "sCost")

attr(AEA_Ref, "symName") <- "pAEA_Ref"
attr(AEA_Ref, "domains") <- c("sStableType", "sAnimalCategory", "sCost")

AEA_Ref$i <- as.factor(AEA_Ref$i)
AEA_Ref$j <- as.factor(AEA_Ref$j)
AEA_Ref$k <- as.factor(AEA_Ref$k)
AEA_Ref$value <- as.numeric(AEA_Ref$value)


#Bundling all data in gdx-file
Directoryname <- paste("C:/Users/ddpue/Documents/Spatial Optimization Flanders/GAMS/FarmsFlanders", 
                       as.character(i), ".gdx", sep="")
wgdx.lst(Directoryname,  Location, LocationImpact, sID, Stable, Exploitation,
                AnimalCategory, NIS, Farmer, StableType,
                   Animals, EmissionFactor, Sector, AEA, AEA_Ref)

}
