##Convert data to .gdx-files, ready for GAMS optimization
rm(list=ls())
#dev.off()

setwd("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataHandling_VLM/")

#Loading data 
EMAV <- read.csv("EMAV.csv")

#Packages and GAMS directory
library(gdxrrw)
library(reshape2)
library(plyr)
library(readxl)
igdx("C:/GAMS/win64/24.6/")

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
Sources[Sources$StableType == "GEEN",'StableType'] <- "UNKNOWN"
Sources[Sources$StableType == "",'StableType'] <- "UNKNOWN"


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
EF_Other <- read.csv("EmissieOverig.csv")
EF_Other$Opmerking <- NULL
EmissionFactor <- rbind(EmissionFactor, EF_Other)
EmissionFactor$STALCODE[EmissionFactor$STALCODE == "GEEN"] <- "UNKNOWN"
EmissionFactor$STALCODE <- sub("S_1_MENGM", "S_1", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- sub("S_1_STALM", "S_1", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- sub("S_2_MENGM", "S_2", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- sub("S_2_STALM", "S_2", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- sub("S_3_MENGM", "S_3", EmissionFactor$STALCODE)
EmissionFactor$STALCODE <- sub("S_3_STALM", "S_3", EmissionFactor$STALCODE)
EmissionFactor <- unique(EmissionFactor)

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
sID$StableType <- sub("S_1_MENGM", "S_1", sID$StableType)
sID$StableType <- sub("S_1_STALM", "S_1", sID$StableType)
sID$StableType <- sub("S_2_MENGM", "S_2", sID$StableType)
sID$StableType <- sub("S_2_STALM", "S_2", sID$StableType)
sID$StableType <- sub("S_3_MENGM", "S_3", sID$StableType)
sID$StableType <- sub("S_3_STALM", "S_3", sID$StableType)
colnames(sID) <- c("i", "j", "k", "l", "m")
sID <- as.data.frame(apply(sID, 2, function(x){
        as.factor(x)
}))
attr(sID, "symName") <- "sID"
attr(sID, "domains") <- c("sStable", "sExploitation", "sNIS", "sFarmer", "sStableType")

StableType <- as.data.frame(unique(sID$m))
colnames(StableType) <- "i"
attr(StableType, "symName") <- "sStableType"

#AEA-list (Ammoniak-emissie arme stalsystemen)
AEAcost <- data.frame(read_excel("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/AEA.xlsx",
                    sheet=1))


AEAcost <- melt(AEAcost, id.vars=c("sAnimalCategory", "sStableType"))

colnames(AEAcost) <- c("i", "j", "k", "value")

colnames(AEAcost) <- c("i", "j", "k", "value")
AEAcost$i <- as.factor(AEAcost$i)
AEAcost$j <- as.factor(AEAcost$j)
AEAcost$k <- as.factor(AEAcost$k)
AEAcost$value <- as.numeric(AEAcost$value)


attr(AEAcost, "symName") <- "pAEAcost"
attr(AEAcost, "domains") <- c("sAnimalCategory", "sStableType",  "sCost")

AEAextracost <- data.frame(read_excel("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/AEA.xlsx",
                                      sheet=2))
#AEAextracost[AEAextracost == 0] <- 10E-15
AEAextracost <- melt(AEAextracost, id.vars=c("sAnimalCategory", "sStableType"))
 
colnames(AEAextracost) <- c("i", "j", "k", "value")

colnames(AEAextracost) <- c("i", "j", "k", "value")
AEAextracost$i <- as.factor(AEAextracost$i)
AEAextracost$j <- as.factor(AEAextracost$j)
AEAextracost$k <- as.factor(AEAextracost$k)
AEAextracost$value <- as.numeric(AEAextracost$value)


attr(AEAextracost, "symName") <- "pAEAextracost"
attr(AEAextracost, "domains") <- c("sAnimalCategory", "sStableType",  "sCost")

#PAS-list (Emissiebeperkende technieken)
PAS <-  data.frame(read_excel("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/DataKWIN_Stalsystemen_PASlijst_AEAlijst.xlsx",
                              sheet=2))

PAS <- PAS[,c(4,5,7,8,10)]
PAS <- na.omit(PAS)

ReductionEFPAS <- PAS[,c(1:3)]
colnames(ReductionEFPAS) <- c("i", "j", "value")
ReductionEFPAS$i <- as.factor(ReductionEFPAS$i)
ReductionEFPAS$j <- as.factor(ReductionEFPAS$j)
attr(ReductionEFPAS, "symName") <- "pReductionEFPAS"
attr(ReductionEFPAS, "domains") <- c("sPAS", "sAnimalCategory")

sPAS <- as.data.frame(unique(ReductionEFPAS$i))
colnames(sPAS) <- "i"
attr(sPAS, "symName") <- "sPAS"

CostPAS <- PAS[,c(1:2,4:5)] 
CostPAS <- melt(CostPAS, id.vars=c("PAS", "Diercat"))


colnames(CostPAS) <- c("i", "j", "k", "value")
CostPAS$i <- as.factor(CostPAS$i)
CostPAS$j <- as.factor(CostPAS$j)
CostPAS$k <- as.factor(CostPAS$k)
attr(CostPAS, "symName") <-"pCostPAS"
attr(CostPAS, "domains") <- c("sPAS", "sAnimalCategory", "sCost")

#GrossMargins
GM <- data.frame(read_excel("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/GrossMargins.xlsx",
                                    sheet=1))

GM <- GM[,-(6:8)]

GM <- melt(GM, id.vars = c("FarmType", "jaarplaats", "Diercat"))
GM$jaarplaats <- NULL
colnames(GM) <- c("i", "j", "k", "value")
GM$i <- as.factor(GM$i)
GM$j <- as.factor(GM$j)
GM$k <- as.factor(GM$k)
attr(GM, "symName") <- "pGM"
attr(GM, "domains") <- c("sFarmType", "sAnimalCategory", "sStatistic")

sFarmType <- as.data.frame(unique(GM$i))
colnames(sFarmType) <- "i"
attr(sFarmType, "symName") <- "sFarmType"

#Bundling all data in gdx-file
Directoryname <- paste("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/GAMS/FarmsFlanders", 
                       as.character(i), ".gdx", sep="")
wgdx.lst(Directoryname,  Location, LocationImpact, sID, Stable, Exploitation,
                AnimalCategory, NIS, Farmer, StableType,
                   Animals, EmissionFactor, Sector, AEAcost, AEAextracost, ReductionEFPAS, CostPAS, GM,
         sPAS, sFarmType, squeeze=FALSE)

}
