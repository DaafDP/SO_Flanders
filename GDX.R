##Convert data to .gdx-files, ready for GAMS optimization
rm(list=ls())
#dev.off()

#Loading data (pick seed 1 or 2)
EMAV <- read.csv("EMAV.csv")
Stables <- read.csv("StablesS1.csv")
#Stables <- read.csv("StablesS2.csv")

#Packages and GAMS directory
library(gdxrrw)
library(reshape2)
igdx("C:/GAMS/win64/24.7/")

#pSourceLocation(sSource, sCoordinates)
Sources <- Stables[c("ID", "Exploitation","NIS", "Farmer", "StableType", "X", "Y",
                       "ADS", "SS")]

Sources$Nr <- paste("s", Sources$ID, sep="")
Sources$StableType <- gsub(" ", "_", Sources$StableType)
Sources$StableType <- gsub(".", "_", Sources$StableType, fixed=TRUE)
Sources$StableType <- gsub("-", "_", Sources$StableType, fixed=TRUE)


SourceLocation <- Sources[c("Nr", "X", "Y")]
SourceLocation <- melt(SourceLocation)
colnames(SourceLocation) <- c("i", "j", "value")
SourceLocation$i <- as.factor(SourceLocation$i)
SourceLocation$j <- as.factor(SourceLocation$j)
attr(SourceLocation, "symName") <- "pSourceLocation"
attr(SourceLocation, "domains") <- c("sSource", "sCoordinates")

#pSourceImpact(sSource, sImpactScores )
SourceImpact <- Sources[c("Nr", "ADS", "SS")]
SourceImpact <- melt(SourceImpact)
colnames(SourceImpact) <- c("i", "j", "value")
SourceImpact$i <- as.factor(SourceImpact$i)
SourceImpact$j <- as.factor(SourceImpact$j)
attr(SourceImpact, "symName") <- "pSourceImpact"
attr(SourceImpact, "domains") <- c("sSource", "sImpactScores")

#pSourceID(sSource, sStableType, sID)
SourceID <- Sources[c("Nr", "Exploitation", "NIS", "Farmer", "StableType")]
SourceID <- melt(SourceID, id =c("Nr", "StableType"))
colnames(SourceID) <- c("i", "j", "k", "value")
SourceID$i <- as.factor(SourceID$i)
SourceID$j <- as.factor(SourceID$j)
SourceID$k <- as.factor(SourceID$k)
attr(SourceID, "symName") <- "pSourceID"
attr(SourceID, "domains") <- c("sSource", "sStableType", "sID")

#pAnimals(sSource, sAnimalCategory)
Animals <- Stables[,6:43]
Animals$Nr <- Sources$Nr
Animals <- melt(Animals, id="Nr")
colnames(Animals) <- c("i", "j", "value")
Animals$i <- as.factor(Animals$i)
Animals$j <- as.factor(Animals$j)
attr(Animals, "symName") <- "pAnimals"
attr(Animals, "domains") <- c("sSource, sAnimalCategory")

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

#Sets
Source <- data.frame(SourceImpact$i)
colnames(Source) <- "i"
attr(Source, "symName") = "sSource"

AnimalCategory <- data.frame(EmissionFactor$i)
colnames(AnimalCategory) <- "i"
attr(AnimalCategory, "symName") = "sAnimalCategory"

StableType <- data.frame(EmissionFactor$j)
colnames(StableType) <- "i"
attr(StableType, "symName") = "sStableType"

ID <- data.frame(c("Exploitation", "NIS", "Farmer"))
colnames(ID) <- "i"
attr(ID, "symName") = "sID"

ImpactScores <- data.frame(c("ADS", "SS"))
colnames(ImpactScores) <- "i"
attr(ImpactScores, "symName") = "sImpactScores"

Coordinates <- data.frame(c("X", "Y"))
colnames(Coordinates) <- "i"
attr(Coordinates, "symName") = "sCoordinates"

#Bundling all data in gdx-file
wgdx.lst("FarmsFlanders.gdx",  Source, AnimalCategory, StableType, ID, 
         ImpactScores, Coordinates,SourceLocation, SourceID, SourceImpact, 
          Animals, EmissionFactor)


