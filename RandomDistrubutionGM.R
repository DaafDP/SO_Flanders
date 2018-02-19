rm(list=ls())

#Packages and GAMS directory
library(gdxrrw)
library(qdap)
library(xlsx)
igdx("C:/GAMS/win64/24.6/")

Ex_FT <- rgdx.set('SODat.gdx', 'sExploitation_FarmType')
set.seed(1)
Ex_FT$zscore <- rnorm(n=nrow(Ex_FT), mean = 0, sd=1)
Ex_FT$percentile <- pnorm(Ex_FT$zscore)

AMS <- read.xlsx("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/GegevensAMS_versie3.xlsx", sheetIndex = 1)
AMS <- subset(AMS, Titel == 3000)

AMS <- subset(AMS, Deler %in% c("D111", "D112", "D113", "D114", "D115", "D116", "D118", "D119", "D122"))

AMSbenaming <- c("Melkvee (incl jongvee)", "Gesloten vleesveehouderij", "Fokvarkens", 
                 "Vleesvarkens", "Gesloten varkenshouderij", "Legpluimvee", "Slachtpluimvee", "SREK224")
Sectors <- c("Dairy", "ClosedBeef", "PigRearing", 
             "PigFattening", "ClosedPigFattening", "LayingHens", "Broilers", "BeefBulls")
AMS$Rekening <- mgsub(AMSbenaming, Sectors, AMS$Rekening)

AMS[which(AMS$Rekening %in% c("LayingHens", "Broilers")),c("Waarde_RG", "Waarde_STDEV")] <- 
        AMS[which(AMS$Rekening %in% c("LayingHens", "Broilers")),c("Waarde_RG", "Waarde_STDEV")]/100

#Take assigned z-score to find gross margin for expoloitation x in year y. 
#Then, take average for all years
Ex_FT$GM <- apply(Ex_FT, 1, function(x){
        print(x[1])
                Sector <- subset(AMS, Rekening == as.character(x[2]))
                Sector$GM <- Sector$Waarde_RG + (as.numeric(x[3])*Sector$Waarde_STDEV)
                return(mean(Sector$GM))
       
})


#Some categories without AMS data
#Standard deviation 1/3 of mean GM

#Sheep: old AMS
Ex_FT[which(Ex_FT$sFarmType == "Sheep"), "GM"] <- 
       111.25 +  Ex_FT[which(Ex_FT$sFarmType == "Sheep"), "zscore"] * 37.08333333

# #BeefBulls: old AMS
# Ex_FT[which(Ex_FT$sFarmType == "BeefBulls"), "GM"] <- 
# 362 +  Ex_FT[which(Ex_FT$sFarmType == "BeefBulls"), "zscore"] * 120.6666667

#RearingLayingHens: KWIN 2017-2018
Ex_FT[which(Ex_FT$sFarmType == "RearingLayingHens"), "GM"] <- 
1.83 +  Ex_FT[which(Ex_FT$sFarmType == "RearingLayingHens"), "zscore"] * (1.83/3)

#RearingParentsBroilers: KWIN 2017-2018
Ex_FT[which(Ex_FT$sFarmType == "RearingParentsBroilers"), "GM"] <- 
3.12 +  Ex_FT[which(Ex_FT$sFarmType == "RearingParentsBroilers"), "zscore"] * (3.12/3)

#ParentsBroilers: KWIN 2017-2018
Ex_FT[which(Ex_FT$sFarmType == "ParentsBroilers"), "GM"] <- 
6.13 +  Ex_FT[which(Ex_FT$sFarmType == "ParentsBroilers"), "zscore"] * (6.13/3)

#Goats: KWIN 2017-2018
Ex_FT[which(Ex_FT$sFarmType == "Goats"), "GM"] <- 
246.85 +  Ex_FT[which(Ex_FT$sFarmType == "Goats"), "zscore"] * (246.85/3)

#Horses: no GM
Ex_FT[which(Ex_FT$sFarmType == "Horses"), "GM"] <- 0

#Turkeys: KWIN 2017-2018
Ex_FT[which(Ex_FT$sFarmType == "Turkeys"), "GM"] <- 
        7.71 +  Ex_FT[which(Ex_FT$sFarmType == "Turkeys"), "zscore"] * (7.71/3)

#FatteningCalves: KWIN 2017-2018 (blankvleeskalf)
Ex_FT[which(Ex_FT$sFarmType == "FatteningCalves"), "GM"] <- 
        50 +  Ex_FT[which(Ex_FT$sFarmType == "FatteningCalves"), "zscore"] * (50/3)

#AnimalCategory for which GM applies
Ex_FT$sAnimalCategory <- "a"
Ex_FT[which(Ex_FT$sFarmType == "Dairy"), "sAnimalCategory"]<- "a1131"
Ex_FT[which(Ex_FT$sFarmType == "ClosedBeef"), "sAnimalCategory"]<- "a1132"
Ex_FT[which(Ex_FT$sFarmType == "BeefBulls"), "sAnimalCategory"]<- "a117"
Ex_FT[which(Ex_FT$sFarmType == "PigFattening"), "sAnimalCategory"]<- "a124"
Ex_FT[which(Ex_FT$sFarmType == "PigRearing"), "sAnimalCategory"]<- "a125"
Ex_FT[which(Ex_FT$sFarmType == "ClosedPigFattening"), "sAnimalCategory"]<- "a125"
Ex_FT[which(Ex_FT$sFarmType == "LayingHens"), "sAnimalCategory"]<- "a133"
Ex_FT[which(Ex_FT$sFarmType == "Broilers"), "sAnimalCategory"]<- "a32"
Ex_FT[which(Ex_FT$sFarmType == "RearingLayingHens"), "sAnimalCategory"]<- "a132"                                                        
Ex_FT[which(Ex_FT$sFarmType == "RearingParentsBroilers"), "sAnimalCategory"]<- "a99"
Ex_FT[which(Ex_FT$sFarmType == "ParentsBroilers"), "sAnimalCategory"]<- "a98"  
Ex_FT[which(Ex_FT$sFarmType == "Sheep"), "sAnimalCategory"]<- "a43"  
Ex_FT[which(Ex_FT$sFarmType == "Goats"), "sAnimalCategory"]<- "a165"  
Ex_FT[which(Ex_FT$sFarmType == "Turkeys"), "sAnimalCategory"]<- "a92" 
Ex_FT[which(Ex_FT$sFarmType == "FatteningCalves"), "sAnimalCategory"]<- "a14"  
Ex_FT[which(Ex_FT$sFarmType == "Horses"), "sAnimalCategory"]<- "a151"  

write.csv(Ex_FT, "GrossMargins.csv")
