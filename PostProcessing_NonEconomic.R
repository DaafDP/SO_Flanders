##Reading in  results from non-economic simulation on all 
##livestock exploitations in Flanders

#Including dynamic simulation 2016-2035

#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

Stables <- read.csv("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/StablesS1.csv")



#Set new working directory
setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/ResultsNonEconomic")

#First: effect of random allocation (NIS based or totally random)
AggregateResults <- data.frame(matrix(nrow=9, ncol=6))
colnames(AggregateResults) <- c("Seed", "Scenario", "Emission", "ClosedStables", "Impact", "Allocation")
AggregateResults$Seed <- paste("NISbased_seed", c(rep(1,3), rep(2,3), rep(3,3)), sep="") 
AggregateResults$Scenario <- paste("scenario", rep(c(1,2,3), 3), sep="")
AggregateResults$Emission <- c(30.8, 31.60, 30.80, 30.86, 31.64, 30.86,
                                     30.73, 31.57, 30.73)
AggregateResults$ClosedStables <- c(856, 2081, 3287, 825, 2033, 
                                          3220, 837, 2193, 3401)
AggregateResults$Impact <- c(20122, 20122, 17590, 20204, 20204, 17706,
                                   20105, 20105, 17471)
AggregateResults$Allocation <- "NISbased"

AggregateResultsRandom <- data.frame(matrix(nrow=9, ncol=6))
colnames(AggregateResultsRandom) <-  c("Seed", "Scenario", "Emission", "ClosedStables", "Impact", "Allocation")
AggregateResultsRandom$Seed <- paste("FullyRandom_seed", c(rep(1,3), rep(2,3), rep(3,3)), sep="") 
AggregateResultsRandom$Scenario <- paste("scenario", rep(c(1,2,3), 3), sep="")
AggregateResultsRandom$Emission <- c(30.46, 31.37, 30.46, 30.48, 31.40, 30.48,
                                     30.52, 31.39, 30.52)
AggregateResultsRandom$ClosedStables <- c(1113, 1954, 3864, 977, 1851, 3165, 
                                          1093, 1940, 3085)
AggregateResultsRandom$Impact <- c(23138, 23138, 19937, 23351, 23351, 20110,
                                   23159, 23159, 20063)
AggregateResultsRandom$Allocation <- "FullyRandom"

AggregateResults <- rbind(AggregateResults, AggregateResultsRandom)

AggregateResults <- melt(AggregateResults)

ggplot(data=subset(AggregateResults, variable=="Emission")
       , aes(x=Scenario, y=value, group=Seed, colour=Seed, shape=Allocation))+
        geom_line() +
        geom_point() +
        theme(legend.title=element_blank())+
        ylab("Total Ammonia Emission (kton/yr)") + xlab(NULL)

ggplot(data=subset(AggregateResults, variable=="ClosedStables")
       , aes(x=Scenario, y=value, group=Seed, colour=Seed, shape=Allocation))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank())+
        ylab("Number of empty stables")  + xlab(NULL)

ggplot(data=subset(AggregateResults, variable=="Impact", shape=Allocation)
       , aes(x=Scenario, y=value, group=Seed, colour=Seed))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank())+
        ylab("Impact (total aggregate deposition score)") + xlab(NULL)

#Read in results (NIS-based allocation, seed 1)
Pars <- c("dSignificanceScore", "dADS", "dTotalADS", "dPercentageOccupied", "dPercentageOccupiedRegion", 
          "dClosedStables", "dClosedExploitations","dEmissionStable",
          "dEmissionExploitation", "dEmissionRegion", "dAnimals", 
        "dStableTypesNIS", "dAnimalsNIS", "dAnimalGroupNIS",
        "dEmissionNIS", "dADSNIS", "dEmissionAnimal", "dEmissionStableType", "dEmissionAnimalGroup",
        "dMarginalSignificanceScore", "dMaxEmissionStable", "dMaxEmissionExploitation", "dMaxEmissionNIS", 
        "dMaxEmissionAnimalCategory","dMaxEmissionSector", "dMaxEmissionStableType", "dMaxAnimalsGroup", "dAnimalsGroup")


AllData <- sapply(Pars, function(x){
        tmp <- rgdx.param("results.gdx", x, squeeze=FALSE)
})

list2env(AllData, envir=.GlobalEnv)

##Emission per sector (cattle, pigs, poultry, others, horses)
colnames(dMaxEmissionSector) <- c("Sector", "Emission")
dMaxEmissionSector$Scenario <- "max"

colnames(dEmissionAnimalGroup) <- c("Sector","Scenario","Emission")
dEmissionAnimalGroup <- rbind.data.frame(dMaxEmissionSector, dEmissionAnimalGroup)
levels(dEmissionAnimalGroup$Sector)[levels(dEmissionAnimalGroup$Sector)
                                    == "Runderen"] <- "Cattle"
levels(dEmissionAnimalGroup$Sector)[levels(dEmissionAnimalGroup$Sector)
                                    == "Varkens"] <- "Pigs"
levels(dEmissionAnimalGroup$Sector)[levels(dEmissionAnimalGroup$Sector)
                                    == "Pluimvee"] <- "Poultry"
levels(dEmissionAnimalGroup$Sector)[levels(dEmissionAnimalGroup$Sector)
                                    == "Andere"] <- "Others"
levels(dEmissionAnimalGroup$Sector)[levels(dEmissionAnimalGroup$Sector)
                                    == "Paarden"] <- "Horses"
#All sectors together
ggplot(data=dEmissionAnimalGroup, 
        aes(x=Scenario, y=Emission, group=Sector, colour=Sector, shape=Sector))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
                    axis.title = element_text(size = 18),
                    legend.title = element_text(size = 18),
                    legend.text = element_text(size = 18),
                    axis.text = element_text(size = 18))+
        ylab("Total Ammonia Emission (kgNH3/yr)") + xlab(NULL)
ggsave("EmissionAnimalGroup_absolute.png", dpi=400)

#Relative to maximum
EmissionAnimalGroupRelative <- subset(dEmissionAnimalGroup, Scenario != "max")

EmissionAnimalGroupRelative$EmissionRelative <- apply(EmissionAnimalGroupRelative, 1, function(x){
        max <- as.numeric(subset(dEmissionAnimalGroup, Sector == x[1] & Scenario== "max")[2])
        print(as.numeric(x[2])/max)
        })

ggplot(data=EmissionAnimalGroupRelative, 
       aes(x=Scenario, y=EmissionRelative, group=Sector, colour=Sector, shape=Sector))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Relative NH3 emission (1=maximum)") + xlab(NULL)
ggsave("EmissionAnimalGroup_relative.png", dpi=400)


##Animal per sector
colnames(dAnimalsGroup) <- c("Sector", "Scenario", "Number")
dMaxAnimalsGroup$Scenario <- "max"
colnames(dMaxAnimalsGroup) <- c("Sector", "Number", "Scenario")

dAnimalsGroup <- rbind.data.frame(dMaxAnimalsGroup, dAnimalsGroup)
levels(dAnimalsGroup$Sector)[levels(dAnimalsGroup$Sector)
                                    == "Runderen"] <- "Cattle"
levels(dAnimalsGroup$Sector)[levels(dAnimalsGroup$Sector)
                                    == "Varkens"] <- "Pigs"
levels(dAnimalsGroup$Sector)[levels(dAnimalsGroup$Sector)
                                    == "Pluimvee"] <- "Poultry"
levels(dAnimalsGroup$Sector)[levels(dAnimalsGroup$Sector)
                                    == "Andere"] <- "Others"
levels(dAnimalsGroup$Sector)[levels(dAnimalsGroup$Sector)
                                    == "Paarden"] <- "Horses"

#Pigs
ggplot(data=subset(dAnimalsGroup, Sector=="Pigs"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line() +
        geom_point()+
        #theme(legend.title=element_blank())+
        ylab("Total number of pigs") + xlab(NULL)

#Cattle
ggplot(data=subset(dAnimalsGroup, Sector=="Cattle"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line() +
        geom_point()+
        #theme(legend.title=element_blank())+
        ylab("Total number of cattle") + xlab(NULL)

#Poultry
ggplot(data=subset(dAnimalsGroup, Sector=="Poultry"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line() +
        geom_point()+
        #theme(legend.title=element_blank())+
        ylab("Total number of poultry") + xlab(NULL)

#Other
ggplot(data=subset(dAnimalsGroup, Sector=="Others"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line() +
        geom_point()+
        #theme(legend.title=element_blank())+
        ylab("Total number of others") + xlab(NULL)

#Horses
ggplot(data=subset(dAnimalsGroup, Sector=="Horses"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line() +
        geom_point()+
        #theme(legend.title=element_blank())+
        ylab("Total number of horses") + xlab(NULL)

#Relative to maximum
AnimalGroupRelative <- subset(dAnimalsGroup, Scenario != "max")

AnimalGroupRelative$NumberRelative <- apply(AnimalGroupRelative, 1, function(x){
        max <- as.numeric(subset(dAnimalsGroup, Sector == x[1] & Scenario== "max")[2])
        print(as.numeric(x[2])/max)
})

ggplot(data=AnimalGroupRelative, 
       aes(x=Scenario, y=NumberRelative, group=Sector, colour=Sector, shape=Sector))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank())+
        ylab("Relative number of animals (1=maximum)") + xlab(NULL)

#ClosedStables per stable Type
EmissionStable <- dcast(dEmissionStable, sStable~sScen)
EmissionStable[is.na(EmissionStable)] <- 0
Stables <- read.csv("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/StablesS1.csv")
StablesClosed <- Stables[c("StableType", "Name")]
colnames(StablesClosed) <- c("StableType", "sStable")
EmissionStable <- merge(EmissionStable, StablesClosed, by="sStable")

EmissionStableZeroSc1 <- subset(EmissionStable, sc1 == 0)
Sc1Closed <- as.data.frame(table(EmissionStableZeroSc1$StableType))
EmissionStableZeroSc2 <- subset(EmissionStable, sc2 == 0)
Sc2Closed <- as.data.frame(table(EmissionStableZeroSc2$StableType))
EmissionStableZeroSc3 <- subset(EmissionStable, sc3 == 0)
Sc3Closed <- as.data.frame(table(EmissionStableZeroSc3$StableType))

##Make files for QGIS
#Stables
StablesQGIS <- dcast(dEmissionStable, sStable~sScen)
StablesQGIS <- merge(StablesQGIS, dMaxEmissionStable, by="sStable")
Stables_Select <- Stables[c("Name", "X", "Y", "ADS", "SS")]
colnames(Stables_Select) <- c("sStable", "X", "Y", "ADS", "SS") 
StablesQGIS <- merge(StablesQGIS, Stables_Select, by="sStable")
StablesQGIS[is.na(StablesQGIS)] <- 0
write.csv(StablesQGIS, "C:/Users/ddpue/Documents/Spatial Optimization Flanders/GIS/StablesQGIS.csv")

##NIS
ADSNIS <- dcast(dADSNIS, sNIS~sScen)
colnames(ADSNIS) <- c("sNIS", "ADS_scen1", "ADs_scen2", "ADS_scen3")
EmissionNIS <- dcast(dEmissionNIS, sNIS~sScen)
colnames(EmissionNIS) <- c("sNIS", "EmSc1", "EmSc2", "EmSc3")
EmissionNIS <- merge(EmissionNIS, dMaxEmissionNIS, by = "sNIS")
DataNISQGIS <- merge(EmissionNIS, ADSNIS, by="sNIS")
library(stringi)
DataNISQGIS$sNIS <- stri_sub(DataNISQGIS$sNIS, 2)
DataNISQGIS[is.na(DataNISQGIS)] <- 0
write.csv(DataNISQGIS, "C:/Users/ddpue/Documents/Spatial Optimization Flanders/GIS/DataNISQGIS.csv")

#Dynamic simulation
Pars <- c("dAmmoniaEmissionExploitation", "dAmmoniaEmissionRegion",  "dImpactExploitation",
          "dImpactRegion")


DynamicData <- sapply(Pars, function(x){
        tmp <- rgdx.param("resultsDynamic.gdx", x, squeeze=FALSE)
})

list2env(DynamicData, envir=.GlobalEnv)

colnames(dAmmoniaEmissionRegion) <- c("Scenario", "Year", "Emission")

dAmmoniaEmissionRegion$Year <- as.character(dAmmoniaEmissionRegion$Year)
dAmmoniaEmissionRegion$Year <- as.numeric(dAmmoniaEmissionRegion$Year)

ggplot(data=dAmmoniaEmissionRegion, 
       aes(x=Year, y=Emission, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        geom_hline(yintercept=32155570, colour="DarkBlue", lty="dashed")+
        geom_hline(yintercept=28546271, colour="Red", lty="dashed")+
        ylab("Total ammonia emission kg NH3/yr") + xlab(NULL)

colnames(dImpactRegion) <- c("Scenario", "Year", "TotalADS")

dImpactRegion$Year <- as.character(dImpactRegion$Year)
dImpactRegion$Year <- as.numeric(dImpactRegion$Year)

ggplot(data=dImpactRegion, 
       aes(x=Year, y=TotalADS, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total ADS") + xlab(NULL)

Ammonia_Impact <- dAmmoniaEmissionRegion[,1:2]
Ammonia_Impact$Ratio <- dAmmoniaEmissionRegion$Emission/dImpactRegion$TotalADS

ggplot(data=Ammonia_Impact, 
       aes(x=Year, y=Ratio, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("NH3/ADS") + xlab(NULL)


