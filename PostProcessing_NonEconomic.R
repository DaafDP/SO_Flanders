##Reading in  results from non-economic simulation on all 
##livestock exploitations in Flanders

#Including dynamic simulation 2016-2035

#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
library(qdap)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

Stables <- read.csv("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/StablesS1.csv")

#Set new working directory
setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/ResultsNonEconomic")

################################################################################
#First: effect of random allocation (NIS based or totally random)
################################################################################
#Clear environment
rm(list = ls())

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

AggregateResults$Scenario <- mgsub(c("scenario1", "scenario2", "scenario3"), 
                                   c("<5% SS", "SO max NH3", "SO min ADS"), 
                                   AggregateResults$Scenario)

ggplot(data=subset(AggregateResults, variable=="Emission" & Scenario %in% c("<5% SS", "SO max NH3"))
       , aes(x=Scenario, y=value, group=Seed, colour=Seed, shape=Allocation))+
        geom_line(aes(linetype=Allocation)) +
        geom_point() +
        theme(legend.title=element_blank())+
        ylab("Total Ammonia Emission (kton/yr)") + xlab(NULL)

ggplot(data=subset(AggregateResults, variable=="ClosedStables")
       , aes(x=Scenario, y=value, group=Seed, colour=Seed, shape=Allocation))+
        geom_line(aes(linetype=Allocation)) +
        geom_point()+
        theme(legend.title=element_blank())+
        ylab("Number of empty stables")  + xlab(NULL)

ggplot(data=subset(AggregateResults, variable=="Impact" & Scenario %in% c("<5% SS", "SO min ADS"))
       , aes(x=Scenario, y=value, group=Seed, colour=Seed, shape=Allocation))+
        geom_line(aes(linetype=Allocation)) +
        geom_point()+
        theme(legend.title=element_blank())+
        ylab("Impact (total aggregate deposition score)") + xlab(NULL)

################################################################################
#Resuls non-dynamic
################################################################################

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

dEmissionAnimalGroup$Scenario <- mgsub(c("sc1", "sc2", "sc3",  "sc4"), 
                                   c("<5% SS", "SO", "PAN", "BAT"), 
                                   dEmissionAnimalGroup$Scenario)

SectorCols <- brewer.pal(5, name='Accent')

#All sectors together
ggplot(data=subset(dEmissionAnimalGroup, Scenario != "max"), 
        aes(x=Scenario, y=Emission, group=Sector, colour=Sector, shape=Sector))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
                    axis.title = element_text(size = 18),
                    legend.title = element_text(size = 18),
                    legend.text = element_text(size = 18),
                    axis.text = element_text(size = 18))+
        ylab("Total Ammonia Emission (kgNH3/yr)") + xlab(NULL)

ggplot(data=subset(dEmissionAnimalGroup, Scenario != "max"), 
       aes(x=Scenario, y=Emission, fill=Sector))+
        geom_bar(colour="black", stat="identity")+
        scale_fill_manual(values=SectorCols)
        


#Relative to maximum
EmissionAnimalGroupRelative <- subset(dEmissionAnimalGroup, Scenario != "max")

EmissionAnimalGroupRelative$EmissionRelative <- apply(EmissionAnimalGroupRelative, 1, function(x){
        max <- as.numeric(subset(dEmissionAnimalGroup, Sector == x[1] & Scenario== "max")[2])
        print(as.numeric(x[2])/max)
        })

ggplot(data=EmissionAnimalGroupRelative, 
       aes(x=Scenario, y=EmissionRelative, group=Sector, colour=Sector, shape=Sector))+
       geom_line(lty="dotted") +
        geom_point(size=2)+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Relative NH3 emission (1=maximum)") + xlab(NULL)

ggplot(data=EmissionAnimalGroupRelative, aes(x=Scenario, y=EmissionRelative, fill=Sector))+
        geom_bar(stat="identity", position=position_dodge(), colour="black")+
        scale_fill_manual(values=SectorCols)

#ggsave("EmissionAnimalGroup_relative.png", dpi=400)
##Animal per sector
colnames(dAnimalsGroup) <- c("Sector", "Scenario", "Number")
dMaxAnimalsGroup$Scenario <- "max"
colnames(dMaxAnimalsGroup) <- c("Sector", "Number", "Scenario")

dAnimalsGroup$Scenario <- mgsub(c("sc1", "sc2", "sc3",  "sc4"), 
                                       c("<5% SS", "SO", "PAN", "BAT"), 
                                       dAnimalsGroup$Scenario)


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
max=as.numeric(subset(dAnimalsGroup, Sector=="Pigs" & Scenario=="max")[2])
ggplot(data=subset(dAnimalsGroup, Sector=="Pigs" & Scenario != "max"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        #theme(legend.title=element_blank())+
        geom_hline(aes(yintercept=max), lty="dashed", colour="red")+
        ylab("Total number of pigs") + xlab(NULL)

#Cattle
max=as.numeric(subset(dAnimalsGroup, Sector=="Cattle" & Scenario=="max")[2])
ggplot(data=subset(dAnimalsGroup, Sector=="Cattle" & Scenario != "max"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        #theme(legend.title=element_blank())+
        geom_hline(aes(yintercept=max), lty="dashed", colour="red")+
        ylab("Total number of cattle") + xlab(NULL)

#Poultry
max=as.numeric(subset(dAnimalsGroup, Sector=="Poultry" & Scenario=="max")[2])
ggplot(data=subset(dAnimalsGroup, Sector=="Poultry" & Scenario != "max"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        #theme(legend.title=element_blank())+
        geom_hline(aes(yintercept=max), lty="dashed", colour="red")+
        ylab("Total number of poultry") + xlab(NULL)

#Other
max=as.numeric(subset(dAnimalsGroup, Sector=="Others" & Scenario=="max")[2])
ggplot(data=subset(dAnimalsGroup, Sector=="Others" & Scenario != "max"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        #theme(legend.title=element_blank())+
        geom_hline(aes(yintercept=max), lty="dashed", colour="red")+
        ylab("Total number of others") + xlab(NULL)
#Horses
max=as.numeric(subset(dAnimalsGroup, Sector=="Horses" & Scenario=="max")[2])
ggplot(data=subset(dAnimalsGroup, Sector=="Horses" & Scenario != "max"),  
       aes(x=Scenario, y=Number, group=1))+
        geom_line(lty="dotted") +
        geom_point(size=2)+
        #theme(legend.title=element_blank())+
        geom_hline(aes(yintercept=max), lty="dashed", colour="red")+
        ylab("Total number of horses") + xlab(NULL)

#Relative to maximum
AnimalGroupRelative <- subset(dAnimalsGroup, Scenario != "max")

AnimalGroupRelative$NumberRelative <- apply(AnimalGroupRelative, 1, function(x){
        max <- as.numeric(subset(dAnimalsGroup, Sector == x[1] & Scenario== "max")[2])
        print(as.numeric(x[2])/max)
})

ggplot(data=AnimalGroupRelative, 
       aes(x=Scenario, y=NumberRelative, group=Sector, colour=Sector, shape=Sector))+
        geom_line(lty='dotted') +
        geom_point(size=2)+
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
colnames(ADSNIS) <- c("sNIS", "ADS_scen1", "ADs_scen2", "ADS_scen3", "ADS_scen4")
EmissionNIS <- dcast(dEmissionNIS, sNIS~sScen)
colnames(EmissionNIS) <- c("sNIS", "EmSc1", "EmSc2", "EmSc3",  "EmSc4")
EmissionNIS <- merge(EmissionNIS, dMaxEmissionNIS, by = "sNIS")
DataNISQGIS <- merge(EmissionNIS, ADSNIS, by="sNIS")
library(stringi)
DataNISQGIS$sNIS <- stri_sub(DataNISQGIS$sNIS, 2)
DataNISQGIS[is.na(DataNISQGIS)] <- 0
write.csv(DataNISQGIS, "C:/Users/ddpue/Documents/Spatial Optimization Flanders/GIS/DataNISQGIS.csv")

################################################################################
#Dynamic simulation
################################################################################
#Clear environment
rm(list = ls())

Pars <- c("dAmmoniaEmissionExploitation", "dAmmoniaEmissionRegion",  "dImpactExploitation",
          "dImpactRegion", "dAnimalGroup", "dLivestockUnits")


DynamicData <- sapply(Pars, function(x){
        tmp <- rgdx.param("resultsDynamicS1.gdx", x, squeeze=FALSE)
})

list2env(DynamicData, envir=.GlobalEnv)

colnames(dAmmoniaEmissionRegion) <- c("Scenario", "Year", "Emission")

dAmmoniaEmissionRegion$Year <- as.character(dAmmoniaEmissionRegion$Year)
dAmmoniaEmissionRegion$Year <- as.numeric(dAmmoniaEmissionRegion$Year)

original <- paste("Scenario", c(1:6), sep="")
new <- c("<5% SS", "PAN", "BAT", "SO max NH3", "SO ref", "SO NEC")

dAmmoniaEmissionRegion$Scenario <- mgsub(original, new, dAmmoniaEmissionRegion$Scenario)

ggplot(data=subset(dAmmoniaEmissionRegion, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=Emission, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        #scale_colour_discrete(labels = c(1:6))+
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

dImpactRegion$Scenario <- mgsub(original, new, dImpactRegion$Scenario)

ggplot(data=subset(dImpactRegion, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")), 
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
Ammonia_Impact$Ratio <- dImpactRegion$TotalADS/dAmmoniaEmissionRegion$Emission * 10^6

Ammonia_Impact$Scenario <- mgsub(original, new, Ammonia_Impact$Scenario)

ggplot(data=subset(Ammonia_Impact, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=Ratio, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("ADS/kton NH3") + xlab(NULL)

colnames(dLivestockUnits) <- c("Scenario", "Year", "TotalLSU")

dLivestockUnits$Year <- as.character(dLivestockUnits$Year)
dLivestockUnits$Year <- as.numeric(dLivestockUnits$Year)

dLivestockUnits$Scenario <- mgsub(original, new, dLivestockUnits$Scenario)

ggplot(data=subset(dLivestockUnits, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=TotalLSU, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total LSU") + xlab(NULL)

colnames(dAnimalGroup) <- c("Sector", "Scenario", "Year", "Number")

dAnimalGroup$Year <- as.character(dAnimalGroup$Year)
dAnimalGroup$Year <- as.numeric(dAnimalGroup$Year)

dAnimalGroup$Scenario <- mgsub(original, new, dAnimalGroup$Scenario)

ggplot(data=subset(dAnimalGroup, Sector=="Runderen" & Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC") ), 
       aes(x=Year, y=Number, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total number of Cattle") + xlab(NULL)

ggplot(data=subset(dAnimalGroup, Sector=="Varkens" & Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC") ), 
       aes(x=Year, y=Number, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total number of Pigs") + xlab(NULL)

ggplot(data=subset(dAnimalGroup, Sector=="Pluimvee" & Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC") ), 
       aes(x=Year, y=Number, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total number of Poultry") + xlab(NULL)

ggplot(data=subset(dAnimalGroup, Sector=="Paarden" & Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC") ), 
       aes(x=Year, y=Number, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total number of Horses") + xlab(NULL)

ggplot(data=subset(dAnimalGroup, Sector=="Andere" & Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")), 
       aes(x=Year, y=Number, group=Scenario, colour=Scenario, shape=Scenario))+
        geom_line() +
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("Total number of Other") + xlab(NULL)

################################################################################
#Effect random draw year of permit (3 iterations)
################################################################################
#Clear environment
rm(list = ls())

Pars <- c("dAmmoniaEmissionRegion", 
          "dImpactRegion", "dLivestockUnits", "dAmmoniaEmissionExploitation")

DataS1 <- sapply(Pars, function(x){
        tmp <- rgdx.param("resultsDynamicS1.gdx", x, squeeze=FALSE)
})

list2env(DataS1, envir=.GlobalEnv)

dAmmoniaEmissionRegion$Iteration <- "Seed1"
Ammonia <- dAmmoniaEmissionRegion

dImpactRegion$Iteration <- "Seed1"
Impact <- dImpactRegion

dLivestockUnits$Iteration <- "Seed1"
Livestock <- dLivestockUnits

DataS2 <- sapply(Pars, function(x){
        tmp <- rgdx.param("resultsDynamicS2.gdx", x, squeeze=FALSE)
})

list2env(DataS2, envir=.GlobalEnv)

dAmmoniaEmissionRegion$Iteration <- "Seed2"
dImpactRegion$Iteration <- "Seed2"
dLivestockUnits$Iteration <- "Seed2"

Ammonia <- rbind(Ammonia, dAmmoniaEmissionRegion)
Impact <- rbind(Impact, dImpactRegion)
Livestock <- rbind(Livestock, dLivestockUnits)

DataS3 <- sapply(Pars, function(x){
        tmp <- rgdx.param("resultsDynamicS3.gdx", x, squeeze=FALSE)
})

list2env(DataS3, envir=.GlobalEnv)

dAmmoniaEmissionRegion$Iteration <- "Seed3"
dImpactRegion$Iteration <- "Seed3"
dLivestockUnits$Iteration <- "Seed3"

Ammonia <- rbind(Ammonia, dAmmoniaEmissionRegion)
Impact <- rbind(Impact, dImpactRegion)
Livestock <- rbind(Livestock, dLivestockUnits)

original <- paste("Scenario", c(1:6), sep="")
new <- c("<5% SS", "PAN", "BAT", "SO max NH3", "SO min ADS", "SO NEC")

colnames(Ammonia) <- c("Scenario", "Year", "Emission", "Iteration")
Ammonia$Scenario <- mgsub(original, new, Ammonia$Scenario)
Ammonia$Year <- as.character(Ammonia$Year)
Ammonia$Year <- as.numeric(Ammonia$Year)

ggplot(data=subset(Ammonia, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=Emission, group=Scenario, colour=Scenario, shape=Iteration))+
        geom_point()+
        #scale_colour_discrete(labels = c(1:6))+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        geom_hline(yintercept=32155570, colour="DarkBlue", lty="dashed")+
        geom_hline(yintercept=28546271, colour="Red", lty="dashed")+
        ylab("Total ammonia emission kg NH3/yr") + xlab(NULL)

colnames(Impact) <- c("Scenario", "Year", "ADS", "Iteration")
Impact$Scenario <- mgsub(original, new, Impact$Scenario)
Impact$Year <- as.character(Impact$Year)
Impact$Year <- as.numeric(Impact$Year)

ggplot(data=subset(Impact, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=ADS, group=Scenario, colour=Scenario, shape=Iteration))+
        geom_point()+
        #scale_colour_discrete(labels = c(1:6))+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
              ylab("Total ADS") + xlab(NULL)

Ammonia_Impact <- Ammonia[,c("Scenario", "Year", "Iteration")]
Ammonia_Impact$Ratio <- Impact$ADS/Ammonia$Emission * 10^6

Ammonia_Impact$Scenario <- mgsub(original, new, Ammonia_Impact$Scenario)

ggplot(data=subset(Ammonia_Impact, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=Ratio, group=Scenario, colour=Scenario, shape=Iteration))+
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
        ylab("ADS/kton NH3") + xlab(NULL)

colnames(Livestock) <- c("Scenario", "Year", "LSU", "Iteration")
Livestock$Scenario <- mgsub(original, new, Livestock$Scenario)
Livestock$Year <- as.character(Livestock$Year)
Livestock$Year <- as.numeric(Livestock$Year)

ggplot(data=subset(Livestock, Scenario %in% c("<5% SS", "PAN", "BAT", "SO ref", "SO NEC")),
       aes(x=Year, y=LSU, group=Scenario, colour=Scenario, shape=Iteration))+
        geom_point()+
        theme(legend.title=element_blank(), 
              plot.title = element_text(size = 15, face = "bold"),
              axis.title = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text = element_text(size = 18))+
         ylab("Total LSU") + xlab(NULL)

#Plot variance in impact
# VarRatio <- aggregate(LSU ~ Scenario + Year, data=Livestock, var)
# 
# ggplot(data=VarRatio,
#        aes(x=Year, y=LSU, group=Scenario, colour=Scenario, shape=Scenario))+
#         geom_line()+
#         geom_point()+
#         theme(legend.title=element_blank(), 
#               plot.title = element_text(size = 15, face = "bold"),
#               axis.title = element_text(size = 18),
#               legend.title = element_text(size = 18),
#               legend.text = element_text(size = 18),
#               axis.text = element_text(size = 18))+
#         ylab("Var ADS/kt NH3") + xlab(NULL)