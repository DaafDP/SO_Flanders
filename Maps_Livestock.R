##Maps of Flanders (type of animals, air scrubbers, ..)
rm(list=ls())
dev.off()

setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/")

#Loading data
EMAV <- read.csv("EMAV.csv")
Stables1 <- read.csv("StablesS1.csv")
Stables2 <- read.csv("StablesS2.csv")

#Different Animal Groups
AnimalGroups <- as.data.frame(unique(EMAV[,c("DIERCODE", "OMS_DIERGROEP")]))
AnimalGroups <- AnimalGroups[-c(40:42),]

#Loading shapefile NIS/Remove all unnesscary columns
library(rgdal)
library(sp)
library(rgeos)
gem <- readOGR(dsn="C:/Users/ddpue/Documents/GPBV Flanders/GIS/Referentiebestanden Vlaams Gewest",
               layer="Refgem")
gems <- gSimplify(gem, tol=10, topologyPreserve = TRUE)
gem <- SpatialPolygonsDataFrame(gems, data=gem@data)
rm(gems)

gem@data[, c("SCmean", "TISmean", "emsum", "emmean", "emSC", "emTIS", "emTISnorm")] <- NULL


# Dataframe with frequency of stable per municipality
NISStables <- Stables1[,c("NIS", "StableType")]
NISStables <- as.data.frame(table(Stables1[,c("NIS", "StableType")]))

NISStables <- subset(NISStables, Freq > 0)

####Map Air Scrubbers (472 in total)

# Select systems with air scrubber (S 1, S 2, S 3)
AirScrubbers <- subset(NISStables, Freq > 0)
AirScrubbers <- subset(AirScrubbers, grepl("S ", StableType) )

#Summate all air scrubbers per municipality
AirScrubbers <- aggregate(AirScrubbers$Freq,by=list(AirScrubbers$NIS), sum)
#AirScrubbers$ScrubberType <- strtrim(AirScrubbers$ScrubberType, 3)
# AirScrubbers <- aggregate(AirScrubbers, by = list(AirScrubbers$StableType), sum)
colnames(AirScrubbers) <- c("NIS","freq")
 
# Couple with attribute table gem
gem@data <- data.frame(gem@data, AirScrubbers[match(gem@data[,"NISCODE"], AirScrubbers[,"NIS"]),
                                              ])
gem@data$NIS <- NULL
gem@data[is.na(gem@data)] <- 0


        
#.Plot air scrubbers on map
library("maptools")
library("ggplot2")
library("plyr")

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")


#palette 
library(RColorBrewer)
pal <- brewer.pal(5, "Reds")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=freq)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", 
                name="# Air Scrubbers")

#Map 3 types of air scrubbers (already in shapefile - QGIS)

ggplot(gem.df)+
        aes(long, lat, group=group, fill=AS_bio)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", 
                name="# Biological \n Air Scrubbers",
                breaks=seq(from=0, to=12, by=2),
                label=seq(from=0, to=12, by=2))

ggplot(gem.df)+
        aes(long, lat, group=group, fill=AS_chem)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", 
                name="# Chemical \n Air Scrubbers", 
                label=c(0,2,4,6,8))

ggplot(gem.df)+
        aes(long, lat, group=group, fill=biobed)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", 
                name="# Biobeds")

####Map Animal Categories (cattle, pigs, poultry, Horses, Horses)
Animals <- Stables1[,c("NIS", intersect(colnames(Stables1), AnimalGroups$DIERCODE))]

##Cows
CowIndex <- as.character(AnimalGroups$DIERCODE[which(AnimalGroups$OMS_DIERGROEP == "Runderen")])
Cows <- Animals[,c("NIS", CowIndex)]
Cows <- aggregate(Cows[,CowIndex], by=list(Cows$NIS), sum)

Cows$CowsTotal <- rowSums(Cows[,CowIndex])
sum(Cows$CowsTotal) #1998233
gem@data <- data.frame(gem@data, Cows[match(gem@data[,"NISCODE"], Cows[,1]),])

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=CowsTotal)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", name="cows"
                )

##Pigs
PigIndex <- as.character(AnimalGroups$DIERCODE[which(AnimalGroups$OMS_DIERGROEP == "Varkens")])
PigIndex <- intersect(PigIndex, colnames(Animals))
Pigs <- Animals[,c("NIS", PigIndex)]
Pigs <- aggregate(Pigs[,PigIndex], by=list(Pigs$NIS), sum)

Pigs$PigsTotal <- rowSums(Pigs[,PigIndex])
sum(Pigs$PigsTotal) #8590512
gem@data <- data.frame(gem@data, Pigs[match(gem@data[,"NISCODE"], Pigs[,1]),])

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=PigsTotal)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", name="Pigs"
        )

##Poultry
PoultryIndex <- as.character(AnimalGroups$DIERCODE[which(AnimalGroups$OMS_DIERGROEP == "Pluimvee")])
PoultryIndex <- intersect(PoultryIndex, colnames(Animals))
Poultry <- Animals[,c("NIS", PoultryIndex)]
Poultry <- aggregate(Poultry[,PoultryIndex], by=list(Poultry$NIS), sum)

Poultry$PoultryTotal <- rowSums(Poultry[,PoultryIndex])
sum(Poultry$PoultryTotal) #46667608
gem@data <- data.frame(gem@data, Poultry[match(gem@data[,"NISCODE"], Poultry[,1]),])

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=PoultryTotal)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", name="Poultry"
        )

##Horses
HorsesIndex <- as.character(AnimalGroups$DIERCODE[which(AnimalGroups$OMS_DIERGROEP == "Paarden")])
HorsesIndex <- intersect(HorsesIndex, colnames(Animals))
Horses <- Animals[,c("NIS", HorsesIndex)]
Horses <- aggregate(Horses[,HorsesIndex], by=list(Horses$NIS), sum)

Horses$HorsesTotal <- rowSums(Horses[,HorsesIndex])
sum(Horses$HorsesTotal) #79316
gem@data <- data.frame(gem@data, Horses[match(gem@data[,"NISCODE"], Horses[,1]),])

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=HorsesTotal)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", name="Horses"
        )

##Other
OtherIndex <- as.character(AnimalGroups$DIERCODE[which(AnimalGroups$OMS_DIERGROEP == "Andere")])
OtherIndex <- intersect(OtherIndex, colnames(Animals))
Other <- Animals[,c("NIS", OtherIndex)]
Other <- aggregate(Other[,OtherIndex], by=list(Other$NIS), sum)

Other$OtherTotal <- rowSums(Other[,OtherIndex])
sum(Other$OtherTotal) #307097
gem@data <- data.frame(gem@data, Other[match(gem@data[,"NISCODE"], Other[,1]),])

gem@data$id <- rownames(gem@data)
gem.points <- fortify(gem, region="id")
gem.df <- join(gem.points, gem@data, by ="id")

ggplot(gem.df)+
        aes(long, lat, group=group, fill=OtherTotal)+
        geom_polygon() +
        geom_path(color="black")+
        coord_equal()+
        scale_fill_gradient(
                low="white", high="darkred", name="Other"
        )