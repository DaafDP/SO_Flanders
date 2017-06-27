##Maps of Flanders (type of animals, air scrubbers, ..)
rm(list=ls())

#1. Loading data
EMAV <- read.csv("EMAV.csv")
Stables1 <- read.csv("StablesS1.csv")
Stables2 <- read.csv("StablesS2.csv")

#2. Different Animal Groups
AnimalGroups <- as.data.frame(unique(EMAV[,c("DIERCODE", "OMS_DIERGROEP")]))
AnimalGroups <- AnimalGroups[-c(40:42),]

#3. Loading shapefile NIS/Remove all unnesscary columns
library(rgdal)
library(sp)
gem <- readOGR(dsn="C:/Users/ddpue/Documents/GPBV Flanders/GIS/Referentiebestanden Vlaams Gewest",
               layer="Refgem")
plot(gem)
gem@data[, c("SCmean", "TISmean", "emsum", "emmean", "emSC", "emTIS", "emTISnorm")] <- NULL


#4. Dataframe with frequency of stable per municipality
NISStables <- Stables1[,c("NIS", "StableType")]
NISStables <- as.data.frame(table(Stables1[,c("NIS", "StableType")]))

#5. Select systems with air scrubber (S 1, S 2, S 3)
AirScrubbers <- subset(NISStables, Freq > 0)
AirScrubbers <- subset(AirScrubbers, grepl("S ", StableType) )

#Summate all air scrubbers per municipality
AirScrubbers <- aggregate(AirScrubbers$Freq,by=list(AirScrubbers$NIS), sum)
colnames(AirScrubbers) <- c("NIS", "freq")
 
#6. Couple with attribute table gem
gem@data <- data.frame(gem@data, AirScrubbers[match(gem@data[,"NISCODE"], AirScrubbers[,"NIS"]),
gem@data$NIS <- NULL                                              ])
        
#7.Plot air scrubbers on map
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
        geom_path(color="white")+
        coord_equal()+
        scale_color_manual(values=pal)
