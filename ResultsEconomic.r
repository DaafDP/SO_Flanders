##Reading in  results from non-economic simulation on all 
##livestock exploitations in Flanders

#Including dynamic simulation 2016-2035

#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
library(qdap)
igdx("C:/GAMS/win64/24.6/")

#Clear environment
rm(list = ls())

#Load multiplot function
source("C:/Users/ddpue/Documents/R/multiplot.R")

################################################################################
#Global Stats
################################################################################
Stats <- c("dGap", "dObjEst", 
"dADSregion", "dAmmoniaRegion", "dProfitRegion", "dClosedStables",
"dClosedExploitations")


GlobalStats <- data.frame(c("FC", "ref", "sc1", "sc2", "sc3"))
names(GlobalStats) <- 'sScen'



for (x in (1:length(Stats))) {
  print(Stats[x])
  tmp <- rgdx.param("all.gdx", Stats[x], squeeze=FALSE)
  if(("FC" %in% tmp$sScen)==FALSE)  {
    tmp <- rbind(tmp, c("FC", 0))
  }
  GlobalStats <- merge(GlobalStats, tmp, id="sScen")
}

rownames(GlobalStats) <- GlobalStats$sScen

GlobalStats$dClosedStables <- as.numeric(GlobalStats$dClosedStables)
GlobalStats$dClosedExploitations <- as.numeric(GlobalStats$dClosedExploitations)

#GlobalStats$sScen <- NULL

rm(tmp, x, Stats)

#plot impact
p1 <- ggplot(data=GlobalStats, 
       aes(x=sScen, y=dADSregion))+
  geom_bar(colour="black", stat="identity")+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 18))+
  xlab("Scenario") + ylab("Total impact (ADS)")

#plot emission
p2 <- ggplot(data=GlobalStats, 
       aes(x=sScen, y=dAmmoniaRegion))+
  geom_bar(colour="black", stat="identity")+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 18))+
  xlab("Scenario") + ylab("NH3 (kg/yr)")

#plot profit
p3 <- ggplot(data=GlobalStats, 
       aes(x=sScen, y=dProfitRegion))+
  geom_bar(colour="black", stat="identity")+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 18))+
  xlab("Scenario") + ylab("Total Profit (€)")

Closed <- GlobalStats[c("sScen", "dClosedStables", "dClosedExploitations")]
Closed <- melt(Closed, id = "sScen")

#plot closed exploitations and closed stables
p4 <- ggplot(data=subset(Closed, value > 0), 
       aes(x=sScen, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_hue(labels=c("Stables", "Exploitations"))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 18),
        legend.title = element_blank(), legend.text = element_text(size=15))+
  xlab("Scenario") + ylab("Closed entities")

RegionalStats <- 

png("Graphs and figures/RegionalStats.png", width = 800, height=500)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()



