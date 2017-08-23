##Reading in  results from non-economic simulation on all 
##livestock exploitations in Flanders

#Required packages + defining GAMS directory 
library(gdxrrw)
library(reshape2)
library(ggplot2)
library(scales)
igdx("C:/GAMS/win64/24.7/")

#Clear environment
rm(list = ls())

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

#Read in results (NIS-based, seed 1)



