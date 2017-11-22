##Retrieving gross margins from FADN 
#Including variance (STD)

#Clear environment
rm(list=ls())

#Working Directory
setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataEconomic//")

#Reading in data
BEL2010 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2010.csv")
BEL2011 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2011.csv")
BEL2012 <- read.csv("S:/team_jb/staff/ddpue/FADN/BEL2012.csv")

#Select Flanders
VLA2010 <- subset(BEL2010, A1==341)
VLA2011 <- subset(BEL2011, A1==341)
VLA2012 <- subset(BEL2012, A1==341)

#Select non-organic farms (because of specificity organic production)
VLA2010 <- subset(VLA2010, A32==1)
VLA2011 <- subset(VLA2011, A32==1)
VLA2012 <- subset(VLA2012, A32==1)

#1. Rearing pigs (piglets, sows, boars).
##2010
#D44AV:number of sows
#SE206: Total output livestock&livestock products
#SE281: Total specific Costs
R_Pigs <- subset(VLA2010, D44AV>75 & D46AV <300) #103 observations
mean(R_Pigs$D44AV) #194 sows on averarage

outputsow <- lm(SE206~D44AV, data=R_Pigs)
outputpiglet <- lm(SE206~D43AV, data=R_Pigs)

output <- lm(SE206~D22AV + D23AV + D24AV + D25AV+ D26AV + D27AV + D28AV + D29AV + D30AV
             +D31AV + D32AV + D33AV + D34AV + D35AV +D36AV + D37AV + D38AV + D39AV +
                     D40AV + D41AV + D43AV + D44AV + D45AV + D46AV + D47AV + D48AV +
                     D49AV + D50AV, data=R_Pigs)
summary(outputsow) #1368 euro/sow

costs <- lm(SE281~D30AV      +D43AV + D44AV + D45AV + D46AV + D47AV, data=R_Pigs) 
cost <- lm(SE281~D44AV, data=R_Pigs)
summary(cost) #1106 euro/sow

