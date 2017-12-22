rm(list=ls())

#Packages and GAMS directory
library(gdxrrw)
library(xlsx)
igdx("C:/GAMS/win64/24.6/")

Ex_FT <- rgdx.set('SODat.gdx', 'sExploitation_FarmType')
Ex_FT$zscore <- rnorm(n=nrow(Ex_FT), mean = 0, sd=1)

AMS <- read.xlsx("C:/Users/ddpue/Documents/Werk/Spatial Optimization Flanders/DataEconomic/GegevensAMS_versie2.xlsx", sheetIndex = 1)
AMS <- subset(AMS, Titel == 3000)

