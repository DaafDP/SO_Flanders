##Processing of VLM data livestock and animal housing Flanders
rm(list=ls())

#1. Loading Data
Production <- read.csv(file="Productie.csv")
Stables <- read.csv(file="Stallen.csv")

#2. Exploring Data
#number of unique instances (farmers, exploitations, ...) in data
length(unique(Production$CO_NIS_FGEM_EXPLOITATIE)) #310 municipalities
length(unique(Production$NR_LANDBOUWER_FICT)) #19717 farmers
length(unique(Production$NR_EXPLOITANT_FICT)) #19830 operators
length(unique(Production$NR_EXPLOITATIE_FICT)) #22125 exploitations
       
length(unique(Stables$CO_NIS_FGEM_EXPLOITATIE)) #317 municipalities
length(unique(Stables$NR_LANDBOUWER_FICT)) #21033 farmers
length(unique(Stables$NR_EXPLOITANT_FICT)) #21158 operators
length(unique(Stables$NR_EXPLOITATIE_FICT)) #23929 exploitations

#AnimalCategories
unique(Production$OMS_DIERGROEP) #varkens, paarden, runderen, pluimvee, andere
length(unique(Production$CODE_DIERSOORT)) #38 diercategoriën
length(unique(Stables$CODE_DIERSOORT)) #38 diercategoriën
table(Production$OMS_DIERGROEP)
table(Production$OMS_DIERSOORT)

#Stable types
length(unique(Stables$CODE_STALTYPE)) #88 staltypes
table(Stables$CODE_STALTYPE)

#Operator versus farmer
length(setdiff(Production$NR_LANDBOUWER_FICT, Production$NR_EXPLOITANT_FICT)) 
        #139 non-mactches farmer and operator
length(setdiff(Stables$NR_LANDBOUWER_FICT, Stables$NR_EXPLOITANT_FICT)) 
        #137 non-matches farmer and operator

#3. Data processing
library(tidyr)

#Only keep columns that are strictly necessary
#OMS DIERSOORT instead of CODE_DIERSOORT (more detail, eg milk production)
SelCols <- c("CO_NIS_FGEM_EXPLOITATIE", "NR_LANDBOUWER_FICT", "NR_EXPLOITATIE_FICT",
             "OMS_DIERSOORT", "CODE_DIERSOORT", "CODE_STALTYPE", "AAN_BEZETTING_STALTYPE",
             "OMS_DIERGROEP","AAN_STANDPLAATSEN")
Stables_Short <- Stables[,SelCols]

#Filter out all observations with AAN_STANDPLAATSEN 0 of NA
Stables_Short <- subset(Stables_Short, AAN_STANDPLAATSEN > 0)

#Sum number of permitted animals for same exploitations and animal type
StablesAggregated <- aggregate(Stables_Short$AAN_STANDPLAATSEN,
                           by=list(Stables_Short$CO_NIS_FGEM_EXPLOITATIE,
                                   Stables_Short$NR_LANDBOUWER_FICT, 
                                   Stables_Short$NR_EXPLOITATIE_FICT, 
                                   Stables_Short$CODE_DIERSOORT,
                                   Stables_Short$CODE_STALTYPE), FUN=sum)

colnames(StablesAggregated) <- c("NIS", "Farmer", "Exploitation",
                                 "AnimalType", "StableType", "Capacity")

StablesAggregated$AnimalType <- paste("a", StablesAggregated$AnimalType, sep="")

#CodeBooks
NIS <- cbind(unique(Stables$CO_NIS_FGEM_EXPLOITATIE), as.character(unique(Stables$GEMEENTE_EXPLOITATIE)))
NIS <- data.frame(NIS)
colnames(NIS) <- c("Code", "Municipality")

StableTypes <- cbind(unique(as.character((Stables$CODE_STALTYPE))), as.character(
        unique(Stables$OMS_STALTYPE)))
StableTypes <- data.frame(StableTypes)
colnames(StableTypes) <- c("Code", "StableType")

#Spread data
StablesWide <- spread(StablesAggregated, key=AnimalType, value= Capacity)

#SortData (NIS, Farmer, Exploitation)
StablesWide <- StablesWide[order(StablesWide$NIS, StablesWide$Farmer, 
                                 StablesWide$Exploitation),]

StablesWide$Name <- paste("s", c(1:nrow(StablesWide)), sep="")
length(unique(StablesWide$Exploitation)) #23422 exploitations
length(unique(StablesWide$Farmer)) #20721 farmers

#Replace NA by zero
StablesWide[is.na(StablesWide)] <- 0

#4. Save resulting dataframe as 
write.csv(StablesWide, "StablesWide.csv", row.names = FALSE)

