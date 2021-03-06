##Random coupling of VLM data to locations of stables, based on NIS codes
rm(list=ls())

#Working Directory
setwd("C:/Users/ddpue/Documents/Spatial Optimization Flanders/DataHandling_VLM/")

#1.Loading Data
StablesWide <- read.csv("StablesWide.csv")
Locations <- read.csv("LocationStables.csv")

#2. Cleaning data.frame Locations
Locations <- Locations[,c("X", "Y", "NISCODE", "NAAM", "ADS", "SS")]

#3.Unique Exploitations For which location is sought
Exploitations <- StablesWide[,c("NIS", "Exploitation")]
Exploitations <- unique(Exploitations)

#4. Remove Walloon and Brussels locations from VLM dataset
NIS <- unique(Exploitations$NIS)
length(NIS) #315 municipalities in VLM dataset
length(unique(Locations$NISCODE)) #305 municipalities in locations dataset
WalloBrux <- StablesWide$NIS %in% setdiff(NIS, Locations$NISCODE)
StablesWide <- StablesWide[WalloBrux==FALSE,]
Exploitations <- StablesWide[,c("NIS", "Exploitation")]
Exploitations <- unique(Exploitations)
NIS <- unique(Exploitations$NIS)

#5.Assign locations to exploitations, based on random pick from municapilities
#three random picks (different seeds)
sample1 <- 
        sapply(NIS, function(x) {
                print(x)
                SelectedExploitations <- subset(Exploitations, NIS==x)
                SelectedLocations <- subset(Locations, NISCODE==x)
                set.seed(1)
                #If available locations bigger than exploitations, random pick without replacement:
                if (nrow(SelectedLocations) >= nrow(SelectedExploitations)){
                        index <- sample(1:nrow(SelectedLocations), nrow(SelectedExploitations))
                        #print(index)
                }
                #Else: pick random without replacement until locations are 'empty', than take other 
                #picks from locations with replacement
                else {  print(x)
                        ind <- sample(1:nrow(SelectedLocations), nrow(SelectedLocations))
                        ex <- sample(1:nrow(SelectedLocations),
                                     (nrow(SelectedExploitations)-nrow(SelectedLocations)), replace=TRUE)
                        index <- c(ind, ex)
                        print(index)
                }
                return(SelectedLocations[index,c("X", "Y", "ADS", "SS")])
        })

sample1 <- t(sample1)

Exploitations_seed1 <- data.frame(matrix(unlist(sample1), nrow=nrow(Exploitations), byrow=F))
colnames(Exploitations_seed1) <- c("X", "Y", "ADS", "SS")
Exploitations_seed1 <- as.data.frame(cbind(Exploitations_seed1, Exploitations))


sample2 <- 
        sapply(NIS, function(x) {
                print(x)
                SelectedExploitations <- subset(Exploitations, NIS==x)
                SelectedLocations <- subset(Locations, NISCODE==x)
                set.seed(2)
                #If available locations bigger than exploitations, random pick without replacement:
                if (nrow(SelectedLocations) >= nrow(SelectedExploitations)){
                        index <- sample(1:nrow(SelectedLocations), nrow(SelectedExploitations))
                        #print(index)
                }
                #Else: pick random without replacement until locations are 'empty', than take other 
                #picks from locations with replacement
                else {  print(x)
                        ind <- sample(1:nrow(SelectedLocations), nrow(SelectedLocations))
                        ex <- sample(1:nrow(SelectedLocations),
                                     (nrow(SelectedExploitations)-nrow(SelectedLocations)), replace=TRUE)
                        index <- c(ind, ex)
                        print(index)
                }
                return(SelectedLocations[index,c("X", "Y", "ADS", "SS")])
        })
sample2 <- t(sample2)

Exploitations_seed2 <- data.frame(matrix(unlist(sample2), nrow=nrow(Exploitations), byrow=F))
colnames(Exploitations_seed2) <- c("X", "Y", "ADS", "SS")
Exploitations_seed2 <- as.data.frame(cbind(Exploitations_seed2, Exploitations))

sample3 <- 
        sapply(NIS, function(x) {
                print(x)
                SelectedExploitations <- subset(Exploitations, NIS==x)
                SelectedLocations <- subset(Locations, NISCODE==x)
                set.seed(3)
                #If available locations bigger than exploitations, random pick without replacement:
                if (nrow(SelectedLocations) >= nrow(SelectedExploitations)){
                        index <- sample(1:nrow(SelectedLocations), nrow(SelectedExploitations))
                        #print(index)
                }
                #Else: pick random without replacement until locations are 'empty', than take other 
                #picks from locations with replacement
                else {  print(x)
                        ind <- sample(1:nrow(SelectedLocations), nrow(SelectedLocations))
                        ex <- sample(1:nrow(SelectedLocations),
                                     (nrow(SelectedExploitations)-nrow(SelectedLocations)), replace=TRUE)
                        index <- c(ind, ex)
                        print(index)
                }
                return(SelectedLocations[index,c("X", "Y", "ADS", "SS")])
        })
sample3 <- t(sample3)

Exploitations_seed3 <- data.frame(matrix(unlist(sample3), nrow=nrow(Exploitations), byrow=F))
colnames(Exploitations_seed3) <- c("X", "Y", "ADS", "SS")
Exploitations_seed3 <- as.data.frame(cbind(Exploitations_seed3, Exploitations))

#Control samples: 3x randomly allocated without taking into account NIS code
set.seed(1)
Random1 <- sample(c(1:nrow(Locations)), nrow(Exploitations))
Random1<- Locations[Random1,]
Random1 <- Random1[,c("X","Y","ADS","SS")]
Random1 <- as.data.frame(cbind(Random1, Exploitations))

set.seed(2)
Random2 <- sample(c(1:nrow(Locations)), nrow(Exploitations))
Random2 <- Locations[Random2,]
Random2 <- Random2[,c("X", "Y", "ADS", "SS")]
Random2 <- as.data.frame(cbind(Random2, Exploitations))

set.seed(3)
Random3 <- sample(c(1:nrow(Locations)), nrow(Exploitations))
Random3 <- Locations[Random3,]
Random3 <- Random3[,c("X", "Y", "ADS", "SS")]
Random3 <- as.data.frame(cbind(Random3, Exploitations))

#6. Couple X and Y coordinates to stables dataframe, coupling based on exploitation number
Stables_seed1 <- merge(StablesWide, Exploitations_seed1[,c("X","Y", "Exploitation", "ADS", "SS")], 
                       by ="Exploitation")
Stables_seed2 <- merge(StablesWide, Exploitations_seed2[,c("X","Y", "Exploitation", "ADS", "SS")], 
                       by ="Exploitation")
Stables_seed3 <- merge(StablesWide, Exploitations_seed3[,c("X","Y", "Exploitation", "ADS", "SS")], 
                       by ="Exploitation")

Random_seed1 <- merge(StablesWide, Random1[,c("X","Y", "Exploitation", "ADS", "SS")], 
                      by ="Exploitation")
Random_seed2 <- merge(StablesWide, Random2[,c("X","Y", "Exploitation", "ADS", "SS")], 
                      by ="Exploitation")
Random_seed3 <- merge(StablesWide, Random3[,c("X","Y", "Exploitation", "ADS", "SS")], 
                      by ="Exploitation")

#7. Rename the observations in the dataset
Stables_seed1$Name <- paste("s", 1:nrow(Stables_seed1), sep="")
Stables_seed2$Name <- paste("s", 1:nrow(Stables_seed2), sep="")
Stables_seed3$Name <- paste("s", 1:nrow(Stables_seed3), sep="")
Random_seed1$Name <- paste("s", 1:nrow(Random_seed1), sep="")
Random_seed2$Name <- paste("s", 1:nrow(Random_seed2), sep="")
Random_seed3$Name <- paste("s", 1:nrow(Random_seed3), sep="")

#8. Save as .csv
write.csv(Stables_seed1, "StablesS1.csv")
write.csv(Stables_seed2, "StablesS2.csv")
write.csv(Stables_seed3, "StablesS3.csv")
write.csv(Random_seed1, "RandomStablesS1.csv")
write.csv(Random_seed2, "RandomStablesS2.csv")
write.csv(Random_seed3, "RandomStablesS3.csv")