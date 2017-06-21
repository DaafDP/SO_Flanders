##Random coupling of VLM data to locations of stables, based on NIS codes

#1.Loading Data
StablesWide <- read.csv("StablesWide.csv")
Locations <- read.csv("LocationStables.csv")

#2. Cleaning data.frame Locations
Locations <- Locations[,c("X", "Y", "NISCODE", "NAAM")]

#3.Unique Exploitations For which location is sought
Exploitations <- StablesWide[,c("NIS", "Exploitation")]
Exploitations <- unique(Exploitations)

#3. Remove Walloon and Brussels locations from VLM dataset
NIS <- unique(Exploitations$NIS)
length(NIS) #315 municipalities in VLM dataset
length(unique(Locations$NISCODE)) #305 municipalities in locations dataset
WalloBrux <- StablesWide$NIS %in% setdiff(NIS, Locations$NISCODE)
StablesWide <- StablesWide[WalloBrux==FALSE,]
Exploitations <- StablesWide[,c("NIS", "Exploitation")]
Exploitations <- unique(Exploitations)
NIS <- unique(Exploitations$NIS)

#5.Assign locations to exploitations, based on random pick from municapilities
#Two random picks (different seeds)
sample1 <- 
        sapply(NIS, function(x) {
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
        else {   print(x)
                 ind <- sample(1:nrow(SelectedLocations), nrow(SelectedLocations))
                 ex <- sample(1:nrow(SelectedLocations),
                             (nrow(SelectedExploitations)-nrow(SelectedLocations)), replace=TRUE)
                 index <- c(ind, ex)
                 print(index)
        }
        return(SelectedLocations[index,1:2])
})

Exploitations_seed1 <- data.frame(matrix(unlist(sample1), nrow=nrow(Exploitations), byrow=T))
colnames(Exploitations_seed1) <- c("X", "Y")
Exploitations_seed1 <- as.data.frame(cbind(Exploitations_seed1, Exploitations))


sample2 <- 
        sapply(NIS, function(x) {
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
                return(SelectedLocations[index,1:2])
        })

Exploitations_seed2 <- data.frame(matrix(unlist(sample2), nrow=nrow(Exploitations), byrow=T))
colnames(Exploitations_seed2) <- c("X", "Y")
Exploitations_seed2 <- as.data.frame(cbind(Exploitations_seed2, Exploitations))



#6. Couple X and Y coordinates to stables dataframe, coupling based on exploitation number
Stables_seed1 <- merge(StablesWide, Exploitations_seed1[,c("X","Y", "Exploitation")], 
                       by ="Exploitation")
Stables_seed2 <- merge(StablesWide, Exploitations_seed2[,c("X","Y", "Exploitation")], 
                       by ="Exploitation")

#7. Rename the observations in the dataset
Stables_seed1$Name <- paste("s", 1:nrow(Stables_seed1), sep="")
Stables_seed2$Name <- paste("s", 1:nrow(Stables_seed1), sep="")

#8. Save as .csv
write.csv(Stables_seed1, "StablesS1.csv")
write.csv(Stables_seed2, "StablesS2.csv")
