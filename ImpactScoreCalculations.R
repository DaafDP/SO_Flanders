##Impact Score calculations for all exploitations
rm(list=ls())

#1. Loading data
Stables_seed1 <- read.csv("StablesS1.csv")
Stables_seed2 <- read.csv("StablesS2.csv")

Receptor <- read.delim("C:/Users/ddpue/Documents/GPBV Flanders/R/GPBVFlanders/Receptor.txt")
Hoedje <- read.csv("~/Regional Model/SpatialOptimization  GPBV Flanders/dataHoedje.csv")

#2. Sources (two seeds): unique exploitations/pairs of coordinates
Sources1 <- unique(Stables_seed1[,c("Exploitation", "X", "Y")])
Sources2 <- unique(Stables_seed2[,c("Exploitation", "X", "Y")])

Sources1$nr <- Sources2$nr <- 1:nrow(Sources1)

#3. Hoedje with seperate tables for Dry deposition and wet deposition
X <- c(-199:200) * 100
Y <- X
Hoedje <- Hoedje[,1:5]
colnames(Hoedje) <- c("ID", "X", "Y", "DD", "ND")
hoedjeDD <- as.data.frame(matrix(Hoedje$DD[1:160000], nrow=length(X)))
rownames(hoedjeDD) <- Y
colnames(hoedjeDD) <- X
hoedjeND <- as.data.frame(matrix(Hoedje$ND[1:160000], nrow=length(X)))
rownames(hoedjeND) <- Y
colnames(hoedjeND) <- X

#3.Impact indicator calculations
Scores1 <- apply(Sources1, 1, function(x)
{
        print(x[4])
        impacttable <- as.data.frame(matrix(ncol=9, nrow=nrow(Receptor)))
        colnames(impacttable) <- c("ID", "dep", "CL", "Vd", "IS", "X", "Y", "DD", "ND")
        impacttable$ID <- Receptor$ID
        impacttable$CL <- Receptor$CL
        impacttable$Vd <- Receptor$Vd
        impacttable$TND <- Receptor$TND
        #Convert to 'hoedje' coordinates
        impacttable$X <- Receptor$X - x[2]
        impacttable$Y <- Receptor$Y - x[3]
        #Round to 100m 
        impacttable$X <- round(impacttable$X/100)*100
        impacttable$Y <- round(impacttable$Y/100)*100
        #Create lookuptable 
        impacttable <- impacttable[which(impacttable$X <= 20000 & impacttable$X >= -19900),]
        impacttable <- impacttable[which(impacttable$Y <= 20000 & impacttable$Y >= -19900),]
        print(nrow(impacttable))
        mat <- apply(impacttable, 1, function(y){
                DD <- hoedjeDD[as.character(y[7]), as.character(y[6])]
                ND <- hoedjeND[as.character(y[7]), as.character(y[6])]
                cbind(DD, ND)
                #hoedje[which(hoedje$X==y[6] & hoedje$Y==y[7]),3:4]
        })
        mat <- data.frame(matrix(unlist(mat), nrow=nrow(impacttable),byrow=T))
        impacttable$DD <- mat$X1
        impacttable$ND <- mat$X2
        impacttable$dep <- (5000/8784)*(((impacttable$Vd/0.88)*impacttable$DD)+impacttable$ND)
        impacttable$IS <- impacttable$dep/impacttable$CL
        ADS <- sum(impacttable$IS)
        SS <- 100 * max(subset(impacttable, TND > CL, select = IS)) #CHC: TDN > CL!!
        return(cbind(ADS, SS))
        #return(((cbind(sum(impacttable$IS), (100*max((impacttable$IS)))))))
        
})

