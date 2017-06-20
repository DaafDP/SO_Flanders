##Processing of VLM data livestock and animal housing Flanders

#Loading Data
Production <- read.csv(file="Productie.csv")
Stables <- read.csv(file="Stallen.csv")

##Exploring Data
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


