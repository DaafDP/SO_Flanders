*===============================================================================
*=======================Data preprocessing and data input=======================
*===============================================================================
Set
*sStable /s119*s173/
*sExploitation /e75*e100/
*sStable /s1*s44540/
*sStable /s1*s173/
sStable/s1*s3655/
*sExploitation /e1*e23408/
*sExploitation /e1*e100/
sExploitation/e1*e2000/
sNIS
sStableType
sFarmer /f1*f20709/
sID(sStable, sExploitation, sNIS, sFarmer, sStableType)
sAnimalCategory
sCoordinates /X, Y/
sImpactScores /ADS, SS/
sSectors /Runderen, Varkens,  Pluimvee,  Andere, Paarden/
sSectors_AnimalCategory(sSectors, sAnimalCategory)
sScen /sc1*sc4/
sScenario(sScen)
sCost /InvPerDP, JaarKostDP/
sPAS
sFarmType
sStatistic /GM, std/
;

$gdxin FarmsFlanders1.gdx
$load sNIS, sStableType, sID, sAnimalCategory, sSectors_AnimalCategory, sPAS, sFarmType
$gdxin

Parameters
pAnimals(sStable, sAnimalCategory)
pLocationImpact(sExploitation, sImpactscores)
pEmissionFactor(sAnimalCategory, sStableType)
pSourceLocation(sExploitation, sCoordinates)
pAEAcost(sAnimalCategory, sStableType,  sCost)
pAEAextracost(sAnimalCategory, sStableType, sCost)
pReductionEFPAS(sPAS, sAnimalCategory)
pCostPAS(sPAS, sAnimalCategory, sCost)
pGM(sFarmType, sAnimalCategory, sStatistic)
;

*FarmFlanders.gdx: seed 1 R (allocation sources)
*FarmFlanders2.gdx: seed2 R
*FarmFlanders3.gdx: seed3 R
$gdxin FarmsFlanders1.gdx
$load pAnimals, pLocationImpact, pEmissionFactor, pSourceLocation, pAEAcost, pAEAextracost, pReductionEFPAS, pCostPAS, pGM
$gdxin

pAEAextracost(sAnimalCategory, sStableType, sCost) = abs(pAEAextracost(sAnimalCategory, sStableType, sCost)) ;

*******************************************************************************
********************Misc. Initial Parameters and MultiDsets*********************
********************************************************************************
Set sClass /Green, Orange, Red/
    sStable_Exploitation(sStable, sExploitation) Stables that belong to same exploitation (assumed same point location)
    sExploitation_Farmer(sExploitation, sFarmer) Exploitations that belong to certain farmer
    sStable_StableType(sStable, sStableType)
    sStable_NIS(sStable, sNIS)
    sExploitation_NIS(sExploitation, sNIS)
    sAnimalCategory_Stable(sAnimalCategory, sStable)
    sAnimalCategory_Stabletype(sAnimalCategory, sStableType)
;

option sStable_Exploitation <= sID  ;
option sExploitation_Farmer <= sID ;
option sStable_StableType <= sID ;
option sStable_NIS <= sID ;
option sExploitation_NIS <= sID ;
option sAnimalCategory_Stable<= pAnimals ;
option sAnimalCategory_Stabletype <= pEmissionFactor ;

Parameter
iPermittedStable(sStable) number of animal categories that are permitted in exploitation
iPermittedAnimals(sExploitation) number of types of animals that are permitted in exploitation
iMaxAmmoniaStable(sStable)
iWorstCaseStable(sStable)
iMaxAmmoniaSource(sExploitation)
iMaxAmmoniaRegion
iMaxSS(sExploitation)
iFarmColour(sExploitation) Classify Stable according to ANB colour - assuming maximum capacity 1:green 2:orange 3: red
iTotalClassNumbers(sClass)
iMaxADS(sExploitation)
iMaxADSRegion
iWorstCaseRegion The worst stable types for all stables (highest emission factors)
;

iPermittedStable(sStable) = sum(sAnimalCategory, rel_ne(pAnimals(sStable, sAnimalCategory),0)) ;

iPermittedAnimals(sExploitation) = sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory), rel_ne(pAnimals(sStable, sAnimalCategory),0))  ;

iMaxAmmoniaStable(sStable) = sum((sStable_StableType(sStable, sStableType),sAnimalCategory), (pEmissionFactor(sAnimalCategory, sStableType)) *
                                 pAnimals(sStable, sAnimalCategory))      ;

iWorstCaseStable(sStable) = sum(sAnimalCategory, smax(sStableType, (pEmissionFactor(sAnimalCategory, sStableType))) *
                                 pAnimals(sStable, sAnimalCategory))      ;

*$exit
iMaxAmmoniaSource(sExploitation) =  sum(sStable_Exploitation(sStable, sExploitation), iMaxAmmoniaStable(sStable)) ;

iMaxAmmoniaRegion = sum(sExploitation, iMaxAmmoniaSource(sExploitation));
iMaxSS(sExploitation) =   (iMaxAmmoniaSource(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS') ;

iWorstCaseRegion = sum((sAnimalCategory, sStable_Exploitation(sStable, sExploitation)), (pAnimals(sStable, sAnimalCategory) * smax(sStableType, pEmissionFactor(sAnimalCategory, sStableType))));

iMaxADS(sExploitation) =  (iMaxAmmoniaSource(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS') ;
iMaxADSRegion = sum(sExploitation, iMaxADS(sExploitation))      ;

Loop(sClass,
iTotalClassNumbers(sClass) = 0
) ;

Loop(sExploitation,
If (iMaxSS(sExploitation) > 50,
   iFarmColour(sExploitation) = 3 ;
   iTotalClassNumbers('Red') = iTotalClassNumbers('Red')+1 ;
Elseif iMaxSS(sExploitation) <5,
   iFarmColour(sExploitation) = 1 ;
   iTotalClassNumbers('Green') = iTotalClassNumbers('Green')+1 ;
Else
   iFarmColour(sExploitation) = 2 ;
   iTotalClassNumbers('Orange') = iTotalClassNumbers('Orange')+1 ;  )
   ;
) ;

********************************************************************************
*******************************Defining Farmtype per stable*********************
********************************************************************************
set sExploitation_FarmType(sExploitation, sFarmType) ;

parameter pNumberofFarmsperFarmType(sFarmType) ;

*Closed PigFattening: first select all farms with both fattening pigs and sows
*Then keep farms with ratio sows/fatteningpigs > 0.25
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123'))>0)  = yes ;
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a125'))=0)  = no ;
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124'))=0)  = no ;

sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$((sum(sStable_Exploitation(sStable, sExploitation), (pAnimals(sStable, 'a123') + pAnimals(sStable, 'a125')))
                                                                      / sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124')))<0.25 ) = no ;

*pNumberofFarmersperFarmType('ClosedPigFattening') = card(sExploitation_FarmType(sExploitation, 'ClosedPigFattening')) ;

*PigRearing: farms with sows, but not closed pigfattening
sExploitation_FarmType(sExploitation, 'PigRearing')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123'))>0)  = yes ;
sExploitation_FarmType(sExploitation, 'PigRearing') = sExploitation_FarmType(sExploitation, 'PigRearing') -  sExploitation_FarmType(sExploitation, 'ClosedPigFattening')        ;

*Fattening pigs: all farms with fattening pigs that don't belong to closed pig fattening
sExploitation_FarmType(sExploitation, 'PigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124'))>0)  = yes ;
sExploitation_FarmType(sExploitation, 'PigFattening') = sExploitation_FarmType(sExploitation, 'PigFattening') - sExploitation_FarmType(sExploitation, 'ClosedPigFattening') ;

*Dairy Farms: dairy cows present
sExploitation_FarmType(sExploitation, 'Dairy')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a1131'))>0) = yes ;

*Closed beef: suckling cows present
sExploitation_FarmType(sExploitation, 'ClosedBeef')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a1132'))>0) = yes ;

*Beef Bulls: rest category cattle (no closed beef, no dairy)
sExploitation_FarmType(sExploitation, 'BeefBulls')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a117'))>0) = yes ;
sExploitation_FarmType(sExploitation, 'BeefBulls')  = sExploitation_FarmType(sExploitation, 'BeefBulls') - sExploitation_FarmType(sExploitation, 'Dairy') - sExploitation_FarmType(sExploitation, 'ClosedBeef')    ;

*LayingHens: laying hens present
sExploitation_FarmType(sExploitation, 'LayingHens')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a133'))>0) = yes ;

*Broilers: broiler present
sExploitation_FarmType(sExploitation, 'Broilers')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a32'))>0) = yes ;

*Sheep: sheep present
sExploitation_FarmType(sExploitation, 'Sheep')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a43'))>0) = yes ;

pNumberofFarmsperFarmType(sFarmType) = sum(sExploitation_FarmType(sExploitation, sFarmType), 1) ;

********************************************************************************
*****************************Fix animal ratios depending on sector**************
********************************************************************************
*lock animal ratios per exploitation
alias(sAnimalCategory, sAnimalCategory2) ;

Parameter pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)
          pAnimalExploitation(sExploitation, sAnimalCategory) ;


pAnimalExploitation(sExploitation, sAnimalCategory) = sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sAnimalCategory)) ;

**Cattle
Set sYoungCattle(sAnimalCategory) /a111, a112, a115, a116, a117/
    sAdultCattle(sAnimalCategory) /a1131, a1132/
;

*Only Dairy
pAnimalRatio(sExploitation, sYoungCattle, 'a1131')$(not sExploitation_FarmType(sExploitation, 'ClosedBeef') and sExploitation_FarmType(sExploitation, 'Dairy'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sYoungCattle))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a1131') ) ;

*Only Suckler (closed beef)
pAnimalRatio(sExploitation, sYoungCattle, 'a1132')$(not sExploitation_FarmType(sExploitation, 'Dairy') and sExploitation_FarmType(sExploitation, 'ClosedBeef'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sYoungCattle))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a1132') ) ;

*Both Dairy and Suckler Cows
pAnimalRatio(sExploitation, sYoungCattle, sAdultCattle)$(sExploitation_FarmType(sExploitation, 'Dairy') and sExploitation_FarmType(sExploitation, 'ClosedBeef'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sYoungCattle))
/ sum(sStable_Exploitation(sStable, sExploitation), (pAnimals(sStable, 'a1131') + pAnimals(sStable, 'a1132'))) ;

*BeefBulls
pAnimalRatio(sExploitation, sYoungCattle, 'a117')$(sExploitation_FarmType(sExploitation, 'BeefBulls'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sYoungCattle))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a117')) ;

**Pigs
Set sPigs(sAnimalCategory) /a121*a125/     ;

*ClosedPigFattening
pAnimalRatio(sExploitation, sPigs, 'a124')$(sExploitation_FarmType(sExploitation, 'ClosedPigFattening'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124') ) ;

*PigRearing or combination Pig Rearing/Pig Fattening
pAnimalRatio(sExploitation, sPigs, 'a123')$(sExploitation_FarmType(sExploitation, 'PigRearing'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123') ) ;

*Exclusive PigFattening
pAnimalRatio(sExploitation, sPigs, 'a124')$(not sExploitation_FarmType(sExploitation, 'PigRearing') and sExploitation_FarmType(sExploitation, 'PigFattening'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124') ) ;

*Sheep
pAnimalRatio(sExploitation, 'a42', 'a43')$(sExploitation_FarmType(sExploitation, 'Sheep'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a42'))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a43') ) ;

*LayingHens
Set sLayingHens(sAnimalCategory) /a132, a134/ ;

pAnimalRatio(sExploitation, sLayingHens, 'a133')$(sExploitation_FarmType(sExploitation, 'LayingHens'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sLayingHens))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a133') ) ;

*Broilers
Set sBroilers(sAnimalCategory) /a98, a99/ ;

pAnimalRatio(sExploitation, sBroilers, 'a32')$(sExploitation_FarmType(sExploitation, 'Broilers'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sBroilers))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a32') ) ;

pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)$(ord(sAnimalCategory) eq ord (sAnimalCategory2)) =   0 ;

********************************************************************************
*****************************Gross Margin from normal distribution**************
********************************************************************************
Parameter pGrossMargin(sExploitation, sAnimalCategory), pMaxProfit(sExploitation)     ;

pGrossMargin(sExploitation, sAnimalCategory)$(pAnimalExploitation(sExploitation, sAnimalCategory) > 0)= sum(sExploitation_FarmType(sExploitation, sFarmType), normal(pGM(sFarmType, sAnimalCategory, 'GM'), pGM(sFarmType, sAnimalCategory, 'std')))
;

pGrossMargin(sExploitation, 'a125')$((pGrossMargin(sExploitation, 'a125') > 0)) = pGrossMargin(sExploitation, 'a123')         ;


********************************************************************************
***********Stables that model can choose: initial stable + stable with cost info
********************************************************************************
Set
sAEA(sAnimalCategory, sStabletype)
*sInitialStab(sStable, sStableType)
sPosStab(sStable, sStableType, sAnimalCategory)
sPossibleStables(sStable, sStableType)
sOtherAnimals(sAnimalCategory) /a14, a132, a134, a141, a194, a195, a93, a98, a99, a91, a92, a151, a152, a153, a165, a166, a631, a632, a633, a641, a642, a643/
sOneStableAnimals(sAnimalCategory)  /a14, a141, a194, a195, a93, a91, a92, a151, a152, a153, a165, a166, a631, a632, a633, a641, a642, a643/
sNEA(sStableType) /TRAD_MENGM, TRAD_STALM, UNKNOWN, BATT_NEA, GRONDH_NEA, BATT_OV, GRONDH_OV, SLK_OV, SLKO_OV, OSLKO_OV/
sEA(sStableType)
sEligbleStablesAEA(sStable) stables for which choice of AEA can be made (NEA + pigs or poultry)
sStableChoice(sStable, sStableType) ;

option sAEA <= pAEAextraCost       ;

sEA(sStableType) = yes ;

sEA(sNEA) = no ;

sEligbleStablesAEA(sStable)   = sum(sSectors, (sum(sAnimalCategory, (sAnimalCategory_Stable(sAnimalCategory, sStable) * sSectors_Animalcategory('Varkens', sAnimalCategory)))
                                    + sum(sAnimalCategory, (sAnimalCategory_Stable(sAnimalCategory, sStable) * sSectors_Animalcategory('Pluimvee', sAnimalCategory)))));

sEligbleStablesAEA(sStable) = sum(sStableType, (sEligbleStablesAEA(sStable) * sStable_StableType(sStable, sStableType) * sNEA(sStableType))) ;



*option sInitialStab <= sID ;

sPosStab(sStable, sStableType, sAnimalCategory) =  sAEA(sAnimalCategory, sStableType) * sAnimalCategory_Stable(sAnimalCategory, sStable) ;

*sPosStab(sStable, sStableType, sAnimalCategory) =  sAnimalCategory_StableType(sAnimalCategory, sStableType) * sAnimalCategory_Stable(sAnimalCategory, sStable) ;

parameter nrAnimals(sStable, sStableType)
          nrAnimals2(sStable, sStableType) used for selection of stables that can house all animal categories present ;

nrAnimals(sStable, sStableType) = sum(sPosStab(sStable, sStableType, sAnimalCategory) , 1) ;

nrAnimals2(sStable, sStableType)$((iPermittedStable(sStable) = nrAnimals(sStable, sStableType)) and sEligbleStablesAEA(sStable))   = nrAnimals(sStable, sStableType)  ;

*option sStable_Exploitation <= sID  ;
option sPossibleStables <=  nrAnimals2 ;

*sPossibleStables(sEligbleStablesAEA(sStable), sStableType) = sPossibleStables(sStable, sStableType) + sStable_StableType(sStable, sStableType) ;

sStableChoice(sStable, sStableType) = sPossibleStables(sStable, sStableType) + sStable_StableType(sStable, sStableType) ;

********************************************************************************
******PAS measures that model can choose****************************************
********************************************************************************
Set
sPAS_AnimalCategory(sPAS, sAnimalCategory)
sStable_PAS(sStable, sPAS) ;

option sPAS_AnimalCategory <= pReductionEFPAS ;

sStable_PAS(sStable, sPAS) = sum(sAnimalCategory, (sPAS_AnimalCategory(sPAS, sAnimalCategory) * sAnimalCategory_stable(sAnimalCategory, sStable))) ;

Parameter
pEmissionAll(sStable, sStableType)
pInvestmentAEAAll(sStable, sStableType)
pYearlyAEAAll(sStable, sStableType)
pInvestmentPASAll(sStable, sPAS)
pYearlyPASAll(sStable, sPAS)
pMaxRevenue(sExploitation)
pMaxRevenueStable(sStable)
*pTest(sStable, sStableType, sAnimalCategory)
*pAEA_Max_Investment(sStable)
*pAEA_Max_Yearly(sStable)
*pPAS_Max_Investment(sStable)
*pPAS_Max_Yearly(sStable);
;

pEmissionAll(sStableChoice(sStable, sStableType)) = sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType)))
;

pInvestmentAEAAll(sEligbleStablesAEA, sEA) = sum(sAnimalCategory, (pAnimals(sEligbleStablesAEA, sAnimalCategory) * pAEAcost(sAnimalCategory, sEA, 'InvPerDP'))) ;

pYearlyAEAAll(sEligbleStablesAEA, sEA) = sum(sAnimalCategory, (pAnimals(sEligbleStablesAEA, sAnimalCategory) * pAEAextracost(sAnimalCategory, sEA, 'JaarKostDP'))) ;

pInvestmentPASAll(sStable_PAS(sStable, sPAS)) = sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pCostPAS(sPAS, sAnimalCategory, 'InvPerDP')))   ;

pYearlyPASAll(sStable_PAS(sStable, sPAS)) = sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pCostPAS(sPAS, sAnimalCategory, 'JaarKostDP')))    ;

pMaxRevenue(sExploitation) = sum(sAnimalCategory, (pAnimalExploitation(sExploitation, sAnimalCategory) * pGrossMargin(sExploitation, sAnimalCategory))) ;

pMaxRevenueStable(sStable) = sum(sAnimalCategory, pAnimals(sStable, sAnimalCategory) * sum(sStable_exploitation(sStable, sExploitation), pGrossMargin(sExploitation, sAnimalCategory))) ;

*pAEA_Max_Investment(sStable) = sum(sStableType, pInvestmentAEAAll(sStable, sStableType))    ;

*pPAS_Max_Yearly(sStable) = sum(sStableType, pYearlyAEAall(sStable, sStableType)

********************************************************************************
*********Fix animals when impact not over threshold*****************************
********************************************************************************
*Exploitations
Scalar pSStreshold /5/ ;

Set
sNotFix(sExploitation) ;

sNotFix(sExploitation)$((iMaxAmmoniaSource(sExploitation)/5000) * pLocationImpact(sExploitation, 'SS')>pSStreshold) = yes ;

execute_unloaddi 'SODat.gdx'
