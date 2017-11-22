*=============================================================================
* File      : SpatialOptimizationNotEconomic.gms
* Author    : David De Pue
* Version   : 1.0
* Start Date: 11-October-2017
* Changed   :
* Changed
* Remarks:
*Dataset VLM farms Flanders (permitted animals, stable type)
*coordinates randomly assigned based on NIS code and Landbouwgebruikspercelen
*EMAV ammonia stable emissions
*Hoedje dataset (IFDM, VITO, 20*20 km², resolutie 100 m, meteo 2012 Luchtbal)
*Deposition velocities VLOPS
*Gross margins from department of Agriculture and Fisheries
*Data preprocessing and making of gdx file in R (GDX.R, see Github)


*===============================================================================
*=======================Data preprocessing and data input=======================
*===============================================================================
Set
sStable /s1*s44540/
*sStable /s1*s109/
sExploitation /e1*e23408/
*sExploitation /e1*e68/
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
pAEA(sStableType, sAnimalCategory, sCost)
pAEA_Ref(sStableType, sAnimalCategory, sCost)
pReductionEFPAS(sPAS, sAnimalCategory)
pCostPAS(sPAS, sAnimalCategory, sCost)
pGM(sFarmType, sAnimalCategory, sStatistic)
;

*FarmFlanders.gdx: seed 1 R (allocation sources)
*FarmFlanders2.gdx: seed2 R
*FarmFlanders3.gdx: seed3 R
$gdxin FarmsFlanders1.gdx
$load pAnimals, pLocationImpact, pEmissionFactor, pSourceLocation, pAEA, pAEA_Ref, pReductionEFPAS, pCostPAS, pGM
$gdxin

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
iPermittedAnimals(sExploitation) number of types of animals that are permitted in exploitation
iMaxAmmoniaStable(sStable, sStableType)
iMaxAmmoniaSource(sExploitation)
iMaxAmmoniaRegion
iMaxSS(sExploitation)
iFarmColour(sExploitation) Classify Stable according to ANB colour - assuming maximum capacity 1:green 2:orange 3: red
iTotalClassNumbers(sClass)
iMaxADS(sExploitation)
iMaxADSRegion
iWorstCaseRegion The worst stable types for all stables (highest emission factors)
;

iPermittedAnimals(sExploitation) = sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory), rel_ne(pAnimals(sStable, sAnimalCategory),0))  ;

iMaxAmmoniaStable(sStable, sStableType) = sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType))) ;

iMaxAmmoniaSource(sExploitation) =  sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory), smax(sStableType, (pEmissionFactor(sAnimalCategory, sStableType)) *
                                 pAnimals(sStable, sAnimalCategory))) ;

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
*Tijdelijk manueel invoeren GM beefbulls (tot nieuwe computer en R beschikbaar)
pGM('BeefBulls', 'a117', 'GM') = 362 ;
pGM('BeefBulls', 'a117', 'std') = 120.67 ;

Parameter pGrossMargin(sExploitation, sAnimalCategory) ;

pGrossMargin(sExploitation, sAnimalCategory) = sum(sExploitation_FarmType(sExploitation, sFarmType), normal(pGM(sFarmType, sAnimalCategory, 'GM'), pGM(sFarmType, sAnimalCategory, 'std')))
;

pGrossMargin(sExploitation, 'a125')$(sExploitation_FarmType(sExploitation, 'PigRearing')) = normal(pGM('PigRearing', 'a125', 'GM'), pGM('PigRearing', 'a125', 'std'))
;

pGrossMargin(sExploitation, 'a123')$(sExploitation_FarmType(sExploitation, 'PigRearing')) = pGrossMargin(sExploitation, 'a125') ;
;

********************************************************************************
********************************Model*******************************************
********************************************************************************
*pAnimals(sStable, sAnimalCategory)

*pTest(sStable, sStableType, sAnimalCategory) = ;
* sum(sAnimalCategory_StableType(sAnimalCategory, sStableTye), 1) ;

Set
sPossibleStables(sStable, sStableType)
sTest(sStable, sStableType)
sOtherAnimals(sAnimalCategory) /a14, a132, a134, a141, a194, a195, a93, a98, a99, a91, a92, a151, a152, a153, a165, a166, a631, a632, a633, a641, a642, a643/
sOneStableAnimals(sAnimalCategory)  /a14, a141, a194, a195, a93, a91, a92, a151, a152, a153, a165, a166, a631, a632, a633, a641, a642, a643/;

sPossibleStables(sStable, sStableType) = sum(sAnimalCategory, (sAnimalCategory_StableType(sAnimalCategory, sStableType) * sAnimalCategory_Stable(sAnimalCategory, sStable)))
;

Parameter
pEmissionAll(sStable, sStableType)       ;
*pTest(sStable, sStableType, sAnimalCategory) ;

pEmissionAll(sPossibleStables(sStable, sStableType)) = sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType)))
;


*sPossibleStables(sStable_stableType(sStable, sStableType), sAnimalCategory)$()=no;



*only stables possible for which there is EF for all present animal categories
*sPossibleStables(sStable, sStableType, sAnimalCategory)$((pEmissionFactor(sAnimalCategory, sStableType) * sStable) = 0) = no ;
*;

Variable
vAmmoniaEmissionRegion
vAmmoniaEmissionExploitation(sExploitation)
 ;

positive variable
*vPASreduction(sStable, sAnimalCategory)
vEmissionAll(sStable, sStableType)
vAnimalsExploitation(sExploitation, sAnimalCategory)
vAmmoniaEmissionStableType(sStable, sStabletype)
vAnimals(sStable, sAnimalCategory)
;

Binary variable
vStable(sStable, sStableType) Stable type
*vPAS(sStable, sPAS) PAS measure ;
;

*initialize stable: stable from sID (original stable)
vStable.l(sPossibleStables(sStable, sStableType)) =  sStable_StableType(sStable, sStableType) ;
vStable.fx(sStable, sStableType)$(not sPossibleStables(sStable, sStableType)) = 0 ;

*$exit
*vStable.fx(sStable, sStableType)$(sum(sPossibleStables(sStable, sStableType), 1) = 1)  = sStable_StableType(sStable, sStableType)


*initialize PAS: in beginning, no farm applies PAS measure
*vPAS.l(sStable, sPAS) =0 ;

*fix animals for which we don't have economic data: no farm type, or animal type 'others'
vAnimalsExploitation.fx(sExploitation, sAnimalCategory)$(not sum(sFarmType, sExploitation_FarmType(sExploitation, sFarmType)) and (pAnimalExploitation(sExploitation, sAnimalCategory) > 0)) = pAnimalExploitation(sExploitation, sAnimalCategory) ;

vAnimalsExploitation.fx(sExploitation, sOtherAnimals)$(pAnimalExploitation(sExploitation, sOtherAnimals) > 0) = pAnimalExploitation(sExploitation, sOtherAnimals) ;

Equations
eqStable(sStable)
*eqPAS(sStable)
eqAnimals(sStable, sAnimalCategory) Permit constraint: maximum number of animals per stable
eqAnimalsExploitation(sExploitation, sAnimalCategory) Animals on exploitation level
eqAmmoniaEmissionSource(sExploitation) ammonia emission per exploitation
eqAmmoniaEmissionAllStableTypes(sStable, sStableType)
eqAnimalReduction(sStable, sStableType)
eqAmmoniaEmissionRegion
eqAnimalRatio(sExploitation, sAnimalCategory)
*eqInvestmentCostExploitation(sExploitation)
*eqPASreduction(sStable, sAnimalCategory)
*eqEF(sAnimalCategory, sStable)
eqAmmoniaExploitationConstraint(sExploitation)
;

eqStable(sStable)..
sum(sStableType, vStable(sStable, sStableType)) =e= 1 ;

*sum(sStableType, vStable(sStable, sStableType)) =e= 1 ;

*vAnimals.l(sStable, sAnimalCategory) = pAnimals(sStable, sAnimalCategory) ;

*Permit Constraint
eqAnimals(sStable, sAnimalCategory)..
vAnimals(sStable, sAnimalCategory) =l= pAnimals(sStable, sAnimalCategory)    ;
* sum(sAnimalCategory_StableType(sAnimalCategory, sStableTye), 1) ;

*$ontext
*eerste poging
eqAmmoniaEmissionAllStableTypes(sPossibleStables(sStable, sStableType))..
vEmissionAll(sStable, sStableType) =e= pEmissionAll(sStable, sStableType) * vStable(sStable, sStableType) ;



eqAnimalReduction(sPossibleStables(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStableType) =e= vEmissionAll(sStable, sStableType)
 -  sum(sAnimalCategory, ((pAnimals(sStable, sAnimalCategory) - vAnimals(sStable, sAnimalCategory)) * pEmissionFactor(sAnimalCategory, sStableType))) ;
*$offtext

*tweede poging
$ontext
Equation
eqAmmoniaStableStableType(sStable, sStableType)
eqAmmoniaStableStableType2(sStable, sStableType)
;

eqAmmoniaStableStableType(sPossibleStables(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStabletype) =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType)));

eqAmmoniaStableStableType2(sPossibleStables(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStabletype) =l= vStable(sPossibleStables) * 100000000      ;
$offtext

eqAmmoniaEmissionSource(sExploitation)..
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sStableType), vAmmoniaEmissionStableType(sStable, sStableType)) ;

*Total ammonia emission
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sExploitation,vAmmoniaEmissionExploitation(sExploitation));

*AmmoniaConstraint
eqAmmoniaExploitationConstraint(sExploitation)..
vAmmoniaEmissionExploitation(sExploitation) =l= iMaxAmmoniaSource(sExploitation) ;

*Linking stables with exploitations
eqAnimalsExploitation(sExploitation, sAnimalCategory)..
vAnimalsExploitation(sExploitation, sAnimalCategory) =e= sum(sStable_Exploitation(sStable, sExploitation), vAnimals(sStable, sAnimalCategory));

*Ratios of animals within exploitations
eqAnimalRatio(sExploitation, sAnimalCategory)..
vAnimalsExploitation(sExploitation, sAnimalCategory)$(sum(sAnimalCategory2, pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)) > 0)
=e= sum(sAnimalCategory2, (vAnimalsExploitation(sExploitation, sAnimalCategory2) * pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2))) ;
*$offtext

Model test /all/ ;

*test.OptFile = 1 ;

*option mip = GAMSCHK ;

option mip=CPLEX    ;

Solve test maximizing vAmmoniaEmissionRegion using mip ;

Parameter dSumStableTypes(sStableType), dInitialStableType(sStableType), dSum1, dSum2 ;

dSumStableTypes(sStableType) = sum(sStable, vStable.l(sStable, sStableType)) ;
dInitialStableType(sStableType) = sum(sStable_StableType(sStable, sStableType), 1) ;
dSum1 = sum(sStableType, dInitialStableType(sStableType))      ;
dSum2 = sum(sStableType, dSumStableTypes(sStableType)) ;

$exit

$ontext
*Ammonia emission per stable without PAS measure
*z < A * x
eqAmmoniaEmissionStable1(sStable, sStableType)$(sum(sPossibleStables(sStable, sStableType), 1) = 1)..
vAmmoniaEmissionStableType(sStable, sStabletype)  =l= (iMaxAmmoniaStable(sStable, sStableType)) * vStable(sStable, sStableType) ;

*sum(sStableType, (pAnimals(sStable, sAnimalCategory) * vStable(sStable, sStableType)
* * pEmissionFactor(sAnimalCategory, sStableType)))  ;

* =g= sum(sStable_StableType(sStable, sStableType), (pEmissionFactor(sAnimalCategory, sStableType)*vAnimals(sStable, sAnimalCategory))) -  sum(sStable_StableType(sStable, sStableType), (iMaxAmmoniaStableAnimalCategory(sStable, sAnimalCategory)*(1-vStable(sStable, sStableType)))) ;


*vAmmoniaEmissionStable(sStable, sAnimalCategory) =l= sum (sStableType, (iMaxAmmoniaStableAnimalCategory(sStable, sAnimalCategory) * vStable(sStable, sStableType))) ;

*z <= a
eqAmmoniaEmissionStable2(sStable, sStableType)$(sum(sPossibleStables(sStable, sStableType), 1) = 1) ..
vAmmoniaEmissionStableType(sStable, sStabletype)  =l= vEmissionAll(sStable, sStableType) ;

*z >= a - (1-x)A
eqAmmoniaEmissionStable3(sStable, sStableType)$(sum(sPossibleStables(sStable, sStableType), 1) = 1) ..
vAmmoniaEmissionStableType(sStable, sStabletype)  =g= vEmissionAll(sStable, sStableType)
 - ((1-vStable(sStable, sStableType))*(iMaxAmmoniaStable(sStable, sStableType))) ;




*$ontext
eqPASreduction(sStable, sAnimalCategory)..
vPASreduction(sStable, sAnimalCategory) =e= sum(sPAS, (1 - (pReductionEFPAS(sPAS, sAnimalCategory)/100)*vPAS(sStable, sPAS))) * vAnimals(sStable, sAnimalCategory) ;
;

*Ammonia emission per exploitation
eqAmmoniaEmissionSource(sExploitation)..

vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory, sStable_StableType(sStable, sStableType)),
(vAmmoniaEmissionStable(sStable, sAnimalCategory) - vPASreduction(sStable, sAnimalCategory)) *  vStable(sStable, sStableType)    ) ;
*$offtext

*$ontext
eqAmmoniaEmissionSource(sExploitation)..
*Ammonia Emission depending on stable Type
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory, sStable_StableType(sStable, sStableType), sPAS),
(pEmissionFactor(sAnimalCategory, sStableType) * vAnimals(sStable, sAnimalCategory) * vStable(sStable, sStableType)
*
*Reduction From PAS measure
 * ((vPAS(sStable, sPAS) -(pReductionEFPAS(sPAS, sAnimalCategory)/100)* vPAS(sStable, sPAS))
)))    ;

$offtext

*vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sStable_StableType(sStable, sStableType), sAnimalCategory),
*(pEmissionFactor(sAnimalCategory, sStableType) * vAnimals(sStable, sAnimalCategory))) ;



*Total ammonia emission
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sExploitation,vAmmoniaEmissionExploitation(sExploitation));

Model test /all/ ;

*test.OptFile = 1 ;

*option mip = GAMSCHK ;

option mip=CPLEX    ;

Solve test minimizing vAmmoniaEmissionRegion using mip ;

Parameter dSumStableTypes(sStableType), dInitialStableType(sStableType) ;



$exit

$ontext
*Total Investment Cost
Variable vInvestmentCost(sExploitation) ;

eqInvestmentCostExploitation(sExploitation)..
vInvestmentCost(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory, sStable_Stabletype(sStable, sStableType)),
(pAEA_Ref(sStableType, sAnimalCategory, 'InvPerDP') * vStable(sStable, sStableType) * vAnimals(sStable, sAnimalCategory))) ;

$offtext

*$exit

*$ontext
*TestModule
Variable
AnimalNumber ;

Equation
eqTotalAnimals ;

eqTotalAnimals..
AnimalNumber =e= sum((sStable, sAnimalCategory), vAnimals(sStable, sAnimalCategory))  ;

Model test /all/ ;

Option mip = CPLEX ;

Solve test maximizing AnimalNumber using mip ;
*$offtext





$exit

********************************Scenario Analysis*******************************
*===============================================================================

*-------------------------------------------------------------------------------
*Scenario 1: <5% CL in  CHC (Reference)-----------------------------------------
*-------------------------------------------------------------------------------
Parameter
pCurrentScenario       ;

pCurrentScenario = 1       ;

Equations
eqSignificanceScore(sExploitation) Significance Score constraint
;


eqSignificanceScore(sExploitation)..
(vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS') =l= 5 ;

Model Scenario1 /eqAnimals, eqAmmoniaEmissionSource, eqAmmoniaEmissionRegion, eqSignificanceScore/          ;

Option lp = CPLEX ;

Solve Scenario1 maxmizing vAmmoniaEmissionRegion using lp ;

$batinclude Reporting.gms

parameter
pModelStat, pSolveStat       ;
pModelStat = Scenario1.MODELSTAT         ;
pSolveSTat = Scenario1.SOLVESTAT         ;

execute_unload 'sc1.gdx'

$ontext
*-------------------------------------------------------------------------------
*Scenario 2: Efficiency check: Total ADS max.(sc1), max. vAmmoniaEmisison, no individual farm constraints
*-------------------------------------------------------------------------------
Equations
eqTotalADSRegion
;

pCurrentScenario = 2       ;

*lower than total impact from previous model
eqTotalADSRegion..
sum(sExploitation, (vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS')) =l= dTotalADS('sc1') ;

Model Scenario2 /Scenario1 - eqSignificanceScore + eqTotalADSRegion/          ;

Option lp = CPLEX ;

Solve Scenario2 maxmizing vAmmoniaEmissionRegion using lp ;

*$batinclude Reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario2.MODELSTAT         ;
pSolveSTat = Scenario2.SOLVESTAT         ;


execute_unload 'sc2.gdx'

$offtext

*-------------------------------------------------------------------------------
**Scenario 2: Effectivity check, minimize ADS, emission bigger than sc1---------
*-------------------------------------------------------------------------------

Variable
vTotalADS
;

Equation
eqTotalADSRegionSc2
eqAmmoniaFloor ;

eqTotalADSRegionSc2..
vTotalADS =e= sum(sExploitation, (vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS')) ;

eqAmmoniaFloor..
vAmmoniaEmissionRegion =g= dEmissionRegion('sc1') ;

pCurrentScenario = 2      ;

Model Scenario2 /Scenario1 - eqSignificanceScore + eqTotalADSRegionSc2 + eqAmmoniaFloor/ ;

Option lp = CPLEX ;

Solve Scenario2 using lp minimizing vTotalADS ;

$batinclude reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario2.MODELSTAT         ;
pSolveSTat = Scenario2.SOLVESTAT         ;

execute_unload 'sc3.gdx'

*-------------------------------------------------------------------------------
**Scenario 3: PAN - Close red farms, allow orange farm to perform according to best practice (lowest emission factor)
*If no improvement is possible: close
*-------------------------------------------------------------------------------

Equation
eqOrange(sExploitation) Best practice (lowest EF)
eqOrangeNoImprovement(sExploitation) If orange farm already best practice: close
eqRed(sExploitation) Red farms: close
eqAmmoniaEmissionSc2(sExploitation) Scen2 - Green farms - Orange farms before renewal - red farms before renewal
;

eqOrange(sExploitation)$(iFarmColour(sExploitation)= 2)..
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory),
(pBestEF(sAnimalCategory) * vAnimals(sStable, sAnimalCategory))) ;

eqAmmoniaEmissionSc2(sExploitation)$(iFarmColour(sExploitation) = 1)..
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sStable_StableType(sStable, sStableType), sAnimalCategory),
(pEmissionFactor(sAnimalCategory, sStableType) * vAnimals(sStable, sAnimalCategory))) ;

eqOrangeNoImprovement(sExploitation)$(sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory), (pBestEF(sAnimalCategory) * pAnimals(sStable, sAnimalCategory))) = iMaxAmmoniaSource(sExploitation))..
vAmmoniaEmissionExploitation(sExploitation)$(iFarmColour(sExploitation) = 2) =e= 0 ;

eqRed(sExploitation)$(iFarmColour(sExploitation)=3)..
vAmmoniaEmissionExploitation(sExploitation) =e= 0 ;

pCurrentScenario = 3      ;

Model Scenario3 /Scenario1 - eqSignificanceScore - eqAmmoniaEmissionSource + eqAmmoniaEmissionSc2  + eqOrange + eqOrangeNoImprovement + eqRed/ ;

Option lp = CPLEX ;

Solve Scenario3 using lp maxmizing vAmmoniaEmissionRegion ;

$batinclude reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario3.MODELSTAT         ;
pSolveSTat = Scenario3.SOLVESTAT         ;

execute_unload 'sc3.gdx'

*-------------------------------------------------------------------------------
*Scenario 4: Best practice animal housing for all exploitations at permit renewal, all farms remain in business with same capacity
*-------------------------------------------------------------------------------
Equation
eqBestPractice(sExploitation) All farms become best practice
;

eqBestPractice(sExploitation)..
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory),
(pBestEF(sAnimalCategory) * vAnimals(sStable, sAnimalCategory))) ;

pCurrentScenario = 4      ;

Model Scenario4 /Scenario1 - eqSignificanceScore - eqAmmoniaEmissionSource + eqBestPractice/ ;

Option lp = CPLEX ;

Solve Scenario4 using lp maxmizing vAmmoniaEmissionRegion ;

$batinclude reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario4.MODELSTAT         ;
pSolveSTat = Scenario4.SOLVESTAT         ;

execute_unload 'sc4.gdx'

********************************************************************************
**********Writing GDX with all relevant data************************************
********************************************************************************
execute_unload 'results.gdx' dSignificanceScore, dADS, dTotalADS, dPercentageOccupied, dPercentageOccupiedRegion, dClosedStables,dClosedExploitations,dEmissionStable, dEmissionExploitation, dEmissionRegion, dAnimals, dStableTypesNIS, dAnimalsNIS, dAnimalGroupNIS, dEmissionNIS, dADSNIS, dEmissionAnimal, dEmissionStableType, dEmissionAnimalGroup, dMarginalSignificanceScore, dMaxEmissionStable, dMaxEmissionExploitation, dMaxEmissionNIS, dMaxEmissionAnimalCategory, dMaxEmissionSector, dMaxEmissionStableType, dAnimalsGroup, dMaxAnimalsGroup

