*=============================================================================
* File      : SpatialOptimizationNotEconomic.gms
* Author    : David De Pue
* Version   : 1.0
* Date      : 04-August-2017
* Changed   :
* Changed
* Remarks:
*Dataset VLM farms Flanders (permitted animals, stable type)
*coordinates randomly assigned based on NIS code and Landbouwgebruikspercelen
*EMAV ammonia stable emissions
*Hoedje dataset (IFDM, VITO, 20*20 km², resolutie 100 m, meteo 2012 Luchtbal)
*Deposition velocities VLOPS
*Data preprocessing and making of gdx file in R (GDX.R, see Github)


*===============================================================================
*=======================Data preprocessing and data input=======================
*===============================================================================
Set
sStable /s1*s44540/
sExploitation /e1*e23408/
sNIS
sStableType
sFarmer /f1*f20709/
sID(sStable, sExploitation, sNIS, sFarmer, sStableType)
sAnimalCategory
sCoordinates /X, Y/
sImpactScores /ADS, SS/
sSectors /Runderen, Varkens,  Pluimvee,  Andere, Paarden/
sSectors_AnimalCategory(sSectors, sAnimalCategory)
sScen /sc1, sc2, sc3/
sScenario(sScen)
;

$gdxin FarmsFlanders1.gdx
$load sNIS, sStableType, sID, sAnimalCategory, sSectors_AnimalCategory
$gdxin

Parameters
pAnimals(sStable, sAnimalCategory)
pLocationImpact(sExploitation, sImpactscores)
pEmissionFactor(sAnimalCategory, sStableType)
pSourceLocation(sExploitation, sCoordinates)
;

*FarmFlanders.gdx: seed 1 R (allocation sources)
*FarmFlanders2.gdx: seed2 R
*FarmFlanders3.gdx: seed3 R
$gdxin FarmsFlanders1.gdx
$load pAnimals, pLocationImpact, pEmissionFactor, pSourceLocation
$gdxin

$ontext
Parameter pCombination(sStable, sStableType)    all stable-stable type combinations that occur in the dataset   ;

pCombination(sSource,sStableType)$(pSourceID(sSource, sStableType, 'Exploitation') ne 0) = 1 ;
$offtext

*******************************************************************************
********************Misc. Initial Parameters and MultiDsets*********************
********************************************************************************
Set sClass /Green, Orange, Red/
    sStable_Exploitation(sStable, sExploitation) Stables that belong to same exploitation (assumed same point location)
    sExploitation_Farmer(sExploitation, sFarmer) Exploitations that belong to certain farmer
    sStable_StableType(sStable, sStableType)
    sStable_NIS(sStable, sNIS)
    sExploitation_NIS(sExploitation, sNIS)
;

option sStable_Exploitation <= sID  ;
option sExploitation_Farmer <= sID ;
option sStable_StableType <= sID ;
option sStable_NIS <= sID ;
option sExploitation_NIS <= sID ;

Parameter
iPermittedAnimals(sExploitation) number of types of animals that are permitted in exploitation
iMaxAmmoniaSource(sExploitation)
iMaxAmmoniaRegion
iMaxSS(sExploitation)
iFarmColour(sExploitation) Classify Stable according to ANB colour - assuming maximum capacity 1:green 2:orange 3: red
iTotalClassNumbers(sClass)
iMaxADS(sExploitation)
iMaxADSRegion
;

iPermittedAnimals(sExploitation) = sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory), rel_ne(pAnimals(sStable, sAnimalCategory),0))  ;


iMaxAmmoniaSource(sExploitation) =  sum((sStable_Exploitation(sStable, sExploitation), sStable_StableType(sStable, sStableType), sAnimalCategory), (pEmissionFactor(sAnimalCategory, sStableType) *
                                 pAnimals(sStable, sAnimalCategory))) ;

iMaxAmmoniaRegion = sum(sExploitation, iMaxAmmoniaSource(sExploitation));
iMaxSS(sExploitation) =   (iMaxAmmoniaSource(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS') ;

iMaxADS(sExploitation) =  (iMaxAmmoniaSource(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS') ;
iMaxADSRegion = sum(sExploitation, iMaxADS(sExploitation))      ;

$exit

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
********************************Model*******************************************
********************************************************************************

Variables
vAmmoniaEmissionRegion
vAmmoniaEmissionExploitation(sExploitation)  ;

Positive  variable
vAnimals(sStable, sAnimalCategory) ;

Equations
eqAnimals(sStable, sAnimalCategory) Permit constraint: maximum number of animals per stable
eqAmmoniaEmissionSource(sExploitation) ammonia emission per exploitation
eqAmmoniaEmissionRegion objective function
;

eqAmmoniaEmissionSource(sExploitation)..
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sStable_StableType(sStable, sStableType), sAnimalCategory),
(pEmissionFactor(sAnimalCategory, sStableType) * vAnimals(sStable, sAnimalCategory))) ;

**Constraint
eqAnimals(sStable, sAnimalCategory)..
vAnimals(sStable, sAnimalCategory) =l= pAnimals(sStable, sAnimalCategory) ;

**Objective
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sExploitation,vAmmoniaEmissionExploitation(sExploitation))    ;

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

$batinclude Reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario2.MODELSTAT         ;
pSolveSTat = Scenario2.SOLVESTAT         ;


execute_unload 'sc2.gdx'

*-------------------------------------------------------------------------------
**Scenario 3: Effectivity check, minimize ADS, emission bigger than sc1---------
*-------------------------------------------------------------------------------

Variable
vTotalADS
;

Equation
eqTotalADSRegionSc3
eqAmmoniaFloor ;

eqTotalADSRegionSc3..
vTotalADS =e= sum(sExploitation, (vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS')) ;

eqAmmoniaFloor..
vAmmoniaEmissionRegion =g= dEmissionRegion('sc1') ;

pCurrentScenario = 3      ;

Model Scenario3 /Scenario1 - eqSignificanceScore + eqTotalADSRegionSc3 + eqAmmoniaFloor/ ;

Option lp = CPLEX ;

Solve Scenario3 using lp minimizing vTotalADS ;

$batinclude reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario3.MODELSTAT         ;
pSolveSTat = Scenario3.SOLVESTAT         ;

execute_unload 'sc3.gdx'

********************************************************************************
**********Writing GDX with all relevant data************************************
********************************************************************************
execute_unload 'results.gdx' dSignificanceScore, dADS, dTotalADS, dPercentageOccupied, dPercentageOccupiedRegion, dClosedStables,dClosedExploitations,dEmissionStable, dEmissionExploitation, dEmissionRegion, dAnimals, dStableTypesNIS, dAnimalsNIS, dAnimalGroupNIS, dEmissionNIS, dADSNIS, dEmissionAnimal, dEmissionStableType, dEmissionAnimalGroup, dMarginalSignificanceScore, dMaxEmissionStable, dMaxEmissionExploitation, dMaxEmissionNIS, dMaxEmissionAnimalCategory, dMaxEmissionSector, dMaxEmissionStableType, dAnimalsGroup, dMaxAnimalsGroup

