*=============================================================================
* File      : SpatialOptimizationNotEconomic.gms
* Author    : David De Pue
* Version   : 2.0
* Start Date: 11-October-2017
* Changed   : 20-November-2017
* Changed
* Remarks:
*Dataset VLM farms Flanders (permitted animals, stable type)
*coordinates randomly assigned based on NIS code and Landbouwgebruikspercelen
*EMAV ammonia stable emissions
*Hoedje dataset (IFDM, VITO, 20*20 km², resolutie 100 m, meteo 2012 Luchtbal)
*Deposition velocities VLOPS
*Gross margins from department of Agriculture and Fisheries
*Data preprocessing and making of gdx file in R (GDX.R, see Github)
*Futher processing in GAMS (SO_Preprocessing.gms)




********************************************************************************
********************************Data Input**************************************
********************************************************************************
$batinclude SO_Preprocessing.gms


********************************************************************************
********************************Model*******************************************
********************************************************************************


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

*vAnimals.fx(sStable, sAnimalCategory) = pAnimals(sStable, sAnimalCategory) ;

vEmissionAll.fx(sStable, sStableType)$(not sStableChoice(sStable, sStableType)) = 0 ;
vAmmoniaEmissionStableType.fx(sStable, sStabletype)$(not sStableChoice(sStable, sStableType)) = 0 ;
;

Binary variable
vStable(sStable, sStableType) Stable type
vPAS(sStable, sPAS) PAS measure ;
;

*initialize stable: stable from sID (original stable)
vStable.l(sPossibleStables(sStable, sStableType)) =  sStable_StableType(sStable, sStableType) ;
vStable.fx(sStable, sStableType)$(not sPossibleStables(sStable, sStableType)) = sStable_StableType(sStable, sStableType) ;

*initialize PAS: in beginning, no farm applies PAS measure
vPAS.l(sStable, sPAS) =0 ;
vPAS.fx(sStable, sPAS)$(not sStable_PAS(sStable, sPAS)) = 0 ;

*fix animals for which we don't have economic data: no farm type, or animal type 'others'
vAnimalsExploitation.fx(sExploitation, sAnimalCategory)$(not sum(sFarmType, sExploitation_FarmType(sExploitation, sFarmType)) and (pAnimalExploitation(sExploitation, sAnimalCategory) > 0) and sFix(sExploitation)) = pAnimalExploitation(sExploitation, sAnimalCategory) ;

vAnimalsExploitation.fx(sExploitation, sOtherAnimals)$((pAnimalExploitation(sExploitation, sOtherAnimals) > 0) and sFix(sExploitation)) = pAnimalExploitation(sExploitation, sOtherAnimals) ;

Equations
eqStable(sStable)
eqPAS(sStable)
eqAnimals(sStable, sAnimalCategory) Permit constraint: maximum number of animals per stable
eqAnimalsExploitation(sExploitation, sAnimalCategory) Animals on exploitation level
eqAmmoniaEmissionSource(sExploitation) ammonia emission per exploitation
eqAmmoniaEmissionRegion
eqAnimalRatio(sExploitation, sAnimalCategory)
eqAmmoniaExploitationConstraint(sExploitation)
;

eqStable(sStable)..
sum(sStableType, vStable(sStable, sStableType)) =e= 1 ;

eqPAS(sStable)$(sum(sPAS, sStable_PAS(sStable, sPAS)))..
sum(sPAS, vPAS(sStable, sPAS)) =l= 1 ;


*Permit Constraint
eqAnimals(sStable, sAnimalCategory)..
vAnimals(sStable, sAnimalCategory) =l= pAnimals(sStable, sAnimalCategory)    ;
* sum(sAnimalCategory_StableType(sAnimalCategory, sStableTye), 1) ;

*https://www.leandro-coelho.com/linearization-product-variables/
Equation
eqEmissionAll(sStable, sStableType)
eqAmmoniaStableStableType1(sStable, sStableType)
eqAmmoniaStableStableType2(sStable, sStableType)
eqAmmoniaStableStableType3(sStable, sStableType)
;

eqEmissionAll(sStableChoice(sStable, sStableType))..
vEmissionAll(sStable, sStableType) =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType)))     ;

eqAmmoniaStableStableType1(sStableChoice(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStabletype) =l= iWorstCaseStable(sStable) * vStable(sStable, sStableType) ;

eqAmmoniaStableStableType2(sStableChoice(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStabletype)  =l= vEmissionAll(sStable, sStableType) ;

eqAmmoniaStableStableType3(sStableChoice(sStable, sStableType))..
vAmmoniaEmissionStableType(sStable, sStabletype) =g= vEmissionAll(sStable, sStableType) - (1- vStable(sStable, sStableType)) * iWorstCaseStable(sStable) ;

*$ontext
*PASmeasures
Positive Variable
vReductionPASall(sStable, sPAS)
vReductionPAS(sStable, sPAS)
vAmmoniaEmissionStable(sStable) ;

Equation
eqReductionPASall(sStable, sPAS)
eqReductionPAS1(sStable, sPAS)
eqReductionPAS2(sStable, sPAS)
eqReductionPAS3(sStable, sPAS)
eqAmmoniaEmissionStable_PAS(sStable)
;

vReductionPAS.fx(sStable, sPAS)$(not sStable_PAS(sStable, sPAS)) = 0 ;

eqReductionPASall(sStable_PAS(sStable, sPAS))..
vReductionPASall(sStable, sPAS)  =e= sum((sAnimalCategory, sStableType), (vAmmoniaEmissionStableType(sStable, sStabletype) * (pReductionEFPAS(sPAS, sAnimalCategory)/100))) ;

eqReductionPAS1(sStable_PAS(sStable, sPAS))..
vReductionPAS(sStable, sPAS) =l= vPAS(sStable, sPAS) * iWorstCaseStable(sStable) ;

eqReductionPAS2(sStable_PAS(sStable, sPAS))..
vReductionPAS(sStable, sPAS) =l= vReductionPASall(sStable, sPAS) ;

eqReductionPAS3(sStable_PAS(sStable, sPAS))..
vReductionPAS(sStable, sPAS) =g= vReductionPASall(sStable, sPAS)  - (1-vPAS(sStable, sPAS)) * iWorstCaseStable(sStable) ;

eqAmmoniaEmissionStable_PAS(sStable)..
vAmmoniaEmissionStable(sStable) =e= sum(sStableType, vAmmoniaEmissionStableType(sStable, sStableType)) - sum(sPAS, vReductionPAS(sStable, sPAS))   ;
*$offtext


eqAmmoniaEmissionSource(sExploitation)..
*vAmmoniaEmissionExploitation(sExploitation) =e= sum(sStable_Exploitation(sStable, sExploitation), vAmmoniaEmissionStable(sStable)) ;
vAmmoniaEmissionExploitation(sExploitation) =e= sum((sStable_Exploitation(sStable, sExploitation), sStableType), (vAmmoniaEmissionStableType(sStable, sStableType))) ;
*- vAmmoniaEmissionStable_PAS(sStable, sStableType))) ;

*Total ammonia emission
eqAmmoniaEmissionRegion..
vAmmoniaEmissionRegion =e= SUM(sExploitation,vAmmoniaEmissionExploitation(sExploitation));

*AmmoniaConstraint
eqAmmoniaExploitationConstraint(sExploitation)..
vAmmoniaEmissionExploitation(sExploitation) =l= iMaxAmmoniaSource(sExploitation) ;

*Linking stables with exploitations
eqAnimalsExploitation(sExploitation, sAnimalCategory)$(sum(sStable, pAnimals(sStable, sAnimalCategory)) > 0)..
vAnimalsExploitation(sExploitation, sAnimalCategory) =e= sum(sStable_Exploitation(sStable, sExploitation), vAnimals(sStable, sAnimalCategory));


*Ratios of animals within exploitations
eqAnimalRatio(sExploitation, sAnimalCategory)..
vAnimalsExploitation(sExploitation, sAnimalCategory)$(sum(sAnimalCategory2, pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)) > 0)
=e= sum(sAnimalCategory2, (vAnimalsExploitation(sExploitation, sAnimalCategory2) * pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2))) ;
*$offtext

Equation
eqInvestmentAEAAll(sStable, sStableType)
eqInvestmentAEA1(sStable, sStableType)
eqInvestmentAEA2(sStable, sStableType)
eqInvestmentAEA3(sStable, sStableType)
;

positive variable
vInvestmentAEAall(sStable, sStableType)
vInvestmentAEA(sStable, sStableType)
;

eqInvestmentAEAAll(sStable, sStableType)$(pInvestmentAEAAll(sStable, sStableType) > 0)..
vInvestmentAEAall(sStable, sStableType) =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pAEA(sAnimalCategory, sStableType, 'InvPerDP')))     ;

eqInvestmentAEA1(sStable, sStableType)$(pInvestmentAEAAll(sStable, sStableType) > 0)..
vInvestmentAEA(sStable, sStableType) =l= vStable(sStable, sStableType) * pInvestmentAEAAll(sStable, sStableType)     ;

eqInvestmentAEA2(sStable, sStableType)$(pInvestmentAEAAll(sStable, sStableType) > 0)..
vInvestmentAEA(sStable, sStableType) =l= vInvestmentAEAall(sStable, sStableType) ;

eqInvestmentAEA3(sStable, sStableType)$(pInvestmentAEAAll(sStable, sStableType) > 0)..
vInvestmentAEA(sStable, sStableType) =g=   vInvestmentAEAall(sStable, sStableType) - (1 - vStable(sStable, sStableType)) *   pInvestmentAEAAll(sStable, sStableType) ;

Equation
eqYearlyAEAAll(sStable, sStableType)
eqYearlyAEA1(sStable, sStableType)
eqYearlyAEA2(sStable, sStableType)
eqYearlyAEA3(sStable, sStableType)
;

positive variable
*add small extra amount to prevent Stable Choice for empty stables
vYearlyAEAall(sStable, sStableType)
vYearlyAEA(sStable, sStableType)
;

eqYearlyAEAAll(sStable, sStableType)$(pYearlyAEAAll(sStable, sStableType) > 0)..
vYearlyAEAall(sStable, sStableType) =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pAEA(sAnimalCategory, sStableType, 'JaarkostDP')))     ;

eqYearlyAEA1(sStable, sStableType)$(pYearlyAEAAll(sStable, sStableType) > 0)..
vYearlyAEA(sStable, sStableType) =l= vStable(sStable, sStableType) * pYearlyAEAAll(sStable, sStableType) ;

eqYearlyAEA2(sStable, sStableType)$(pYearlyAEAAll(sStable, sStableType) > 0)..
vYearlyAEA(sStable, sStableType) =l= vYearlyAEAall(sStable, sStableType) ;

eqYearlyAEA3(sStable, sStableType)$(pYearlyAEAAll(sStable, sStableType) > 0)..
vYearlyAEA(sStable, sStableType) =g=   vYearlyAEAall(sStable, sStableType) - (1 - vStable(sStable, sStableType)) * pYearlyAEAAll(sStable, sStableType) ;

Equation
eqInvestmentPASAll(sStable, sPAS)
eqInvestmentPAS1(sStable, sPAS)
eqInvestmentPAS2(sStable, sPAS)
eqInvestmentPAS3(sStable, sPAS)
;

positive variable
vInvestmentPASall(sStable, sPAS)
vInvestmentPAS(sStable, sPAS)
;

eqInvestmentPASAll(sStable, sPAS)$(pInvestmentPASAll(sStable, sPAS) > 0)..
vInvestmentPASall(sStable, sPAS)   =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pCostPAS(sPAS, sAnimalCategory,'InvPerDP')))     ;

eqInvestmentPAS1(sStable, sPAS)$(pInvestmentPASAll(sStable, sPAS) > 0)..
vInvestmentPAS(sStable, sPAS) =l= vPAS(sStable, sPAS) * pInvestmentPASAll(sStable, sPAS) ;

eqInvestmentPAS2(sStable, sPAS)$(pInvestmentPASAll(sStable, sPAS) > 0)..
vInvestmentPAS(sStable, sPAS)=l= vInvestmentPASall(sStable, sPAS) ;

eqInvestmentPAS3(sStable, sPAS)$(pInvestmentPASAll(sStable, sPAS) > 0)..
vInvestmentPASall(sStable, sPAS) =g=  vInvestmentPASall(sStable, sPAS)  - (1 - vPAS(sStable, sPAS)) *  pInvestmentPASAll(sStable, sPAS) ;

Equation
eqYearlyPASAll(sStable, sPAS)
eqYearlyPAS1(sStable, sPAS)
eqYearlyPAS2(sStable, sPAS)
eqYearlyPAS3(sStable, sPAS)
;

positive variable
vYearlyPASall(sStable, sPAS)
vYearlyPAS(sStable, sPAS)
;

eqYearlyPASAll(sStable, sPAS)$(pYearlyPASAll(sStable, sPAS) > 0)..
vYearlyPASall(sStable, sPAS)   =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pCostPAS(sPAS, sAnimalCategory,'JaarKostDP')))     ;

eqYearlyPAS1(sStable, sPAS)$(pYearlyPASAll(sStable, sPAS) > 0)..
vYearlyPAS(sStable, sPAS) =l= vPAS(sStable, sPAS) * pYearlyPASAll(sStable, sPAS) ;

eqYearlyPAS2(sStable, sPAS)$(pYearlyPASAll(sStable, sPAS) > 0)..
vYearlyPAS(sStable, sPAS)=l= vYearlyPASall(sStable, sPAS) ;

eqYearlyPAS3(sStable, sPAS)$(pYearlyPASAll(sStable, sPAS) > 0)..
vYearlyPASall(sStable, sPAS) =g=  vYearlyPASall(sStable, sPAS)  - (1 - vPAS(sStable, sPAS)) *  pYearlyPASAll(sStable, sPAS) ;

*$ontext
$ontext
Positive Variable
vInvestmentAEA(sStable, sStableType)
vYearlyAEA(sStable, sStableType)
vInvestmentPAS(sStable, sPAS)
vYearlyPAS(sStable, sPAS)
;

*Investment Cost Stables
*https://www.leandro-coelho.com/linearization-product-variables/
Equation
eqInvestmentAEA(sStable, sStableType)
eqYearlyAEA(sStable, sStableType)
eqInvestmentPAS(sStable, sPAS)
eqYearlyPAS(sStable, sPAS)
;

eqInvestmentAEA(sStable, sStableType)$(pInvestmentAEAAll(sStable, sStableType) > 0)..
vInvestmentAEA(sStable, sStableType) =e= pInvestmentAEAall(sStable, sStableType) * vStable(sStable, sStableType) ;

eqYearlyAEA(sStable, sStableType)$(pYearlyAEAAll(sStable, sStableType) > 0)..
vYearlyAEA(sStable, sStableType) =e= pYearlyAEAall(sStable, sStableType) * vStable(sStable, sStableType) ;

eqInvestmentPAS(sStable_PAS(sStable, sPAS))..
vInvestmentPAS(sStable, sPAS) =e= pInvestmentPASAll(sStable, sPAS) * vPAS(sStable, sPAS) ;

eqYearlyPAS(sStable_PAS(sStable, sPAS))..
vYearlyPAS(sStable, sPAS) =e=  pYearlyPASAll(sStable, sPAS) * vPAS(sStable, sPAS) ;

$offtext

Variables
vRevenue(sExploitation) Revenue animals
vProfitExploitation(sExploitation) Profit animals + cost investment AEA & PAS + operational cost AEA & PAS(5years)
vProfitTotal

Equation
eqRevenue(sExploitation)
eqOverallProfitExploitation(sExploitation)
eqOverallProfit
;

eqRevenue(sExploitation)..
vRevenue(sExploitation) =e=  sum(sAnimalCategory, (vAnimalsExploitation(sExploitation, sAnimalCategory)*  pGrossMargin(sExploitation, sAnimalCategory)))
                                 + sum(sAnimalCategory, (pPenalty(sExploitation, sAnimalCategory)* vAnimalsExploitation(sExploitation, sAnimalCategory)));


*1 year - no time-discounting
$ontext
eqOverallProfitExploitation(sExploitation)..
vProfitExploitation(sExploitation) =e= vRevenue(sExploitation) -
sum((sStableType, sStable_Exploitation(sStable, sExploitation)), (vInvestmentAEA(sStable, sStableType) + vYearlyAEA(sStable, sStableType))) ;
$offText

*$ontext
*5years, calculation Net present value with r=0.08
eqOverallProfitExploitation(sExploitation)..
vProfitExploitation(sExploitation) =e= - sum((sStableType, sStable_Exploitation(sStable, sExploitation)), vInvestmentAEA(sStable, sStableType))
- sum((sStable_Exploitation(sStable, sExploitation), sPAS), vInvestmentPAS(sStable, sPAS))
+ (vRevenue(sExploitation)
-sum((sStableType, sStable_Exploitation(sStable, sExploitation)), vYearlyAEA(sStable, sStableType))
 - sum((sPAS, sStable_Exploitation(sStable, sExploitation)), vYearlyPAS(sStable, sPAS)))*(((1.08**5)-1) /(0.08*(1.08**5))) ;
*$offtext

eqOverallProfit..
vProfitTotal =e= sum(sExploitation, vProfitExploitation(sExploitation)) ;
*sum(sExploitation, vProfitExploitation(sExploitation)) ;

*$offtext

$ontext
Model test /all/ ;

*test.OptFile = 1 ;

option mip = GAMSCHK ;

option mip=CPLEX    ;

option optcr = 0 ;

*Solve test maximizing vAmmoniaEmissionRegion using mip ;
Solve test maximizing vProfitTotal using mip ;

Parameter dSumStableTypes(sStableType), dInitialStableType(sStableType), dSum1, dSum2 ;

dSumStableTypes(sStableType) = sum(sStable, vStable.l(sStable, sStableType)) ;
dInitialStableType(sStableType) = sum(sStable_StableType(sStable, sStableType), 1) ;
dSum1 = sum(sStableType, dInitialStableType(sStableType))      ;
dSum2 = sum(sStableType, dSumStableTypes(sStableType)) ;

$offtext
*$exit

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
(vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS') =l= pSStreshold ;

Model Scenario1 /all/          ;

Option lp = CPLEX ;

option optcr = 0 ;

Solve Scenario1 maximizing vProfitTotal using mip ;

*$batinclude Reporting.gms

*If model changed stable type (vStable), but the new stable is empty (AEA cost: 0), than we change the solution back to the initial stable
vStable.l(sStable, sStableType)$(vInvestmentAEA.l(sStable, sStableType) = 0) = sStable_StableType(sStable, sStableType) ;

parameter
pModelStat, pSolveStat,dSumStableTypes(sStableType), dInitialStableType(sStableType), dSum1, dSum2, dTotalADS      ;

pModelStat = Scenario1.MODELSTAT  ;
pSolveSTat = Scenario1.SOLVESTAT  ;
dSumStableTypes(sStableType) = sum(sStable, vStable.l(sStable, sStableType)) ;
dInitialStableType(sStableType) = sum(sStable_StableType(sStable, sStableType), 1) ;
dSum1 = sum(sStableType, dInitialStableType(sStableType))      ;
dSum2 = sum(sStableType, dSumStableTypes(sStableType)) ;
dTotalADS = sum(sExploitation, (vAmmoniaEmissionExploitation.l(sExploitation) * pLocationImpact(sExploitation, 'ADS'))) ;

execute_unload 'sc1bis.gdx'

$exit

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

