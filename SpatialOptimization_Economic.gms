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
*;

Binary variable
vStable(sStable, sStableType) Stable type
vPAS(sStable, sPAS) PAS measure ;
;

vStable.l(sStable, sStableType) = sStable_StableType(sStable, sStableType) ;
*vStable.fx(sStable, sStableType) = sStable_StableType(sStable, sStableType) ;

*initialize stable: stable from sID (original stable)
vStable.fx(sStable, sStableType)$(not sStableChoice(sStable, sStableType)) = 0 ;

*initialize PAS: in beginning, no farm applies PAS measure
*vPAS.fx(sStable, sPAS) =0 ;
vPAS.l(sStable, sPAS) = 0 ;
vPAS.fx(sStable, sPAS)$(not sStable_PAS(sStable, sPAS)) = 0 ;

*fix animals for which we don't have economic data: no farm type, or animal type 'others'
vAnimals.fx(sFix, sOtherAnimals) = pAnimals(sFix, sOtherAnimals) ;


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

eqPAS(sStable)..
sum(sPAS, vPAS(sStable, sPAS)) =l= 1 ;


*Permit Constraint
eqAnimals(sStable, sAnimalCategory)..
vAnimals(sStable, sAnimalCategory) =l= pAnimals(sStable, sAnimalCategory)    ;

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
vAmmoniaEmissionExploitation(sExploitation) =e= sum(sStable_Exploitation(sStable, sExploitation), vAmmoniaEmissionStable(sStable)) ;
*- vAmmoniaEmissionStable_PAS(sStable, sStableType))) ;

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


Equation
eqYearlyAEAAll(sStable, sStableType)
eqYearlyAEA1(sStable, sStableType)
eqYearlyAEA2(sStable, sStableType)
eqYearlyAEA3(sStable, sStableType)
eqYearlyAEA4(sStable, sStableType)
eqYearlyAEA5(sStable, sStableType)
eqYearlyAEA6(sStable, sStableType)
eqYearlyAEA7(sStable, sStableType)
;

variable
vYearlyAEAall(sStable, sStableType)
vYearlyAEA(sStable, sStableType)
;


vYearlyAEA.fx(sStable, sStableType)$(not sPossibleStables(sStable, sStableType)) = 0 ;
*vYearlyAEAall.fx(sPossibleStables(sStable, sStableType))$(pYearlyAEAAll(sStable, sStableType) = eps) = 10 ;
*vYearlyAEAall.fx(sPossibleStables(sStable, sStableType))$(pYearlyAEAAll(sStable, sStableType) = 0) = 0 ;

eqYearlyAEAAll(sPossibleStables(sStable, sStableType))..
vYearlyAEAall(sStable, sStableType) =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pAEAextracost(sAnimalCategory, sStableType, 'JaarkostDP')))     ;

eqYearlyAEA1(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType) =l= 1E9 ;

eqYearlyAEA2(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType) =g= -1E9 ;

eqYearlyAEA3(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType) =l= vStable(sStable, sStableType) * 1E9 ;

eqYearlyAEA4(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType) =g= vStable(sStable, sStableType) * (-1E9) ;

*constraints 'loosened' with 0.001 to avoid small infeasibilities
eqYearlyAEA5(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType)  =l=   (vYearlyAEAall(sStable, sStableType)) - (1 - vStable(sStable, sStableType)) * (-1E9)  ;

*constraints 'loosened' with 1 to avoid small infeasibilities
eqYearlyAEA6(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType) =g= (vYearlyAEAall(sStable, sStableType) - 1) - (1 - vStable(sStable, sStableType)) * 1E9  ;

eqYearlyAEA7(sPossibleStables(sStable, sStableType))..
vYearlyAEA(sStable, sStableType)  =l=   (vYearlyAEAall(sStable, sStableType)+1) + (1 - vStable(sStable, sStableType)) * 1E9  ;


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

vYearlyPAS.fx(sStable, sPAS)$(not sStable_PAS(sStable, sPAS)) = 0 ;
*vYearlyPASall.fx(sStable, sPAS)$(not sStable_PAS(sStable, sPAS)) = 0 ;

eqYearlyPASAll(sStable_PAS(sStable, sPAS))..
vYearlyPASall(sStable, sPAS)   =e= sum(sAnimalCategory, (vAnimals(sStable, sAnimalCategory) * pCostPAS(sPAS, sAnimalCategory,'JaarKostDP')))     ;

eqYearlyPAS1(sStable_PAS(sStable, sPAS))..
vYearlyPAS(sStable, sPAS) =l= vPAS(sStable, sPAS) * pYearlyPASAll(sStable, sPAS) ;

eqYearlyPAS2(sStable_PAS(sStable, sPAS))..
vYearlyPAS(sStable, sPAS)=l= vYearlyPASall(sStable, sPAS) ;

eqYearlyPAS3(sStable_PAS(sStable, sPAS))..
vYearlyPAS(sStable, sPAS) =g=  vYearlyPASall(sStable, sPAS)  - (1 - vPAS(sStable, sPAS)) *  pYearlyPASAll(sStable, sPAS) ;
*$offtext

Variables
vRevenue(sExploitation) Revenue animals
vEmissionAbatementCostStable(sStable)
vEmissionAbatementCost(sExploitation) Additional cost of emission abatement (CAPEX + OPEX) compared with reference
vProfitExploitation(sExploitation) Profit animals - yearly additional cost AEA or PAS (CAPEX + OPEX)
;

Variable
vProfitTotal       ;

*vProfitTotal.lo = -1e10 ;

Equation
eqRevenue(sExploitation)
eqEmissionAbatementStable(sStable)
eqEmissionAbatement(sExploitation)
eqProfitExploitation(sExploitation)
eqOverallProfit
;

eqEmissionAbatementStable(sStable)..
vEmissionAbatementCostStable(sStable) =e= sum(sStableType, vYearlyAEA(sStable, sStableType)) + sum(sPAS, vYearlyPAS(sStable, sPAS)) ;

eqEmissionAbatement(sExploitation)..
vEmissionAbatementCost(sExploitation) =e= sum(sStable_Exploitation(sStable, sExploitation), vEmissionAbatementCostStable(sStable)) ;

eqRevenue(sExploitation)..
vRevenue(sExploitation) =e=  sum(sAnimalCategory, (vAnimalsExploitation(sExploitation, sAnimalCategory)*  pGrossMargin(sExploitation, sAnimalCategory)))      ;

eqProfitExploitation(sExploitation)..
vProfitExploitation(sExploitation) =e= vRevenue(sExploitation)
- vEmissionAbatementCost(sExploitation) ;

eqOverallProfit..
vProfitTotal =e= sum(sExploitation, vProfitExploitation(sExploitation)) ;

Equation
eqTotalADSRegion
;

Variable
vTotalADS;

eqTotalADSRegion..
vTotalADS =e= sum(sExploitation, (vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS')) ;



********************************Scenario Analysis*******************************
*===============================================================================
*-------------------------------------------------------------------------------
*FullCapacity, no change in stable type, no PAS---------------------------------
*-------------------------------------------------------------------------------
Parameter pStable(sStable, sStableType) ;

pStable(sStable, sStableType) = sStable_Stabletype(sStable, sStableType) ;

Variable vTotalAnimals ;

equation eqTotalAnimals,
         eqStableFix(sStable, sStableType)
         eqPASFix(sStable, sPAS);

eqTotalAnimals..
vTotalAnimals =e= sum((sExploitation, sAnimalCategory), vAnimalsExploitation(sExploitation, sAnimalCategory)) ;

eqStableFix(sStable, sStableType)..
vStable(sStable, sStableType) =e= pStable(sStable, sStableType) ;

eqPASFix(sStable, sPAS)..
vPAS(sStable, sPAS) =e= 0 ;

Model FullCapacity /all/          ;

Option Profile = 3 ;
Option mip = CPLEX ;
*$onecho > cplex.opt
*scaind 1
*relaxfixedinfeas 1
*$offecho

*option optcr = 0 ;
option optcr = 0.002;
*option reslim =  72000 ;
option reslim =  3600 ;


FullCapacity.OptFile = 1 ;

Solve FullCapacity maximizing vTotalAnimals using mip ;

Parameter pCurrentScenario ;

pCurrentScenario = 1 ;

$batinclude reportingEconomic.gms

dModelStat('FC') = FullCapacity.MODELSTAT  ;
dSolveSTat('FC') = FullCapacity.SOLVESTAT  ;
dObjEst('FC') = FullCapacity.ObjEst ;
dGap('FC') = abs(1 - FullCapacity.ObjVal / FullCapacity.ObjEst)

*-------------------------------------------------------------------------------
*Reference: <5% CL in  CHC, no change in stable type, no PAS--------------------
*-------------------------------------------------------------------------------
Positive Variable
vSlackSignificanceScore(sStable) overshoot emission for stables with 'fixed animals'       ;



vSlackSignificanceScore.fx(sStable)$(not sAboveThresholdS(sStable)) = 0 ;

Equations
eqSignificanceScore(sExploitation) Significance Score constraint
eqSlackSignificanceScore1(sStable)
eqSlackSignificanceScore2(sExploitation)

;

eqSignificanceScore(sExploitation)..
(vAmmoniaEmissionExploitation(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS')
- (sum(sStable_Exploitation(sAboveThresholdS, sExploitation), vSlackSignificanceScore(sAboveThresholdS))/5000)*pLocationImpact(sExploitation, 'SS')
 =l=  pSSthreshold ;

eqSlackSignificanceScore1(sAboveThresholdS)..
vSlackSignificanceScore(sAboveThresholdS) =l= sum((sOtherAnimals, sStableType), (vAnimals(sAboveThresholdS, sOtherAnimals) * pEmissionFactor(sOtherAnimals, sStableType))) ;

eqSlackSignificanceScore2(sAboveThresholdE)..
vAmmoniaEmissionExploitation(sAboveThresholdE) - ((pSSthreshold*5000)/(pLocationImpact(sAboveThresholdE, 'SS'))) - sum(sStable_Exploitation(sAboveThresholdS, sAboveThresholdE), vSlackSignificanceScore(sAboveThresholdS)) =e= 0 ;

Model Reference /all/          ;

Option Profile = 3 ;
Option mip = CPLEX ;
*$onecho > cplex.opt
*scaind 1
*relaxfixedinfeas 1
*$offecho

*option optcr = 0 ;
option optcr = 0.002;
*option reslim =  72000 ;
option reslim =  3600 ;


Reference.OptFile = 1 ;

Solve Reference maximizing vProfitTotal using mip ;

pCurrentScenario = 2 ;

$batinclude reportingEconomic.gms

dModelStat('Ref') = Reference.MODELSTAT  ;
dSolveSTat('Ref') = Reference.SOLVESTAT  ;
dObjEst('Ref') = Reference.ObjEst ;
dGap('Ref') = abs(1 - Reference.ObjVal / Reference.ObjEst)

*-------------------------------------------------------------------------------
*Scenario 1: <5% CL in  CHC (Reference)-----------------------------------------
*-------------------------------------------------------------------------------


*Add very low fake gross margin to animals without GM, to make sure that the model results has the maximum number of animals allowed
*pGrossMargin(sExploitation, sAnimalCategory)$((pAnimalExploitation(sExploitation, sAnimalCategory) > 0) and (pGrossMargin(sExploitation, sAnimalCategory)= 0))
*= 10E-4 ;

Model Scenario1 /all - eqStableFix - eqPASfix/          ;

Option Profile = 3 ;
Option mip = CPLEX ;
*$onecho > cplex.opt
*scaind 1
*relaxfixedinfeas 1
*$offecho

*option optcr = 0 ;
option optcr = 0.002;
*option reslim =  72000 ;
option reslim =  3600 ;


Scenario1.OptFile = 1 ;

Solve Scenario1 maximizing vProfitTotal using mip ;


pCurrentScenario = 3 ;

$batinclude reportingEconomic.gms

dModelStat('sc1') = Scenario1.MODELSTAT  ;
dSolveSTat('sc1') = Scenario1.SOLVESTAT  ;
dObjEst('sc1') = Scenario1.ObjEst ;
dGap('sc1') = abs(1 - Scenario1.ObjVal / Scenario1.ObjEst)


*execute_unload 'sc1.gdx'


*$exit
*$offtext


*$ontext

*-------------------------------------------------------------------------------
**Scenario 2: Effectivity check, minimize ADS, profit   bigger than sc1---------
*-------------------------------------------------------------------------------

Equation
eqProfitFloor ;



eqProfitFloor..
vProfitTotal =g=  dProfitRegion('Sc1');

pCurrentScenario = 4      ;

Model Scenario2 /Scenario1 - eqSignificanceScore - eqSlackSignificanceScore1 - eqSlackSignificanceScore2 +  eqProfitFloor/ ;

Option Profile = 3 ;
Option mip = CPLEX ;
*$onecho > cplex.opt
*scaind 1
*relaxfixedinfeas 1
*$offecho

*option optcr = 0 ;
option optcr = 0.002;
*option reslim =  72000 ;
option reslim =  600 ;


Scenario2.OptFile = 1 ;

Solve Scenario2 using mip minimizing vTotalADS ;

dModelStat('sc2') = Scenario2.MODELSTAT  ;
dSolveSTat('sc2') = Scenario2.SOLVESTAT  ;
dObjEst('sc2') = Scenario2.ObjEst ;
dGap('sc2') = abs(1 - Scenario2.ObjVal / Scenario2.ObjEst)   ;

$batinclude reportingEconomic.gms

*execute_unload 'sc2_reslim600s.gdx'

*$exit
*$offtext

*-------------------------------------------------------------------------------
*Scenario 3: Efficiency check: Total ADS max.(sc1), max. profit, no individual farm constraints
*-------------------------------------------------------------------------------
Equations
eqTotalADSRegionSc3
;

pCurrentScenario = 5       ;

*lower than total impact from previous model
eqTotalADSRegionSc3..
vTotalADS =l= dADSregion('sc1') ;

Model Scenario3 /Scenario2 -   eqProfitFloor + eqTotalADSRegionSc3   /          ;

Option Profile = 3 ;
Option mip = CPLEX ;
*$onecho > cplex.opt
*scaind 1
*relaxfixedinfeas 1
*$offecho

*option optcr = 0 ;
option optcr = 0.002;
*option reslim =  72000 ;
option reslim =  600 ;


Scenario3.OptFile = 1 ;


Parameter pModelStat, pSolveStat ;

pModelStat = Scenario3.MODELSTAT         ;
pSolveSTat = Scenario3.SOLVESTAT         ;

Solve Scenario3 maxmizing vProfitTotal using mip ;

$batinclude reportingEconomic.gms


dModelStat('sc3') = Scenario3.MODELSTAT  ;
dSolveSTat('sc3') = Scenario3.SOLVESTAT  ;
dObjEst('sc3') = Scenario3.ObjEst ;
dGap('sc3') = abs(1 - Scenario3.ObjVal / Scenario3.ObjEst)

*execute_unload 'sc3_reslim600s.gdx'

*$offtext

$exit
$ontext
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

*$batinclude reporting.gms

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

*$batinclude reporting.gms

Parameter pModelStat, pSolveStat ;

pModelStat = Scenario4.MODELSTAT         ;
pSolveSTat = Scenario4.SOLVESTAT         ;

execute_unload 'sc4.gdx'

********************************************************************************
**********Writing GDX with all relevant data************************************
********************************************************************************
execute_unload 'results.gdx' dSignificanceScore, dADS, dTotalADS, dPercentageOccupied, dPercentageOccupiedRegion, dClosedStables,dClosedExploitations,dEmissionStable, dEmissionExploitation, dEmissionRegion, dAnimals, dStableTypesNIS, dAnimalsNIS, dAnimalGroupNIS, dEmissionNIS, dADSNIS, dEmissionAnimal, dEmissionStableType, dEmissionAnimalGroup, dMarginalSignificanceScore, dMaxEmissionStable, dMaxEmissionExploitation, dMaxEmissionNIS, dMaxEmissionAnimalCategory, dMaxEmissionSector, dMaxEmissionStableType, dAnimalsGroup, dMaxAnimalsGroup

$offtext
