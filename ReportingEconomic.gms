*All parameters/results that are of interest
*Zeros replaced by EPS
*All parameters are defined over set 'scenario' (all results can then be bundled in 1 big GDX)
*Set sScen /FC, ref, sc1, sc2, sc3/
*    sAbatement /animalreduction, AEA/ ;

*define current scenario
sScenario(sScen) = no ;
sScenario('FC')$(pCurrentScenario = 1) = yes ;
sScenario('Ref')$(pCurrentScenario = 2) = yes ;
sScenario('sc1')$(pCurrentScenario = 3) = yes ;
sScenario('sc2')$(pCurrentScenario = 4) = yes ;
sScenario('sc3')$(pCurrentScenario = 5) = yes ;

*-------------------------------------------------------------------------------
*StableStats--------------------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dMaxAmmoniaStable(sScen, sExploitation, sStable)
dAmmoniaEmissionStable(sScen, sExploitation, sStable)
dAmmoniaOldStable(sExploitation, sStable)
dEmissionReduction(sScen, sExploitation, sStable)
dAbatementCostAbsolute(sScen, sExploitation, sStable, sAbatement)
dAbatementCostTotal(sScen, sExploitation, sStable)
dAbatementCostPerKgNH3(sScen, sExploitation, sStable, sAbatement)
dAbatementCostPerKgNH3Total(sScen, sExploitation, sStable)
dPercentageOccupied(sScen, sExploitation, sStable, sAnimalCategory) percentage of permitted capacity occupied by animals
dEmissionStable(sScen, sExploitation, sStable)
dAnimals(sScen, sExploitation, sStable, sAnimalCategory)
;

dAmmoniaEmissionStable(sScenario, sExploitation, sStable) = vAmmoniaEmissionStable.l(sStable) * sStable_exploitation(sStable, sExploitation);

dMaxAmmoniaStable(sScenario, sExploitation, sStable) = sStable_Exploitation(sStable, sExploitation) * sum(sStableType, (vStable.l(sStable, sStableType) *
sum(sAnimalCategory, (pAnimals(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType))))) ;

dAmmoniaOldStable(sExploitation, sStable) = sStable_Exploitation(sStable, sExploitation) * sum(sStableType, (sStable_StableType(sStable, sStableType) *
sum(sAnimalCategory, (vAnimals.l(sStable, sAnimalCategory) * pEmissionFactor(sAnimalCategory, sStableType)))))   ;

dEmissionReduction(sScenario, sExploitation, sStable) = dMaxAmmoniaStable('FC', sExploitation, sStable) - sStable_Exploitation(sStable, sExploitation) *  vAmmoniaEmissionStable.l(sStable) ;

dEmissionReduction(sScenario, sExploitation, sStable)$(dEmissionReduction(sScenario, sExploitation, sStable) < 0.001) =  0 ;

dAbatementCostAbsolute(sScenario, sExploitation, sStable, 'animalreduction')$(dMaxAmmoniaStable(sScenario, sExploitation, sStable) > vAmmoniaEmissionStable.l(sStable)) = sum((sAnimalCategory, sStable_Exploitation(sStable, sExploitation)),
        ((pAnimals(sStable, sAnimalCategory) - vAnimals.l(sStable, sAnimalCategory))*pGrossMargin(sExploitation, sAnimalCategory)))      ;
*/(dMaxAmmoniaStable(sScenario, sExploitation, sStable) - vAmmoniaEmissionStable.l(sStable))*sStable_exploitation(sStable, sExploitation) ;
;

dAbatementCostAbsolute(sScenario, sExploitation, sStable, 'AEA')$(vAmmoniaEmissionStable.l(sStable) < dAmmoniaOldStable(sExploitation, sStable)) =
sum(sStableType, vYearlyAEA.l(sStable, sStableType)) ;

dAbatementCostAbsolute(sScenario, sExploitation, sStable, sAbatement)$(abs(dAbatementCostAbsolute(sScenario, sExploitation, sStable, sAbatement)) < 1.1) = 0 ;

dAbatementCostTotal(sScenario, sExploitation, sStable) = sum(sAbatement, dAbatementCostAbsolute(sScenario, sExploitation, sStable, sAbatement)) ;

dAbatementCostPerKgNH3(sScenario, sExploitation, sStable, 'animalreduction')$(dMaxAmmoniaStable(sScenario, sExploitation, sStable) > vAmmoniaEmissionStable.l(sStable) and
dEmissionReduction(sScenario, sExploitation, sStable) > 0)
 = dAbatementCostAbsolute(sScenario, sExploitation, sStable, 'animalreduction')
/ (dMaxAmmoniaStable(sScenario, sExploitation, sStable) - vAmmoniaEmissionStable.l(sStable))     ;

dAbatementCostPerKgNH3(sScenario, sExploitation, sStable, 'AEA')$(vAmmoniaEmissionStable.l(sStable) < dAmmoniaOldStable(sExploitation, sStable) and
dEmissionReduction(sScenario, sExploitation, sStable) > 0)
= dAbatementCostAbsolute(sScenario, sExploitation, sStable, 'AEA') / (dAmmoniaOldStable(sExploitation, sStable) - vAmmoniaEmissionStable.l(sStable)) ;

dAbatementCostPerKgNH3(sScenario, sExploitation, sStable, sAbatement)$(abs(dAbatementCostPerKgNH3(sScenario, sExploitation, sStable, sAbatement) < 1.1)) = 0   ;

dAbatementCostPerKgNH3Total(sScenario, sExploitation, sStable)$(dEmissionReduction(sScenario, sExploitation, sStable) > 0) = dAbatementCostTotal(sScenario, sExploitation, sStable) / dEmissionReduction(sScenario, sExploitation, sStable) ;


dPercentageOccupied(sScenario, sExploitation, sStable, sAnimalCategory)$(pAnimals(sStable, sAnimalCategory) ne 0) = (vAnimals.l(sStable, sAnimalCategory)/pAnimals(sStable, sAnimalCategory)) * 100 * sStable_Exploitation(sStable, sExploitation) ;

dEmissionStable(sScenario, sExploitation, sStable) = vAmmoniaEmissionStable.l(sStable) * sStable_Exploitation(sStable, sExploitation) ;

dAnimals(sScenario, sExploitation, sStable, sAnimalCategory) = vAnimals.l(sStable, sAnimalCategory)  * sStable_Exploitation(sStable, sExploitation) ;
*-------------------------------------------------------------------------------
*ExploitationStats--------------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dProfitExploitation(sScen, sExploitation)
dImpactScoreExploitation(sScen, sExploitation, sImpactScores)
dPercentageOccupiedExploitation(sScen, sExploitation)
dEmissionExploitation(sScen, sExploitation)
;

dProfitExploitation(sScenario, sExploitation) = vProfitExploitation.l(sExploitation) ;

dImpactScoreExploitation(sScenario, sExploitation, sImpactScores) = (vAmmoniaEmissionExploitation.l(sExploitation)/5000) * pLocationImpact(sExploitation, sImpactScores) ;

dPercentageOccupiedExploitation(sScenario, sExploitation)$(iPermittedAnimals(sExploitation) ne 0) = (sum((sStable, sAnimalCategory),
         dPercentageOccupied(sScenario, sExploitation, sStable, sAnimalCategory)) / iPermittedAnimals(sExploitation));
dPercentageOccupiedExploitation(sScenario, sExploitation)$(dPercentageOccupiedExploitation(sScenario, sExploitation) = 0) = Eps ;

dEmissionExploitation(sScenario, sExploitation) = vAmmoniaEmissionExploitation.l(sExploitation) ;

*-------------------------------------------------------------------------------
*AnimalsStats------------------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dEmissionAnimals(sScen, sAnimalCategory)
;

dEmissionAnimals(sScenario, sAnimalCategory) = sum((sStable, sStableType),
(vAnimals.l(sStable, sAnimalCategory) * vStable.l(sStable, sStableType) * pEmissionFactor(sAnimalCategory, sStableType))) ;

*-------------------------------------------------------------------------------
*FarmTypeStats------------------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dEmissionFarmType(sScen, sFarmType)
dProfitFarmType(sScen, sFarmType)
dImpactFarmType(sScen, sFarmType, sImpactScores)
;

dEmissionFarmType(sScenario, sFarmType) = sum(sStable_FarmType(sStable, sFarmType), vAmmoniaEmissionStable.l(sStable))    ;
dProfitFarmType(sScenario, sFarmType) = sum((sStable_FarmType(sStable, sFarmType), sExploitation, sAnimalCategory), (vAnimals.l(sStable, sAnimalCategory)
 * pGrossMargin(sExploitation, sAnimalCategory) * sStable_Exploitation(sStable, sExploitation))) ;

dImpactFarmType(sScen, sFarmType, sImpactScores) = sum(sExploitation_FarmType(sExploitation, sFarmType), dImpactScoreExploitation(sScen, sExploitation, sImpactScores)) ;
*-------------------------------------------------------------------------------
*SectorStats--------------------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dEmissionSector(sScen, sSectors)
dProfitSector(sScen, sSectors)
dImpactSector(sScen, sSectors, sImpactScores)
;

dEmissionSector(sScenario, sSectors) = sum(sSectors_AnimalCategory(sSectors, sAnimalCategory), dEmissionAnimals(sScenario, sAnimalCategory))    ;

dProfitSector(sScenario, sSectors) = sum((sSectors_AnimalCategory(sSectors, sAnimalCategory), sStable, sExploitation),
(vAnimals.l(sStable, sAnimalCategory) * pGrossMargin(sExploitation, sAnimalCategory) * sStable_Exploitation(sStable, sExploitation))) ;

dImpactSector(sScenario, sSectors, sImpactScores) = sum((sSectors_AnimalCategory(sSectors, sAnimalCategory), sStable, sExploitation, sStableType),
(vAnimals.l(sStable, sAnimalCategory) * vStable.l(sStable, sStableType) * (pEmissionFactor(sAnimalCategory, sStableType)/5000)
 * sStable_Exploitation(sStable, sExploitation) * pLocationImpact(sExploitation, sImpactScores)) )       ;

*-------------------------------------------------------------------------------
*Municipality (NIS) stats-------------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
dStableTypesNIS(sScen, sNIS, sStableType)
dAnimalsNIS(sScen, sNIS, sAnimalCategory)
dAnimalsGroupNIS (sScen, sNIS, sSectors)
dEmissionNIS(sScen, sNIS)
dImpactNIS(sScen, sNIS, sImpactScores)
dProfitNIS(sScen, sNIS)
;

dStableTypesNIS(sScenario, sNIS, sStableType) = sum(sStable,
(vStable.l(sStable, sStableType) * sStable_NIS(sStable, sNIS))) ;

dAnimalsNIS(sScenario, sNIS, sAnimalCategory) = sum(sStable, (vAnimals.l(sStable, sAnimalCategory) *  sStable_NIS(sStable, sNIS))) ;

dAnimalsGroupNIS(sScenario, sNIS, sSectors) = sum(sSectors_AnimalCategory(sSectors, sAnimalCategory), dAnimalsNIS(sScenario, sNIS, sAnimalCategory)) ;

dEmissionNIS(sScenario, sNIS) = sum(sStable, (vAmmoniaEmissionStable.l(sStable)*  sStable_NIS(sStable, sNIS))) ;

dImpactNIS(sScenario, sNIS, sImpactScores) = sum(sExploitation,
(dImpactScoreExploitation(sScenario, sExploitation, sImpactScores) * sExploitation_NIS(sExploitation, sNIS))) ;

dProfitNIS(sScenario, sNIS) = sum(sExploitation,
(dProfitExploitation(sScenario, sExploitation) * sExploitation_NIS(sExploitation, sNIS))) ;

*-------------------------------------------------------------------------------
*Model Stats & Global Stats-----------------------------------------------------
*-------------------------------------------------------------------------------
Parameter
*modelstats
dModelStat(sScen)
dSolveStat(sScen)
dGap(sScen)
dObjEst(sScen)
dGap(sScen)
*global
dSumStableTypes(sScen, sStabletype)
dPASmeasures(sScen, sPAS)
dADSregion(sScen)
dAmmoniaRegion(sScen)
dProfitRegion(sScen)
dClosedStables(sScen)
dClosedExploitations(sScen)
;

dSumStableTypes(sScenario, sStableType) = sum(sStable, vStable.l(sStable, sStableType)) ;
dPASmeasures(sScenario, sPAS) = sum(sStable, vPAS.l(sStable, sPAS));
dADSregion(sScenario) = vTotalADS.l ;
dAmmoniaRegion(sScenario) = vAmmoniaEmissionRegion.l       ;
dProfitRegion(sScenario) = vProfitTotal.l                   ;
dClosedStables(sScenario)  = card(sStable) - sum(sStable, rel_ne(sum(sAnimalCategory, (vAnimals.l(sStable, sAnimalCategory))), 0))   ;
dClosedExploitations(sScenario) = card(sExploitation) - sum(sExploitation, rel_ne(vAmmoniaEmissionExploitation.l(sExploitation),0)) ;



*-------------------------------------------------------------------------------
*Reporting parameters-----------------------------------------------------------
*-------------------------------------------------------------------------------



$ontext
parameter
dSignificanceScore(sExploitation, sScen)
dADS(sExploitation, sScen)
dTotalADS(sScen)
*dTotalProfit
dPercentageOccupied(sStable, sAnimalCategory, sScen) percentage of permitted capacity occupied by animals
dPercentageOccupiedExploitation(sExploitation, sScen)
dPercentageOccupiedRegion(sScen)
dClosedStables(sScen)
dClosedExploitations(sScen)
dEmissionStable(sStable, sScen)
dEmissionExploitation(sExploitation, sScen)
dEmissionRegion(sScen)
dAnimals(sStable, sAnimalCategory, sScen)
dStableTypesNIS(sNIS, sStableType)
dAnimalsNIS(sNIS, sAnimalCategory, sScen)
dAnimalGroupNIS(sNIS, sSectors, sScen)
dAnimalsGroup(sSectors, sScen)
dMaxAnimalsGroup(sSectors)
dEmissionNIS(sNIS, sScen)
dADSNIS(sNIS, sScen)
dEmissionAnimal(sAnimalCategory, sScen)
dEmissionStableType(sStableType, sScen)
dEmissionAnimalGroup(sSectors, sScen)
dMarginalSignificanceScore(sExploitation)
dMaxEmissionStable(sStable)
dMaxEmissionExploitation(sExploitation)
dMaxEmissionNIS(sNIS)
dMaxEmissionAnimalCategory(sAnimalCategory)
dMaxEmissionSector(sSectors)
dMaxEmissionStabletype(sStableType)
*dProfitSource(sSource)
iStableColour(sStable)
iEmissionAnimalCategory(sStable, sAnimalCategory, sScen)
;

iStableColour(sStable) = sum(sStable_Exploitation(sStable, sExploitation), iFarmColour(sExploitation)) ;

dMaxEmissionStable(sStable) = sum((sStable_StableType(sStable, sStableType), sAnimalCategory), (pEmissionFactor(sAnimalCategory, sStableType) *
                                 pAnimals(sStable, sAnimalCategory))) ;
dMaxEmissionExploitation(sExploitation) =  sum((sStable_Exploitation(sStable, sExploitation)), dMaxEmissionStable(sStable))   ;
dMaxEmissionNIS(sNIS) = sum((sExploitation_NIS(sExploitation, sNIS)), dMaxEmissionExploitation(sExploitation)) ;
dMaxEmissionAnimalCategory(sAnimalCategory) = sum((sStable_StableType(sStable, sStableType)), (pEmissionFactor(sAnimalCategory, sStableType) *
                                 pAnimals(sStable, sAnimalCategory))) ;
dMaxEmissionSector(sSectors) = sum(sSectors_AnimalCategory(sSectors, sAnimalCategory), dMaxEmissionAnimalCategory(sAnimalCategory)) ;
dMaxEmissionStabletype(sStableType)   = sum(sStable_StableType(sStable, sStableType), dMaxEmissionStable(sStable)) ;

dSignificanceScore(sExploitation, sScenario) = (vAmmoniaEmissionExploitation.l(sExploitation)/5000)* pLocationImpact(sExploitation, 'SS')   ;
dSignificanceScore(sExploitation, sScenario)$(dSignificanceScore(sExploitation, sScenario) = 0) = Eps ;
dADS(sExploitation, sScenario) = (vAmmoniaEmissionExploitation.l(sExploitation)/5000)* pLocationImpact(sExploitation, 'ADS')     ;
dADS(sExploitation, sScenario)$(dADS(sExploitation, sScenario) = 0) = Eps ;
dTotalADS(sScenario) = sum(sExploitation, dADS(sExploitation, sScenario)) ;
*dTotalProfit = vProfitSociety.l ;
dPercentageOccupied(sStable, sAnimalCategory, sScenario)$(pAnimals(sStable, sAnimalCategory) ne 0) = (vAnimals.l(sStable, sAnimalCategory)/pAnimals(sStable, sAnimalCategory)) * 100  ;
dPercentageOccupiedExploitation(sExploitation, sScenario)$(iPermittedAnimals(sExploitation) ne 0) = (sum((sStable_Exploitation(sStable, sExploitation), sAnimalCategory),
         dPercentageOccupied(sStable, sAnimalCategory, sScenario)) / iPermittedAnimals(sExploitation));
dPercentageOccupiedExploitation(sExploitation, sScenario)$(dPercentageOccupiedExploitation(sExploitation, sScenario) = 0) = Eps ;
dPercentageOccupiedRegion(sScenario) = (sum(sExploitation,(dPercentageOccupiedExploitation(sExploitation, sScenario)))/card(sExploitation))             ;
dClosedStables(sScenario)  = card(sStable) - sum(sStable, rel_ne(sum(sAnimalCategory, (vAnimals.l(sStable, sAnimalCategory))), 0))                ;

iEmissionAnimalCategory(sStable, sAnimalCategory, sScenario)  = sum(sStable_StableType(sStable, sStableType), (vAnimals.l(sStable, sAnimalCategory) * pEmissionfactor(sAnimalCategory, sStableType)))  ;
iEmissionAnimalCategory(sStable, sAnimalCategory, 'sc4') = vAnimals.l(sStable, sAnimalCategory) * pBestEF(sAnimalCategory) ;
iEmissionAnimalCategory(sStable, sAnimalCategory, 'sc3')$(iStableColour(sStable) = 2) = vAnimals.l(sStable, sAnimalCategory) * pBestEF(sAnimalCategory) ;

dEmissionStable(sStable, sScenario) = sum(sAnimalCategory, iEmissionAnimalCategory(sStable, sAnimalCategory, sScenario)) ;


dEmissionExploitation(sExploitation,  sScenario) = vAmmoniaEmissionExploitation.l(sExploitation) ;
dClosedExploitations(sScenario) = card(sExploitation) - sum(sExploitation, rel_ne(dEmissionExploitation(sExploitation, sScenario),0)) ;
dEmissionRegion(sScenario) = vAmmoniaEmissionRegion.l ;
*dProfitSource(sSource) = vProfitSource.l(sSource) ;
*dProfitSource(sSource)$(dProfitSource(sSource) = 0) = Eps ;
dAnimals(sStable, sAnimalCategory, sScenario) = vAnimals.l(sStable,  sAnimalCategory) ;
dAnimalsNIS(sNIS,  sAnimalCategory,  sScenario) = sum(sStable_NIS(sStable, sNIS), dAnimals(sStable, sAnimalCategory, sScenario)) ;
dAnimalGroupNIS(sNIS, sSectors, sScenario)   = sum(sSectors_AnimalCategory(sSectors, sAnimalCategory), dAnimalsNIS(sNIS, sAnimalCategory, sScenario)) ;
dAnimalsGroup(sSectors, sScenario) = sum(sNIS, dAnimalGroupNIS(sNIS, sSectors, sScenario)) ;
dMaxAnimalsGroup(sSectors) = sum((sSectors_AnimalCategory(sSectors, sAnimalCategory), sStable), pAnimals(sStable, sAnimalCategory)) ;
dEmissionNIS(sNIS, sScenario) = sum(sExploitation_NIS(sExploitation, sNIS), dEmissionExploitation(sExploitation, sScenario)) ;
dADSNIS(sNIS, sScenario) = sum(sExploitation_NIS(sExploitation, sNIS), dADS(sExploitation, sScenario)) ;

dEmissionAnimal(sAnimalCategory, sScenario) = sum(sStable, iEmissionAnimalCategory(sStable, sAnimalCategory, sScenario)) ;
dEmissionAnimalGroup(sSectors, sScenario) = sum(sSectors_AnimalCategory(sSectors, sAnimalCategory), dEmissionAnimal(sAnimalCategory, sScenario)) ;
dEmissionStableType(sStableType, sScenario) = sum(sStable_StableType(sStable, sStableType), dEmissionStable(sStable, sScenario)) ;
dMarginalSignificanceScore(sExploitation)$(pCurrentScenario = 1) = eqSignificanceScore.m(sExploitation)    ;




$ontext
sum(sStable_StableType(sStable, sStableType), sAnimalCategory),
(pEmissionFactor(sAnimalCategory, sStableType) * vAnimals(sStable, sAnimalCategory))) ;
$offtext
