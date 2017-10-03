*All parameters/results that are of interest
*Zeros replaced by EPS
*All parameters are defined over set 'scenario' (all results can than be bundled in 1 big GDX)

*define current scenario
sScenario(sScen) = no ;
sScenario('sc1')$(pCurrentScenario = 1) = yes ;
sScenario('sc2')$(pCurrentScenario = 2) = yes ;
sScenario('sc3')$(pCurrentScenario = 3) = yes ;
sScenario('sc4')$(pCurrentScenario = 4) = yes ;

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
