*===============================================================================
*=======================Data preprocessing and data input=======================
*===============================================================================
Set
*sStable /s1/
*sExploitation /e1/
*sStable /s117*s173/
*sExploitation /e74*e100/
sStable /s1*s173/
sExploitation /e1*e100/
*sStable/s1*s3655/
*sExploitation/e1*e2000/
*sStable/s1*s7181/
*sExploitation/e1*e4000/
*sStable/s7182*s14428/
*sExploitation/e4001*e8000/
*sStable/s14429*s22840/
*sExploitation/e8001*e12000/
*sExploitation /e1*e23408/
*sStable /s1*s44540/
sNIS
sStableType
sFarmer /f1*f20709/
sID(sStable, sExploitation, sNIS, sFarmer, sStableType)
sAnimalCategory
sCoordinates /X, Y/
sImpactScores /ADS, SS/
sSectors /Runderen, Varkens,  Pluimvee,  Andere, Paarden/
sSectors_AnimalCategory(sSectors, sAnimalCategory)
*sScen /sc1*sc4/
*sScenario(sScen)
sCost /InvPerDP, JaarKostDP/
sPAS
*sPAS /PAS_R_1_13, PAS_R_1_6, PAS_V_1_1, PAS_V_2_1, PAS_V_3_1, PAS_V_4_1/
sFarmType /Dairy, ClosedBeef, BeefBulls, Sheep, PigRearing, LayingHens, PigFattening, ClosedPigFattening,
Broilers, ParentsBroilers, RearingLayingHens, RearingParentsBroilers, Goats, FatteningCalves, Turkeys/
sStatistic
sScen /FC, ref, sc1, sc2, sc3/
sScenario(sScen)
sAbatement /animalreduction, AEA/ ;
;

$gdxin FarmsFlanders1.gdx
$load sNIS, sStableType, sID, sAnimalCategory, sSectors_AnimalCategory, sPAS, sStatistic
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
pGM(sExploitation, sFarmType, sAnimalCategory, sStatistic)
;

*FarmFlanders.gdx: seed 1 R (allocation sources)
*FarmFlanders2.gdx: seed2 R
*FarmFlanders3.gdx: seed3 R
$gdxin FarmsFlanders1.gdx
$load pAnimals, pLocationImpact, pEmissionFactor, pSourceLocation, pAEAcost, pGM, pReductionEFPAS, pCostPAS
$OnEPS
$load pAEAextracost
$gdxin

*pAEAextracost(sAnimalCategory, sStableType, sCost)$(pAEAextracost(sAnimalCategory, sStableType, sCost) = 1E-14) = 10 ;

*pAEAextracost: zero elements are infinitely small
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
*******************************Defining Farmtype per exploitation***************
********************************************************************************
set sExploitation_FarmType(sExploitation, sFarmType) ;

parameter pNumberofFarmsperFarmType(sFarmType) ;

*Allocation should only be done once
*option sExploitation_FarmType <= pGM ;
*$ontext
*Closed PigFattening: first select all farms with both fattening pigs and sows
*Then keep farms with ratio a124/a123 between 22.5 and 37.5
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123'))>0)  = yes ;
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a125'))=0)  = no ;
sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124'))=0)  = no ;

*sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124'))/
*sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123')) > 37.5) = no ;

*sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124'))/
*sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a123')) < 22.5) = no ;

sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$((sum(sStable_Exploitation(sStable, sExploitation), (pAnimals(sStable, 'a123') + pAnimals(sStable, 'a125')))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124')))<0.2 ) = no ;

sExploitation_FarmType(sExploitation, 'ClosedPigFattening')$((sum(sStable_Exploitation(sStable, sExploitation), (pAnimals(sStable, 'a123') + pAnimals(sStable, 'a125')))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124')))>(1/3)) = no ;

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
sExploitation_FarmType(sExploitation, 'LayingHens')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a134'))>0) = yes ;

*Broilers: broiler present
sExploitation_FarmType(sExploitation, 'Broilers')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a32'))>0) = yes ;

*RearingParentsBroilers
sExploitation_FarmType(sExploitation, 'RearingParentsBroilers')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a99'))>0) = yes ;

*ParentsBroilers
sExploitation_FarmType(sExploitation, 'ParentsBroilers')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a98'))>0) = yes ;

*RearingLayingHens
sExploitation_FarmType(sExploitation, 'RearingLayingHens')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a132'))>0) = yes ;

*Sheep: adult sheep present
sExploitation_FarmType(sExploitation, 'Sheep')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a43'))>0) = yes ;

*Goats: adults Goats present
sExploitation_FarmType(sExploitation, 'Goats')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a165'))>0) = yes ;

*FatteningCalves: FatteningCalves present
sExploitation_FarmType(sExploitation, 'FatteningCalves')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a14'))>0) = yes ;

*Turkeys: turkeys present
sExploitation_FarmType(sExploitation, 'Turkeys')$(sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a92'))>0) = yes ;

*$offtext

pNumberofFarmsperFarmType(sFarmType) = sum(sExploitation_FarmType(sExploitation, sFarmType), 1) ;



********************************************************************************
*****************************Defining animal groups per farm type***************
********************************************************************************
set sFarmType_AnimalCategory(sFarmType, sAnimalCategory)
    /Dairy.a1131
     ClosedBeef. a1132
     BeefBulls.a117
     ClosedPigFattening.(a123,a125)
     PigFattening.a124
     PigRearing.(a123,a125)
     Sheep.a43
     LayingHens.(a133, a134)
     RearingLayingHens.a132
     Broilers.a32
     RearingParentsBroilers.a99
     ParentsBroilers.a98
     Turkeys.a92
     Goats.a165
     FatteningCalves.a14
     /

Parameter
pAnimalExploitation(sExploitation, sAnimalCategory) ;

pAnimalExploitation(sExploitation, sAnimalCategory) = sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sAnimalCategory)) ;

*Exploitations with overlapping activities
Sets
sExploitationPigFattening_PigRearing(sExploitation)
sExploitationDairy_ClosedBeef(sExploitation)
sExploitationsNoOverlap(sExploitation)
;

sExploitationPigFattening_PigRearing(sExploitation)$(sExploitation_FarmType(sExploitation, 'PigFattening')
and sExploitation_FarmType(sExploitation, 'PigRearing')) = yes ;

sExploitationDairy_ClosedBeef(sExploitation)$(sExploitation_FarmType(sExploitation, 'Dairy')
and sExploitation_FarmType(sExploitation, 'ClosedBeef')) = yes ;

sExploitationsNoOverlap(sExploitation) = yes ;
sExploitationsNoOverlap(sExploitation)$(sExploitationDairy_ClosedBeef(sExploitation)) = no ;
sExploitationsNoOverlap(sExploitation)$(sExploitationPigFattening_PigRearing(sExploitation)) = no ;



*$ontext
********************************************************************************
*****************************Fix animal ratios depending on sector**************
********************************************************************************
*lock animal ratios per exploitation
alias(sAnimalCategory, sAnimalCategory2) ;

Parameter pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)
;

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

Set sOtherPigs(sAnimalCategory) /a121, a122/
    sAdultPigs(sAnimalCategory) /a123*a125/
;

*ClosedPigFattening
pAnimalRatio(sExploitation, sPigs, 'a124')$(sExploitation_FarmType(sExploitation, 'ClosedPigFattening'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124') ) ;

*PigRearing or combination Pig Rearing/Pig Fattening
pAnimalRatio(sExploitation, sOtherPigs, sAdultPigs)$(sExploitation_FarmType(sExploitation, 'PigRearing'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sOtherPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), (pAnimals(sStable, 'a123') + pAnimals(sStable, 'a124') + pAnimals(sStable, 'a125'))) ;

*Exclusive PigFattening
pAnimalRatio(sExploitation, sPigs, 'a124')$(not sExploitation_FarmType(sExploitation, 'PigRearing') and sExploitation_FarmType(sExploitation, 'PigFattening'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sPigs))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a124') ) ;

*Sheep
pAnimalRatio(sExploitation, 'a42', 'a43')$(sExploitation_FarmType(sExploitation, 'Sheep'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a42'))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a43') ) ;

*Goats
pAnimalRatio(sExploitation, 'a166', 'a165')$(sExploitation_FarmType(sExploitation, 'Goats'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a166'))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a165') ) ;

*Turkeys
pAnimalRatio(sExploitation, 'a91', 'a92')$(sExploitation_FarmType(sExploitation, 'Turkeys'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a91'))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a92') ) ;

$ontext
*LayingHens
Set sLayingHens(sAnimalCategory) /a133, a134/ ;

pAnimalRatio(sExploitation, 'a134', 'a133')$(sExploitation_FarmType(sExploitation, 'LayingHens'))
= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a134'))
/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a133') ) ;

*Broilers
*Set sBroilers(sAnimalCategory) /a98, a99/ ;

*pAnimalRatio(sExploitation, sBroilers, 'a32')$(sExploitation_FarmType(sExploitation, 'Broilers'))
*= sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, sBroilers))
*/ sum(sStable_Exploitation(sStable, sExploitation), pAnimals(sStable, 'a32') ) ;
$offtext

pAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)$(ord(sAnimalCategory) eq ord (sAnimalCategory2)) =   0 ;
*$offtext

*
Set sAnimalRatio(sExploitation, sAnimalCategory, sAnimalCategory2)
    sFarmType_animalsExtended(sFarmType, sAnimalCategory)
    /Dairy.(a111, a112, a115, a116, a117, a1131)
     ClosedBeef.(a111, a112, a115, a116, a117, a1132)
     BeefBulls.(a111, a112, a115, a116, a117)
     ClosedPigFattening.(a121*a125)
     PigRearing.(a121, a122, a123, a125)
     PigFattening.(a121, a122, a124)
     Sheep.(a42*a43)
     LayingHens(
/

sSector_FarmType(sSectors, sFarmType)
    /Runderen.(Dairy, ClosedBeef, BeefBulls, FatteningCalves)
     Varkens.(PigFattening, PigRearing, ClosedPigFattening)
     Pluimvee.(LayingHens, Broilers, ParentsBroilers, RearingLayingHens, RearingParentsBroilers, Turkeys)
     Andere.(Sheep, Goats)/ ;



option sAnimalRatio <= pAnimalRatio ;




********************************************************************************
*****************************Gross Margin from normal distribution**************
********************************************************************************
Parameter pGrossMargin(sExploitation, sAnimalCategory) ;

pGrossMargin(sExploitation, sAnimalCategory)= sum(sFarmType, pGM(sExploitation, sFarmType, sAnimalCategory, 'GM'))
;

pGrossMargin(sExploitation, 'a123')$((pGrossMargin(sExploitation, 'a125') > 0)) = pGrossMargin(sExploitation, 'a125')         ;
pGrossMargin(sExploitation, 'a134')$((pGrossMargin(sExploitation, 'a133') > 0)) = pGrossMargin(sExploitation, 'a133')         ;

********************************************************************************
***********Stables that model can choose: initial stable + stable with cost info
********************************************************************************
Set
sAEA(sAnimalCategory, sStabletype)
*sInitialStab(sStable, sStableType)
sPosStab(sStable, sStableType, sAnimalCategory)
sPossibleStables(sStable, sStableType)
sOtherAnimals(sAnimalCategory) /a141, a194, a195, a93, a151, a152, a153, a631, a632, a633, a641, a642, a643/
sOneStableAnimals(sAnimalCategory)  /a14, a141, a194, a195, a93, a91, a92, a151, a152, a153, a165, a166, a631, a632, a633, a641, a642, a643/
sNEA(sStableType) /TRAD_MENGM, TRAD_STALM, UNKNOWN, BATT_NEA, GRONDH_NEA, BATT_OV, GRONDH_OV, SLK_OV, SLKO_OV, OSLKO_OV/
sEA(sStableType)
*sEA(sStableType) /V_1_4, S_2, V_3_4, V_3_5, V_3_6, V_4_6, P_2_1, P_2_3, P_4_5, P_5_1, P_5_4, P_5_5, P_6_3, P_6_5, P_7_5/
sEligbleStablesAEA(sStable) stables for which choice of AEA can be made (NEA + pigs or poultry)
sStableChoice(sStable, sStableType) ;

option sAEA <= pAEAextraCost       ;

sEA(sStableType) = yes ;

sEA(sNEA) = no ;
*sEA('P_2_1') = no ;
*sEA('P_4_1') = no ;
*sEA('S_1') = no ;
*sEA('P_6_1') = no;
*sEA('P_6_2') no ;


*Only AEA for poultry and pigs
sEligbleStablesAEA(sStable)   = sum(sSectors, (sum(sAnimalCategory, (sAnimalCategory_Stable(sAnimalCategory, sStable) * sSectors_Animalcategory('Varkens', sAnimalCategory)))
                                    + sum(sAnimalCategory, (sAnimalCategory_Stable(sAnimalCategory, sStable) * sSectors_Animalcategory('Pluimvee', sAnimalCategory)))));

*Only AEA choice when current stable Not AEA
sEligbleStablesAEA(sStable) = sum(sStableType, (sEligbleStablesAEA(sStable) * sStable_StableType(sStable, sStableType) * sNEA(sStableType))) ;


*option sInitialStab <= sID ;
*Option 1: all AEA to choose from
*sPosStab(sStable, sStableType, sAnimalCategory) =  sAEA(sAnimalCategory, sStableType) * sAnimalCategory_Stable(sAnimalCategory, sStable) ;
*Option 2: only best AEA to choose from
sPosStab(sStable, sEA, sAnimalCategory) =  sAEA(sAnimalCategory, sEA) * sAnimalCategory_Stable(sAnimalCategory, sStable) ;



parameter nrAnimals(sStable, sStableType)
          nrAnimals2(sStable, sStableType) used for selection of stables that can house all animal categories present ;

nrAnimals(sStable, sStableType) = sum(sPosStab(sStable, sStableType, sAnimalCategory) , 1) ;

nrAnimals2(sStable, sStableType)$((iPermittedStable(sStable) = nrAnimals(sStable, sStableType)) and sEligbleStablesAEA(sStable))   = nrAnimals(sStable, sStableType)  ;

*option sStable_Exploitation <= sID  ;
option sPossibleStables <=  nrAnimals2 ;

*All stables, including zero cost and current stable
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
*$ontext
*Exploitations
Scalar pSSthreshold /5/ ;

Parameter pOtherStables(sStable)
          pEmissionOthers(sExploitation) exploitations where sum emission others are bigger than threshold
          pBestEF(sAnimalCategory) best (lowest) EF for each animal category;

pOtherStables(sStable) = sum(sOtherAnimals, pAnimals(sStable, sOtherAnimals)) ;

loop (sAnimalCategory,
pBestEF(sAnimalCategory) = smin(sAnimalCategory_Stabletype(sAnimalCategory, sStableType), pEmissionFactor(sAnimalCategory, sStableType)) ;
) ;


Set
sStable_FarmType(sStable, sFarmType)
*stables that should be fixed (with slack variable in environmental constraint)
sFIX(sStable)
sNotFix(sStable)
sOther(sStable)
sAboveThresholdE(sExploitation)
sAboveThresholdS(sStable);

sStable_FarmType(sStable, sFarmType) = sum(sStable_Exploitation(sStable, sExploitation), sExploitation_FarmType(sExploitation, sFarmType)) ;

option sOther <= pOtherStables            ;

sFix(sStable) = yes;

*sFix(sStable)$(sum(sOtherAnimals, pAnimals(sStable, sOtherAnimals)) = no ;

*sFix(sStable)$(sum

sFix(sStable)$(sum(sFarmType, sStable_FarmType(sStable, sFarmType))) = no ;
sFix(sStable) = sFix(sStable) + sOther(sStable) ;

sNotFix(sStable) = yes ;
sNotFix(sFix) = no ;
*sFIX(sStable)$(not sum((sStable_Exploitation(sStable, sExploitation), sFarmType),sExploitation_FarmType(sExploitation, sFarmType)) = yes ;

pEmissionOthers(sExploitation) = sum((sOtherAnimals, sStable_Exploitation(sFix, sExploitation), sStable_Stabletype(sFIX, sSTableType)), (pAnimals(sFix, sOtherAnimals) * pEmissionFactor(sOtherAnimals, sStableType))) ;

sAboveThresholdE(sExploitation)$((pEmissionOthers(sExploitation)/5000) * pLocationImpact(sExploitation, 'SS')>pSSthreshold) = yes       ;

sAboveThresholdS(sStable)$(sum(sStable_Exploitation(sStable, sExploitation), sAboveThresholdE(sExploitation))) = yes;

sAboveThresholdS(sStable) = sAboveThresholdS(sStable) * sFix(sStable);

*s7125 non-eligble for AEA, but bigger than threshold: exception
*sAboveThresholdS('s7125') = yes ;
*sAboveThresholdS('s11426') = yes ;

*sAboveThresholdS(sStable)$(sum((, sStableType), (pAnimals(sFix, sOtherAnimals) * pEmissionFactor(sOtherAnimals, sStableType)))
*sNotFix(sExploitation)$((iMaxAmmoniaSource(sExploitation)/5000) * pLocationImpact(sExploitation, 'SS')>pSStreshold) = yes ;

*$offtext

execute_unloaddi 'SODat.gdx'

********************************************************************************
*********Declaration reporting variables****************************************
********************************************************************************



$ontext
Parameter
*modelstats
dModelStat(sScen)
dSolveStat(sScen)
dObjEst(sScen)
*global
dSumStableTypes(sScen, sStabletype)
dPASmeasures(sScen, sPAS)
dADSregion(sScen)
dAmmoniaRegion(sScen)
dProfitRegion(sScen)
*sector

*exploitation
dProfitExploitation(sScen, sExploitation)
*stable
dAbatementCost(sScen, sExploitation, sStable, sAbatement)
dAbatementCostAEA(sStable, sStableType)
;
$offtext
