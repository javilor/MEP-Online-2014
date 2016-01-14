************************************************************
****** Going Public Against Institutional Constraints ******
********* Lorenzo Rodriguez & Garmendia Madariaga **********
******************** January 13, 2016 **********************
************************************************************

clear 
use "EUP_goingpubliconline.dta"
set more off


** Figure 1. European candidates' online presence during 2014 EP elections ** 

histogram onlin_presence_four, percent

// Polychoric factor analysis//

** For dummy and ordered variables **
polychoric facebook website twitter linkedin
global N = r(sum_w)
matrix r = r(R)
factormat r, n($N) factors(1)
rotate, varimax horst blanks(.0)

// Zero-Inflated Poisson Model //

gen lnfollowface= ln(fb_party_followers)
gen lnfollowtwi= ln(tw_party_followers)
save "EUP_goingpubliconline.dta",replace

zip online_presence gender incumbent major_party lnfollowface lnfollowtwi isic_hous_aces typeoflist, inflate(gender incumbent major_party isic_hous_aces typeoflist) vuong
zip online_presence gender incumbent major_party lnfollowface lnfollowtwi isic_hous_aces typeoflist, inflate(gender incumbent major_party isic_hous_aces typeoflist) cluster(id_country)

** Figure 2. Determinants of laggards from a ZIP model ** 

** a. Ballot Structure **

zip online_presence gender incumbent major_party lnfollowface lnfollowtwi isic_hous_aces typeoflist, inflate(gender incumbent major_party isic_hous_aces typeoflist) cluster(id_country)
quietly mtable, at(gender=0 incumbent=0 major_party=0 lnfollowface=10.13 lnfollowtwi=9.69 isic_hous_aces=79.91 typeoflist=.505) ///
     atvars(_none) pr(0/7)
mtable, at(gender=0 incumbent=0 major_party=0 lnfollowface=10.13 lnfollowtwi=9.69 isic_hous_aces=79.91 typeoflist=.505) ///
  	atvars(_none) predict(pr)
mgen, at (typeoflist=(0/2)) atmeans pr(0) stub(ZIP) replace
mgen, at (typeoflist=(0/2)) atmeans predict(pr) stub(ZIP) replace
gen ZIPprcount0 = ZIPprany0 - ZIPprall0
label var ZIPprall0 "Always Zero from Binary Equation"
label var ZIPprcount0 "Sometimes Zero from Count Equation"
label var ZIPprany0 "Zeroes from Both Equations"
graph twoway connected ZIPprall0 ZIPprcount0 ZIPprany0 ZIPtypeoflist, ///
    xlabel(0/2) ylabel(0(.1).5, gmax) ///
    ytitle(Probability of Zero) msymbol(Sh Dh O) legend(rows(3))

** b. Percentage of households with Internet access ** 

clear 
use "EUP_goingpubliconline.dta"
zip online_presence gender incumbent major_party lnfollowface lnfollowtwi isic_hous_aces typeoflist, inflate(gender incumbent major_party isic_hous_aces typeoflist) cluster(id_country)
quietly mtable, at(gender=0 incumbent=0 major_party=0 lnfollowface=10.13 lnfollowtwi=9.69 isic_hous_aces=79.91 typeoflist=.505) ///
    atvars(_none) pr(0/7)
mtable, at(gender=0 incumbent=0 major_party=0 lnfollowface=10.13 lnfollowtwi=9.69 isic_hous_aces=79.91 typeoflist=.505) ///
  	atvars(_none) predict(pr)
mgen, at (isic_hous_aces =(57/96)) atmeans pr(0) stub(ZIP) replace
mgen, at (isic_hous_aces =(57/96)) atmeans predict(pr) stub(ZIP) replace
gen ZIPprcount0 = ZIPprany0 - ZIPprall0
label var ZIPprall0 "Always Zero from Binary Equation"
label var ZIPprcount0 "Sometimes Zero from Count Equation"
label var ZIPprany0 "Zeroes from Both Equations"
graph twoway connected ZIPprall0 ZIPprcount0 ZIPprany0 ZIPisic_hous_aces, ///
    xlabel(57/96) ylabel(0(.1).5, gmax) ///
    ytitle(Probability of Zero) msymbol(Sh Dh O) legend(rows(3))
    
** Figure 3. Between country variation regarding candidates' online presence ** 

gen grandmean=2.24192
gen grandmean_reduced=3.699844
gen candidate_age_0=candidate_age
replace candidate_age_0=0 if candidate_age_0==.
by id_country: egen average_country_presence = mean(online_presence)
by id_country: egen average_country_presence_reduced = mean(online_presence) if candidate_age_0>0
twoway scatter average_country_presence_reduced id_country, msize(small) || connected grandmean_reduced id_country, connect(L) clwidth(thick) clcolor(black) mcolor(black) msymbol(none) || , xlabel(1 "Austria" 2 "Belgium" 3 "Bulgaria" 4 "Cyprus" 5 "Czech Republic" 6 "Denmark" 7 "Estonia" 8 "Finland" 9 "France" 10 "Germany" 11 "Greece" 12 "Hungary" 13 "Ireland" 14 "Italy" 15 "Lithuania" 16 "Latvia" 17 "Luxembourg" 18 "Malta" 19 "Netherlands" 20 "Poland" 21 "Portugal" 22 "Romania" 23 "Slovakia" 24 "Slovenia" 25 "Spain" 26 "Sweden" 27 "United Kingdom", angle(45)) yscale(range (0 6)) ylabel(0(1)6) ytitle()  legend(rows(2) lab(1 "Elected candidates' average online presence by country") lab(2 "Grand mean of elected candidates' online presence in Europe") stack)

** Three level variance hierarchical models **

drop if candidate_age==.
****a****
xtmixed online_presence  || id_country: || party_id:, var
****b****
xtmixed online_presence gender incumbent candidate_age candidate_education major_party isic_hous_aces typeoflist || id_country: || party_id:, var
****c****
xtmixed online_presence gender incumbent candidate_age candidate_education major_party lnfollowface lnfollowtwi isic_hous_aces typeoflist || id_country: || party_id:, var

******************************************** R^2=1?(VarNew(Y)/VarOld(Y)) *****
***************************************

**************** Model 1 *******************

*** omega1 R^2 ***
di 1-(0.253/0.446)

*** tau1 R^2 ***
di 1-(0.339/0.375)

*** sigma1 R^2 ***
di 1-(2.302/2.549)

**************** Model 2 ********************

*** omega1 R^2 ***
di 1-(0.017/0.446)

*** tau1 R^2 ***
di 1-(0.190/0.375)

*** sigma1 R^2 ***
di 1-(2.208/2.549)
