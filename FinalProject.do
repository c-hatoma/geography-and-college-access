**********************
* Ivy Yang
* ECON 401
* Spirit Level Final Project
* Data Analysis
**********************

clear all

global dir "N:\Classes\Spring20\ECON0401A\WORKSPACE\ivyy\FinalProject"
global DO "$dir\DO"
global DATA "$dir\DATA"
global OUTPUT "$dir\OUTPUT"

cd "$DATA"

*** NOTE TO CHICA - ignore everything from here until line 160 ****


* edit and merge the data into one dataset
local create = 0
if `create' == 1{
*************** Colleges - Mobility Report Cards *******************

****** 1) Merge the two tables
use "mrc_table1-2.dta", clear

merge 1:1 name using "mrc_table2.dta"

drop _merge

****** 2) create new variables that indicate what you want

* number of colleges in cz
bysort cz: egen ncollege = count(name)
label var ncollege "number of colleges in commuting zone"

gen fouryr = (iclevel == 1)
bysort cz: egen nfouryr = total(fouryr)
label var nfouryr "number of 4-year colleges in commuting zone"

gen fouryrpriv = (iclevel == 1 & type == 2)
bysort cz: egen nfouryrpriv = total(fouryrpriv)
label var nfouryrpriv "number of private, non-profit 4-year colleges in commuting zone"

gen pub = (type == 1)
by cz: egen npub = total(pub)
label var npub "number of public colleges in commuting zone"

gen elite = (tier <= 4)
by cz: egen nelite = total(elite)
label var nelite "number of elite and highly selective private and public schools in cz"

save "colleges.dta", replace

**** 3) collapse the dataset by commuting zone
collapse (mean) par_median k_median par_q1 par_top1pc kq5_cond_parq1 ktop1pc_cond_parq1 mr_kq5_pq1 mr_ktop1_pq1 ncollege nfouryr nfouryrpriv npub nelite, by(cz czname)

drop if cz == . 
save "cz_college.dta", replace

***************** Mobility Data by Commuting Zone *********************
set maxvar 100000

use "cz_outcomes.dta", clear

* drop a bunch of variables to save space and declutter 
drop has_* jail* marr* spouse* stayhome_* two_par* frac_years*

drop *natam*

drop kfr_2* kfr_imm* kfr_native* kfr_stycz* kir_2* kir_imm* kir_native* kir_stycz* pos_hours* working_* hours_wk_* somecoll_* work* *_se frac_below_* kid_* staytract*

** Merge**
* commuting zone characteristics dataset 
merge 1:1 cz using "cz_covariates.dta"

drop _merge

* college data by commuting zone
merge 1:1 cz using "cz_college.dta"

drop _merge

foreach x of varlist ncollege nfouryr nfouryrpriv npub nelite {
	replace `x' = 0 if `x' == .
}

gen hascollege = (ncollege >0)

save "cz.dta", replace

}

******* Investigation of Absolute Income Differences ***************
local try = 0
if `try' == 1{
** look at the percent to dollar crosswalks
use "pctile_to_dollar_cw.dta", clear

gen absdiff = kid_hh_income / parent_hh_income
label var absdiff "proportion of parent income that child earns"

gen absdiff26 = kid_hh_income_age26 / parent_hh_income
label var absdiff26 "proportion of parent income that child earns at age 26"

* scattergram of percentile rank and absolute mobility
scatter absdiff percentile

cd "$OUTPUT" 
graph export "absmobility.png", as(png) replace

scatter absdiff26 percentile


** Look at College Mobility **
use "colleges.dta", clear


gen absmeandiff = k_mean / par_mean
label var absmeandiff "proportion of parent income that child earns - mean"

gen absmeddiff = k_median / par_median 
label var absmeddiff "proportion of parent income that child earns - median"


* create your own tiers - no for-profit colleges
* 0 = two year, 1 = non selective priv / public, 2 - selective public, 3 - selective private, 4 - highly selective and above
gen mytier = 0 if iclevel >=2 
replace mytier = 1 if tier == 8 | tier == 7
replace mytier = 2 if tier == 5
replace mytier = 3 if tier == 6
replace mytier = 4 if elite == 1
replace mytier = . if type == 3
label def tiers 0 "two year" 1 "non selective priv and pub" 2 "selective public" 3 "selective private" 4 "highly selective and above"
label values mytier tiers

hist absmeddiff, by(mytier)
cd "$OUTPUT" 
graph export "histCollegeMobility.png", as(png) replace

twoway (scatter absmeandiff k_mean) (lfit absmeandiff k_mean) 

twoway (scatter absmeddiff k_median) (lfit absmeddiff k_median), title(Abolute Mobility vs Child Median Income)

cd "$OUTPUT"
graph export "absmobilityKidMedian.png", as(png) replace

twoway (scatter absmeddiff par_median) (lfit absmeddiff par_median), title(Absolute Mobility vs Parent Median Income)
graph export "absmobilityParent.png", as(png) replace

twoway (scatter absmeddiff k_median) (lfit absmeddiff k_median), by(mytier)
graph export "mobilitybyTier.png", as(png) replace


twoway (scatter absmeddiff par_median) (lfit absmeddiff par_median), by(mytier)
graph export "mobilityParentbyTier.png", as(png) replace

}


****************** Summary Figures and Tables ??? *************************
cd "$DATA"
use "cz.dta", clear

* kir = mean percentile rank in national dist'n of individual incomes

* kfr = mean percentile rank in national dist'n household income 

* p1 = lowest percentile, p100 is the highest percentile

* create a summary stats table

cd "$OUTPUT"
outreg2 using sumstats, excel replace sum(log) keep(kfr_pooled_pooled_p1 kfr_pooled_pooled_p25 kfr_pooled_pooled_p50 kfr_pooled_pooled_p75 kfr_pooled_pooled_p100 kfr_top20_pooled_pooled_p50 coll_pooled_pooled_mean  popdensity2010 med_hhinc2016 ncollege nfouryr nfouryrpriv npub nelite)


* make some figures - just playing around here but feel free to check them out

* a scatterplot of kfr_pooled_pooled_mean(y), which is the predictedmean household income rank of children, and n college (x) with a line of best fit
* I would probably use the median instead of the mean bc I think it's easier to interpret

twoway (scatter kfr_pooled_pooled_mean ncollege) (lfit kfr_pooled_pooled_mean ncollege)

* a scatterplot of kir_pooled_pooled_mean(y), which is the predicted mean individual income rank of children, and n college (x) with a line of best fit
* I would probably use the median instead of the mean bc I think it's easier to interpret

twoway (scatter kir_pooled_pooled_mean ncollege) (lfit kir_pooled_pooled_mean ncollege)

* a scatterplot of lpov_nbh_pooled_pooled_mean(y) and n college (x) with a line of best fit

twoway (scatter lpov_nbh_pooled_pooled_mean ncollege) (lfit lpov_nbh_pooled_pooled_mean ncollege)

* play around with different combinations - so many possible combinations 
* a scatterplot of kfr_black_pooled_p1 (y) and ncollege (x) with a line of best fit. 
*The Y variable is predicted household income of black kids from the bottom 1%

twoway (scatter kfr_black_pooled_p1 ncollege) (lfit kfr_black_pooled_p1 ncollege)



************************* Regressions **********************************
ssc install outreg2
cd "$OUTPUT"

* in the output tables I uploaded, each file has the regressions for one X variable. There are 6 X variables in total (ncollege, nfouryr, nfouryrpriv, npub, nelite, and hascollege)
* ex. the "ncollege.xlsx" file has the results for:
* 1) kfr_pooled_pooled, 2) kfr_top20_pooled_pooled, 3) kfr_white_pooled, and 4) kfr_black_pooled
* each sheet has 5 columns with regressions for each of the parental percentiles, 1) bottom 1, 2) 25%, 3) 50%, 4) 75%, 5) top percentile
* so in total each file has 4 sheets with 5 regressions each.


*** 1 ***
* regressions of Y =  kfr_pooled_pooled_[percentile = p1, p25, p50, p75, and p100], or the predicted mean percentile rank given parent income
* on X = type of college (ncollege, nfouryr, nfouryrpriv, npub, nelite, and hascollege). For kids of all races and all genders
* controls are population density 2010 and median household income 2016
* the output is 6 separate sheets of 5 regressions each, one for each percentile of the variable

foreach x of varlist ncollege nfouryr nfouryrpriv npub nelite hascollege{
	
foreach y of varlist kfr_pooled_pooled_p1 kfr_pooled_pooled_p25 kfr_pooled_pooled_p50 kfr_pooled_pooled_p75 kfr_pooled_pooled_p100 {
	reg `y' `x' popdensity2010 med_hhinc2016, r 
	outreg2 using `x'_kfr, excel append ctitle(`y')
}

}

*** 2 ***
* regressions of Y =  kfr_top20_pooled_pooled_[percentile = p1, p25, p50, p75, and p100], or the predicted probability 
* that the child with parents from the given percentile rank make it to the top 20%
* on X = type of college (ncollege, nfouryr, nfouryrpriv, npub, nelite, and hascollege). For kids of all races and all genders
* controls are population density 2010 and median household income 2016

foreach x of varlist ncollege nfouryr nfouryrpriv npub nelite hascollege{
	
foreach y of varlist kfr_top20_pooled_pooled_p1 kfr_top20_pooled_pooled_p25 kfr_top20_pooled_pooled_p50 kfr_top20_pooled_pooled_p75 kfr_top20_pooled_pooled_p100 {
	reg `y' `x' popdensity2010 med_hhinc2016, r 
	outreg2 using `x'_kfrtop20, excel append ctitle(`y')
}

}


*** 3 ***
* regressions of Y =  kfr_white_pooled_[percentile = p1, p25, p50, p75, and p100], or the predicted mean percentile rank given parent income
* on X = type of college (ncollege, nfouryr, nfouryrpriv, npub, nelite, and hascollege). For kids who are WHITE of all genders
* controls are population density 2010 and median household income 2016

foreach x of varlist ncollege nfouryr nfouryrpriv npub nelite hascollege{
	
foreach y of varlist kfr_white_pooled_p1 kfr_white_pooled_p25 kfr_white_pooled_p50 kfr_white_pooled_p75 kfr_white_pooled_p100 {
	reg `y' `x' popdensity2010 med_hhinc2016, r 
	outreg2 using `x'_kfr_white, excel append ctitle(`y')
}

}

*** 4 ***
* regressions of Y =  kfr_black_pooled_[percentile = p1, p25, p50, p75, and p100], or the predicted mean percentile rank given parent income
* on X = type of college (ncollege, nfouryr, nfouryrpriv, npub, nelite, and hascollege). For kids who are BLACK of all genders
* controls are population density 2010 and median household income 2016

foreach x of varlist ncollege nfouryr nfouryrpriv npub nelite hascollege{
	
foreach y of varlist kfr_black_pooled_p1 kfr_black_pooled_p25 kfr_black_pooled_p50 kfr_black_pooled_p75 kfr_black_pooled_p100 {
	reg `y' `x' popdensity2010 med_hhinc2016, r 
	outreg2 using `x'_kfr_black, excel append ctitle(`y')
}

}

