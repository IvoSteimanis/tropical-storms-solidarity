/*==============================================================================
Project:     Moderate disaster exposure divides communities; severe exposure does not
File:        01_clean_data.do
Purpose:     Clean and construct analysis variables from panel dataset
Input:       PHI_Panel_12_16_22.dta
Output:      analysis_rdy.dta
Author:      Ivo Steimanis
Date:        Created: January 2022 | Last modified: March 2026
Notes:       - Panel covers 2012 (pre-Haiyan), 2016 (post-Haiyan), 2022 (long-run)
             - Typhoon Haiyan occurred November 2013
             - Main outcome: solidarity transfers in experimental games
==============================================================================*/

* Load panel dataset
use "$working_ANALYSIS/data/PHI_Panel_12_16_22.dta", replace


/*------------------------------------------------------------------------------
1. IDENTIFIERS AND MERGES
------------------------------------------------------------------------------*/

* Generate session identifier from village name
egen session = group(village)

* Merge village-level storm proximity data
merge m:1 session using "$working_ANALYSIS/data/distance_storm_km.dta"
tab _merge
assert _merge == 3
drop _merge


/*------------------------------------------------------------------------------
2. TREATMENT: TYPHOON HAIYAN INTENSITY
------------------------------------------------------------------------------*/

* Reverse distance metric so higher values = more affected
su distance_storm_km, meanonly
gen distance_storm_km_reversed = r(max) - distance_storm_km

* Normalize distance to 0-100 index
gen distance_storm_km_index = ((distance_storm_km - r(min)) / (r(max) - r(min))) * 100
gen intensity_cont = 100 - distance_storm_km_index

* Generate squared term for non-linear effects
gen distance_rev_sqr = distance_storm_km_reversed * distance_storm_km_reversed

* Label variables
lab var distance_storm_km_reversed "Reversed distance to eye of storm (km)"
lab var distance_rev_sqr "Reversed distance squared"

* Indicator for moderately affected villages
* Keyed on session IDs (= group(village)) so it is stable under village
* anonymization; sessions 17 and 20 are the two medium-exposure villages.
gen medium_affected_village = inlist(session, 17, 20)


/*------------------------------------------------------------------------------
3. PANEL STRUCTURE AND PARTICIPATION
------------------------------------------------------------------------------*/

sort panel_id year

* Count waves participated per individual
isid panel_id year, sort
by panel_id: gen n_years_participated = _N
tab n_years_participated if year == 2022

* Generate participation indicators using forward/backward references
gen particip_2022 = 0
replace particip_2022 = 1 if year == 2012 & F10.age != .
replace particip_2022 = 1 if year == 2016 & F6.age != .

replace particip_2012 = 1 if year == 2022 & L10.age != .
replace particip_2016 = 1 if year == 2022 & L6.age != .

* Balanced panel indicator (participated all three waves)
gen particip_12_16_22 = 0
replace particip_12_16_22 = 1 if year == 2012 & particip_2016 == 1 & particip_2022 == 1
replace particip_12_16_22 = 1 if year == 2016 & particip_2012 == 1 & particip_2022 == 1
replace particip_12_16_22 = 1 if year == 2022 & particip_2012 == 1 & particip_2016 == 1

* Returner indicator (participated in 2012 and 2016)
gen returner = (particip_2012 == 1 & particip_2016 == 1 & year <= 2016)
lab def returneeL 0 "Nonreturner" 1 "Returner", replace
lab val returner returneeL


/*------------------------------------------------------------------------------
4. INCOME AND WEALTH VARIABLES
------------------------------------------------------------------------------*/

* Log monthly household income
gen log_ymonth = ln(ymonth + 1)

* Changes in income (relative to previous wave)
gen d_ymonth = ymonth - l.ymonth
gen d_log_ymonth = log_ymonth - l1.log_ymonth

* Household size adjustment (square root scale)
gen sqr_hh_size = sqrt(hh_size)
gen adj_log_ymonth = log_ymonth / sqr_hh_size
replace adj_log_ymonth = log_ymonth if hh_size == 0

* Impute missing income and financial indicators with year-specific means
bys year: sum log_ymonth s_1000php debt_5000 meals

* Generate temporary mean variables by year
bys year: egen mean_log_ymonth = mean(log_ymonth)
bys year: egen mean_s_1000php = mean(s_1000php)
bys year: egen mean_debt_5000 = mean(debt_5000)

* Replace missing values with year-specific means
* 2012: 1 missing income observation
replace log_ymonth = mean_log_ymonth if log_ymonth == . & year == 2012
* 2022: 3 missing income observations
replace log_ymonth = mean_log_ymonth if log_ymonth == . & year == 2022
replace s_1000php = mean_s_1000php if s_1000php == . & year == 2022
replace debt_5000 = mean_debt_5000 if debt_5000 == . & year == 2022

* Clean up temporary variables
drop mean_log_ymonth mean_s_1000php mean_debt_5000

* Construct wealth index via PCA (income, savings, debt, food insecurity)
pca adj_log_ymonth s_1000php debt_5000 meals
predict wealth_index
lab var wealth_index "Wealth index (PCA)"


/*------------------------------------------------------------------------------
5. TYPHOON HAIYAN DAMAGE MEASURES
------------------------------------------------------------------------------*/
sort panel_id year

* Wave indicator for 2016 (post-Haiyan)
gen y16 = .
replace y16 = 1 if year == 2016
replace y16 = 0 if year == 2012
label define year1 0 "2012" 1 "2016"
label value y16 year1

* Distance to storm categorical variable
gen distance_cat = 1 if distance_storm_km_reversed < 40
replace distance_cat = 2 if distance_storm_km_reversed >= 40 & distance_storm_km_reversed < 80
replace distance_cat = 3 if distance_storm_km_reversed >= 80
lab def disti 1 "Low (<40km)" 2 "Medium (40-80km)" 3 "High (>80km)", replace
lab values distance_cat disti
tab distance_cat, gen(dist_cat)

* Wind-speed categorical variable (Saffir-Simpson based; primary categorical spec)
* Cutoffs at the Cat 3 (96 kn) and Cat 4 (113 kn) lower bounds. On this sample the partition
* coincides EXACTLY with distance_cat (cross-tab perfectly diagonal: 14/2/14 villages,
* 378/54/378 obs in 2016), so categorical estimates are unchanged; only the exposure
* metric of the labels changes.
preserve
use "$working_ANALYSIS/data/windspeed_predicted.dta", clear
bys session: egen _wsmax = max(windspeed_predicted)
keep session _wsmax
duplicates drop
tempfile wsv
save `wsv'
restore
merge m:1 session using `wsv', keep(1 3) nogen
sort panel_id year   // merge re-sorts; restore panel order for downstream ts operators
gen ws_cat3 = 1 if _wsmax < 96
replace ws_cat3 = 2 if _wsmax >= 96 & _wsmax < 113
replace ws_cat3 = 3 if _wsmax >= 113 & !missing(_wsmax)
drop _wsmax
lab def wscat3i 1 "Low (<96 kn)" 2 "Medium (Cat 3: 96-112 kn)" 3 "High (>=113 kn)", replace
lab values ws_cat3 wscat3i
lab var ws_cat3 "Wind-speed category (Saffir-Simpson based)"
tab ws_cat3 distance_cat, missing   // must be diagonal; see note above

* Village-level damage aggregates
foreach x of varlist h6need h2house h2bike h2boat h2car h2crop h2work h4finan h4perso {
    egen village_mean_`x' = mean(`x') if y16 == 1, by(session)
}

egen share_houses_damaged = mean(h2house) if y16 == 1, by(session)
egen share_need_aid = mean(h6need) if y16 == 1, by(session)

* Individual-level damage indices
egen damage_index = rowtotal(h2house h2bike h2boat h2car h2crop h2work)
egen avg_damage_index = mean(damage_index) if y16 == 1, by(session)

egen damage_TOTAL = rowtotal(h2housec h2bikec h2carc h2boatc h2workc h2cropc)
replace damage_TOTAL = . if year != 2016

egen mean_dmg = mean(damage_TOTAL) if year == 2016, by(session)
replace mean_dmg = mean_dmg / 1000

*winsorize extreme outliers
winsor4 damage_TOTAL, method(winsor) outlier(tail) level(5)


* Severely affected dummy based on total damages and house damage
gen severely_damaged = (damage_TOTAL > 4500) & (h2house == 1)

*Restrict to 2016: severely_damaged is mechanically 0 in 2012/2022 (damage questions are
*2016-only); averaging over all waves would dilute the village share by ~23pp and
*overstate diff_severe_damage_aid by the same amount.
egen share_severe_damage = mean(severely_damaged) if y16 == 1, by(session)
gen diff_severe_damage_aid = (share_need_aid-share_severe_damage)*100

gen diff_share_houses_aid = (share_need_aid-share_houses_damaged)*100
lab var diff_share_houses_aid "Potential overreporting of being in need of aid after Haiyan."



* House condition worsened
gen house_worse = 0 if y16 == 1
replace house_worse = 1 if h2houser == 0 & y16 == 1

* Food insecurity (reduced meals)
gen d_meals = meals - l4.meals

gen reduce_meals = 0 if meals == 0 & l.meals == 0
replace reduce_meals = 1 if meals == 1 | l1.meals == 1
replace reduce_meals = 2 if meals == 1 & l1.meals == 1
replace reduce_meals = . if returner == 0 | y16 == 0
lab def never_once_twice 0 "Never" 1 "Once" 2 "Twice", replace
lab val reduce_meals never_once_twice


/*------------------------------------------------------------------------------
6. EXPERIMENTAL OUTCOMES: SOLIDARITY TRANSFERS
------------------------------------------------------------------------------*/

* Average solidarity transfers across game rounds
* Strangers condition: mean across all four decisions
egen mean_transfer_stranger = rowmean(transfer_person1 transfer_person2 ///
                                      transfer2_person1 transfer2_person2) if stranger == 1

* Co-villager condition: mean across second-person decisions only
egen mean_transfer = rowmean(transfer_person2 transfer2_person2) if stranger == 0
replace mean_transfer = mean_transfer_stranger if stranger == 1
lab var mean_transfer "Solidarity transfer (0,70)"
lab var stranger "Stranger (=1)"

* Expected transfers
egen mean_exp_stranger = rowmean(exp_person1 exp_person2) if stranger == 1
gen exp_transfer = exp_person2 if stranger == 0
replace exp_transfer = mean_exp_stranger if stranger == 1
lab var exp_transfer "Expected transfer (0,70)"


/*------------------------------------------------------------------------------
7. SOCIODEMOGRAPHIC CONTROLS
------------------------------------------------------------------------------*/

* Education dummies
tab educ, gen(edu_)

* Food insecurity dummies
tab reduce_meals, gen(meals_)

* Marital status
gen married = (status == 2)

* Housing materials (improved housing indicator)
gen improved_house = 0 if y16 == 1
replace improved_house = 1 if (h5iron == 1 | h5stone == 1) & y16 == 1

* Trust and attitudes (impute missing with modal category)
replace a3gov = 4 if a3gov == .
replace a1trust = 0 if a1trust == .
replace a1opti = 2 if a1opti == .

lab var a3gov "Trust in national government"
lab var a1trust "Generalized trust (=1)"
lab var a1opti "General optimism"


/*------------------------------------------------------------------------------
8. VILLAGE-LEVEL AFFECTEDNESS INDEX (FACTOR ANALYSIS)
------------------------------------------------------------------------------*/

* Construct latent damage measure using exogenous distance and self-reports
pwcorr share_houses_damaged avg_damage_index mean_dmg share_need_aid ///
       village_mean_h4finan village_mean_h4perso

factor share_houses_damaged avg_damage_index mean_dmg share_need_aid ///
       village_mean_h4finan village_mean_h4perso, pcf
rotate
predict factor1_village_mean
rename factor1_village_mean affectedness_index
lab var affectedness_index "Affectedness index (Factor analysis)"

gen affectedness_index_sqr = affectedness_index * affectedness_index
lab var affectedness_index_sqr "Affectedness index squared"


/*------------------------------------------------------------------------------
9. INDIVIDUAL-LEVEL AFFECTEDNESS INDEX (FACTOR ANALYSIS)
------------------------------------------------------------------------------*/

* Recode recovery indicators (worse condition = 1)
foreach x in h2houser h2biker h2carr h2boatr h2workr h2cropr {
    gen d_`x' = (missing(`x') == 0 & `x' == 0)
}

* Count assets in worse condition than pre-Haiyan
egen assets_worse_condition = rowtotal(d_h2houser d_h2biker d_h2carr ///
                                       d_h2boatr d_h2workr d_h2cropr)
replace assets_worse_condition = . if year != 2016

* Average recovery time across damaged assets
egen avg_recovery_time = rowmean(h2housed h2biked h2card h2boatd h2workd h2cropd)
replace avg_recovery_time = 0 if avg_recovery_time == . & year == 2016

* Factor analysis of individual affectedness
pwcorr h6need h2house damage_index damage_TOTAL avg_recovery_time ///
       h4finan h4perso assets_worse_condition

factor h6need h2house damage_index damage_TOTAL avg_recovery_time ///
       h4finan h4perso assets_worse_condition
rotate
predict factor1_individual
rename factor1_individual affectedness_FA
lab var affectedness_FA "Affectedness index (Factor analysis)"


/*------------------------------------------------------------------------------
10. PARTICIPANT COUNTS AND VILLAGE SIZE
------------------------------------------------------------------------------*/

* Count returners per village
sort panel_id year
egen count_village12_ubp = count(panel_id) if year == 2012, by(session)
egen count_village = count(panel_id) if year == 2016 & returner == 1, by(session)
replace count_village = count_village[_n+1] if count_village == . & year != 2022


/*------------------------------------------------------------------------------
11. CHANGES IN OUTCOMES (2016 - 2012)
------------------------------------------------------------------------------*/

* First differences in experimental outcomes
gen d_transfer_person2 = transfer_person2 - l4.transfer_person2
gen d_mean_transfer = mean_transfer - l4.mean_transfer
lab var d_mean_transfer "Change in transfers"
gen d_exp_transfer = exp_transfer - l4.exp_transfer

* First differences in attitudes
gen d_a1trust = a1trust - l4.a1trust
gen d_a3gov = a3gov - l4.a3gov
gen d_a1opti = a1opti - l4.a1opti

* First difference in wealth
gen d_wealth = wealth_index - l4.wealth_index

* Standardized transfers and changes
egen mean_transfer_std = std(mean_transfer)
gen d_mean_transfer_std = mean_transfer_std - l4.mean_transfer_std


/*------------------------------------------------------------------------------
12. BASELINE SOLIDARITY RELATIVE TO VILLAGE MEAN
------------------------------------------------------------------------------*/

* Baseline (2012) solidarity level
gen baseline_solidarity = mean_transfer - d_mean_transfer if year == 2016
bys year h6need: sum baseline_solidarity

* Village mean baseline solidarity
egen baseline_mean = mean(baseline_solidarity), by(session)

* Individual deviation from village baseline mean
gen diff_baseline_to_mean = baseline_solidarity - baseline_mean
sort panel_id year
replace diff_baseline_to_mean = diff_baseline_to_mean[_n+1] if ///
        diff_baseline_to_mean == . & returner == 1
bys year: sum diff_baseline_to_mean

lab var diff_baseline_to_mean "Positive values: contributed more than village average in 2012"

* Orthogonalized baseline deviation (residualize for later use)
reg diff_baseline_to_mean if year == 2012, cluster(session)
predict p_diff_baseline_to_mean if year == 2012, resid
sort panel_id year
replace p_diff_baseline_to_mean = p_diff_baseline_to_mean[_n-1] if ///
        p_diff_baseline_to_mean == . & returner == 1


/*------------------------------------------------------------------------------
13. HOUSEHOLD HEAD INDICATOR (2022 CORRECTION)
------------------------------------------------------------------------------*/

* Create dummy for whether household head was interviewed in 2022
gen hh_head_d = 0 if year == 2022
replace hh_head_d = 1 if year == 2022 & hh_head == 1

replace hh_head = 0 if hh_head_d == 0 & year == 2022
replace hh_head = 1 if hh_head_d == 1 & year == 2022


/*------------------------------------------------------------------------------
14. MECHANISM: PRE-HAIYAN VULNERABILITY (2012)
------------------------------------------------------------------------------*/

* --- Median splits for vulnerability dimensions ---

* High socioeconomic status (above median wealth in 2012)
egen median_wealth = median(wealth_index) if year == 2012 & returner == 1
gen high_SES12 = (wealth_index >= median_wealth) if year == 2012 & returner == 1
replace high_SES12 = high_SES12[_n-1] if missing(high_SES12) & year != 2022 & returner == 1
lab var high_SES12 "SES (>median)"
lab def sessie 0 "Low SES" 1 "High SES", replace
lab val high_SES12 sessie

* High age (above median age in 2012)
egen median_age = median(age) if year == 2012 & returner == 1
gen age_above41 = (age >= median_age) if year == 2012 & returner == 1
replace age_above41 = age_above41[_n-1] if missing(age_above41) & year != 2022 & returner == 1
lab var age_above41 "Age (>41)"
lab def agie 0 "Age ≤41" 1 "Age >41", replace
lab val age_above41 agie

* High education (beyond primary)
gen high_educ = (educ > 1) if year == 2012 & returner == 1
replace high_educ = high_educ[_n-1] if missing(high_educ) & year != 2022 & returner == 1
lab var high_educ "Education (>median)"
lab def edie 0 "Primary" 1 "≥ High school", replace
lab val high_educ edie
gen low_educ = 1 - high_educ


* --- Multidimensional vulnerability index ---

* Household composition variables
gen female_hh_head = (hh_head == 1 & gender == 1)

gen child_under7 = (age6 > 0) if age6 != .
gen elderly_present = (age60 > 0) if age60 != .
gen widowed = (status == 4)

* Forward-fill 2016 household composition to 2012 for returners
replace age6 = age6[_n+1] if missing(age6) & year != 2022 & returner == 1
replace age60 = age60[_n+1] if missing(age60) & year != 2022 & returner == 1
replace child_under7 = child_under7[_n+1] if missing(child_under7) & year != 2022 & returner == 1
replace elderly_present = elderly_present[_n+1] if missing(elderly_present) & year != 2022 & returner == 1

* No savings indicator
gen no_savings = (s_1000php == 0 & s_5000php == 0)

* Standardize continuous vulnerability variables
foreach var in ymonth hh_size age {
    egen z_`var' = std(`var')
}

* Define vulnerability variable sets
global vuln_vars log_ymonth hh_size gender age child_under7 elderly_present low_educ
global vuln_vars_clustering z_ymonth z_hh_size gender z_age child_under7 elderly_present low_educ

* --- Economic-vulnerability index (pre-treatment coping-PCA) for Fig. 2c heterogeneity ---
* Pre-specified theory of change: economically vulnerable households drive the U-shape.
* We operationalize vulnerability as the first principal component of seven BASELINE 2012
* economic-vulnerability characteristics (household size, children under seven, elderly
* members, adjusted log income, savings, debt, food insecurity), estimated on 2012
* returners only, oriented so higher = more vulnerable, then split into tertiles. Because
* the index uses pre-Haiyan data only and no outcome, the groups are exogenous to exposure
* (unlike a probit supervised on the post-disaster aid-need item, which sorted partly on
* damage). Used by 02_analysis_main.do (Fig. 2c) and 03_analysis_SI.do (Table S20).
global coping hh_size child_under7 elderly_present adj_log_ymonth s_1000php debt_5000 meals
pca $coping if year==2012 & returner==1, comp(1)
predict vuln_pc_cop if year==2012 & returner==1, score
quietly corr vuln_pc_cop adj_log_ymonth if year==2012 & returner==1
if r(rho) > 0 replace vuln_pc_cop = -vuln_pc_cop   // orient: higher score = lower income = more vulnerable
lab var vuln_pc_cop "Economic-vulnerability index (baseline coping-PCA, higher=more vulnerable)"
xtile tmp_vg = vuln_pc_cop if year==2012 & returner==1, nq(3)
* place the 2012 tertile on the household's 2016 row (matches the former need_group: 2016-only)
sort panel_id year
tsset panel_id year
gen vuln_group = l4.tmp_vg if year==2016
drop tmp_vg
label define vulnlab 1 "Least vulnerable" 2 "Middle" 3 "Most vulnerable", replace
label values vuln_group vulnlab
lab var vuln_group "Economic-vulnerability tertile (baseline coping-PCA; 1=least, 3=most)"
* standardized CONTINUOUS index on the 2016 row (primary moderator for Fig. 2c)
egen vuln_z2012 = std(vuln_pc_cop) if year==2012 & returner==1
gen vuln_cont = l4.vuln_z2012 if year==2016
drop vuln_z2012
lab var vuln_cont "Economic-vulnerability index (z, baseline; higher=more vulnerable)"
sort panel_id year





/*------------------------------------------------------------------------------
15. MECHANISM: POST-HAIYAN SOCIAL DYNAMICS (2016)
------------------------------------------------------------------------------*/

* --- (1) Perceived social network damage ---

* Recode "don't know" responses (999) as "never happened" (1)
global aid_corruption h11rela h11soon h11vuln h11pay h11enti h11eno ///
                       h11corr h11share h11self h11wom h11extra h11promi h11rot
foreach x of varlist $aid_corruption {
    replace `x' = 1 if `x' == 999
}

* Reliability check
alpha h3neigh h3friend h3fam h3friendoth h3famoth
* alpha = 0.93

* PCA to generate social network damage index
pca h3neigh h3friend h3fam h3friendoth h3famoth, comp(1)
predict social_network_damage, score 


* Normalize to 0-100 scale
su social_network_damage, meanonly
gen social_network_damage_norm = (social_network_damage - r(min)) / (r(max) - r(min))
replace social_network_damage_norm = social_network_damage_norm * 100
lab var social_network_damage_norm "Network damage (0-100)"

* Median split
egen median_loss = median(social_network_damage_norm) if year == 2016 & returner == 1
gen high_harm = (social_network_damage_norm >= median_loss) if year == 2016 & returner == 1
lab var high_harm "Shared loss (>median)"
lab def harmie 0 "Below (n=223)" 1 "Above (n=227)", replace
lab val high_harm harmie

* Tertile split
xtile n_shared = social_network_damage_norm, nq(100)
gen shared_loss_cat = 1 if n_shared <= 25
replace shared_loss_cat = 2 if n_shared > 25 & n_shared < 75
replace shared_loss_cat = 3 if n_shared >= 75
replace shared_loss_cat = . if social_network_damage_norm == .
lab def cats_new 1 "Bottom 25%" 2 "Middle 50%" 3 "Top 25%", replace
lab val shared_loss_cat cats_new


* --- (2) Extended social interactions (help received) ---

alpha h6friend h6neigh h6natgov h6locgov h6coun h6natngo h6intngo h6church
* alpha = 0.91

* PCA with two components: volume and source of support
pca h6friend h6neigh h6natgov h6locgov h6coun h6natngo h6intngo h6church, comp(2)
* First component eigenvalue = 4.98, explains 62% of variation
predict help_volume help_source, score

* Normalize to 0-100
su help_volume, meanonly
gen help_volume_norm = (help_volume - r(min)) / (r(max) - r(min))
replace help_volume_norm = help_volume_norm * 100
lab var help_volume_norm "Support volume (0-100)"

su help_source, meanonly
gen help_source_norm = (help_source - r(min)) / (r(max) - r(min))
replace help_source_norm = help_source_norm * 100
lab var help_source_norm "Support source (0-100)"

* Median splits
egen median_interactions = median(help_volume_norm) if year == 2016 & returner == 1
gen high_help = (help_volume_norm >= median_interactions) if year == 2016 & returner == 1
lab var high_help "High volume of support (>median)"

egen median_source = median(help_source) if year == 2016 & returner == 1
gen high_source = (help_source >= median_source) if year == 2016 & returner == 1
lab var high_source "High source score, i.e. internal help (>median)"

* Tertile split
xtile n_help = help_volume_norm, nq(100)
gen help_cat = 1 if n_help <= 25
replace help_cat = 2 if n_help > 25 & n_help < 75
replace help_cat = 3 if n_help >= 75
replace help_cat = . if help_volume_norm == .
lab val help_cat cats_new


* --- (3) Perception of external disaster relief aid ---

* Reliability checks
alpha h7dis h7amo h7org h11promi h11rot h11wom, reverse(h11promi h11rot h11wom)
* alpha = 0.70
alpha h7dis h7amo h7org
alpha h11promi h11rot h11wom

* PCA: satisfaction vs. mismanagement of aid
pca h7dis h7amo h7org h11promi h11rot h11wom, comp(2)
* Eigenvalue = 2.43, explains 41% of variation
predict satisfaction_aid mismanagement_aid, score

* Normalize to 0-100
su satisfaction_aid, meanonly
gen satisfaction_aid_norm = (satisfaction_aid - r(min)) / (r(max) - r(min))
replace satisfaction_aid_norm = satisfaction_aid_norm * 100
lab var satisfaction_aid_norm "Aid satisfaction (0-100)"

su mismanagement_aid, meanonly
gen mismanagement_aid_norm = (mismanagement_aid - r(min)) / (r(max) - r(min))
replace mismanagement_aid_norm = mismanagement_aid_norm * 100
lab var mismanagement_aid_norm "Aid mismanagement (0-100)"

* Median splits
egen median_external = median(satisfaction_aid) if year == 2016 & returner == 1
gen high_external = (satisfaction_aid >= median_external) if year == 2016 & returner == 1
lab var high_external "High satisfaction with external aid (>median)"

egen median_mismanage = median(mismanagement_aid) if year == 2016 & returner == 1
gen high_mismanage = (mismanagement_aid >= median_mismanage) if year == 2016 & returner == 1
lab var high_mismanage "High perception of mismanagement of external aid (>median)"


* --- (4) Corruption in external relief resources ---

alpha h11rela h11soon h11vuln h11pay h11enti h11eno h11corr h11self h11extra
* alpha = 0.87

lab var h11rela "People with good relations received more"
lab var h11corr "People took at expense of others"
lab var h11self "Providers kept goods for themselves"

pca h11rela h11soon h11vuln h11pay h11enti h11eno h11corr h11self h11extra, comp(1)
* Eigenvalue = 4.38, explains 49% of variation
predict corruption_aid, score

* Normalize to 0-100
su corruption_aid, meanonly
gen corruption_aid_norm = (corruption_aid - r(min)) / (r(max) - r(min))
replace corruption_aid_norm = corruption_aid_norm * 100
lab var corruption_aid_norm "Aid corruption (0-100)"

* Median split
egen median_corrupt = median(corruption_aid_norm) if year == 2016 & returner == 1
gen high_corrupt = (corruption_aid_norm >= median_corrupt) if year == 2016 & returner == 1
lab var high_corrupt "High aid corruption (>median)"
lab def corrie 0 "Below (n=223)" 1 "Above (n=227)", replace
lab val high_corrupt corrie

* Tertile split
xtile n_corrupt = corruption_aid_norm, nq(100)
gen corrupt_cat = 1 if n_corrupt <= 25
replace corrupt_cat = 2 if n_corrupt > 25 & n_corrupt < 75
replace corrupt_cat = 3 if n_corrupt >= 75
replace corrupt_cat = . if corruption_aid_norm == .
lab val corrupt_cat cats_new


* --- (5) Aid distribution disappointment ---

* Rename desired and actual aid distribution variables
rename (h9exp h9egal h9egalexp h9vuln h9prep h9norem h9first h9none h9oth) ///
       (desired1 desired2 desired3 desired4 desired5 desired6 desired7 desired8 desired9)

rename (h10exp h10egal h10egalexp h10vuln h10prep h10norem h10first h10none h10oth) ///
       (actual1 actual2 actual3 actual4 actual5 actual6 actual7 actual8 actual9)

* Reshape to compute gap between desired and actual rank for each principle
frame copy default modified, replace
frame modified {
    drop if year != 2016

    reshape long desired actual, i(panel_id) j(desiredNUM)

    by panel_id (desired), sort: gen rank_desired = _n if desired != .
    by panel_id (actual), sort: gen rank_actual = _n if actual != .
    gen diff_desired_actual = desired - actual
    drop desired actual rank_actual rank_desired

    reshape wide diff_desired_actual, i(panel_id) j(desiredNUM)
}

* Merge back differences
frlink m:1 panel_id, frame(modified)
frget diff_desired_actual*, from(modified)
drop modified
frame drop modified

* Set differences to missing for non-2016 waves
foreach x of varlist diff_desired_actual1-diff_desired_actual9 {
    replace `x' = . if year != 2016
}

* Label distribution principles
lab var diff_desired_actual1 "According to people's exposure. The higher the suffering or damage, the higher the relief."
lab var diff_desired_actual2 "Egalitarian. This means all people from the Barangay receive the same amount of aid, regardless their losses."
lab var diff_desired_actual3 "Egalitarian towards exposed. This means all people that lost something because of the typhoon receive the same amount of aid, regardless if some suffered more or less. People that are not affected receive nothing."
lab var diff_desired_actual4 "Vulnerable Groups. Priority on people that had a difficult life before the typhoon already."
lab var diff_desired_actual5 "People who prepared. Priority on people that prepared more for the typhoon than others."
lab var diff_desired_actual6 "People who do not receive remittances."
lab var diff_desired_actual7 "First come first serve."
lab var diff_desired_actual8 "No-one received aid."
lab var diff_desired_actual9 "Other"

* Construct disappointment index based on rank difference of most-preferred principle
gen rank1_disappointment = diff_desired_actual1 if desired1 == 1
replace rank1_disappointment = diff_desired_actual2 if desired2 == 1
replace rank1_disappointment = diff_desired_actual3 if desired3 == 1
replace rank1_disappointment = diff_desired_actual4 if desired4 == 1
replace rank1_disappointment = diff_desired_actual5 if desired5 == 1
replace rank1_disappointment = diff_desired_actual6 if desired6 == 1
replace rank1_disappointment = diff_desired_actual7 if desired7 == 1
replace rank1_disappointment = diff_desired_actual8 if desired8 == 1
replace rank1_disappointment = diff_desired_actual9 if desired9 == 1
replace rank1_disappointment = rank1_disappointment * -1

lab var rank1_disappointment "Difference in rank between most desired and actual aid implementation (0-8)"

* Normalize to 0-100
su rank1_disappointment, meanonly
gen rank1_norm = (rank1_disappointment - r(min)) / (r(max) - r(min))
replace rank1_norm = rank1_norm * 100
lab var rank1_norm "Aid Distribution Preference Gap (0-100)"

* Median split
egen median_rank1 = median(rank1_disappointment) if returner == 1 & year == 2016
gen high_rank1 = (rank1_disappointment > median_rank1) if returner == 1 & year == 2016
lab var high_rank1 "Above median Aid Distribution Preference Gap"

* Tertile split
xtile n_rank1 = rank1_norm, nq(100)
gen rank1_cat = 1 if n_rank1 <= 25
replace rank1_cat = 2 if n_rank1 > 25 & n_rank1 < 75
replace rank1_cat = 3 if n_rank1 >= 75
replace rank1_cat = . if rank1_norm == .
lab val rank1_cat cats_new

* Village-level averages of distribution preferences
egen mean_exposure_actual = mean(actual1) if year == 2016, by(session)
egen mean_exposure_desired = mean(desired1) if year == 2016, by(session)
egen mean_egalitarian_desired = mean(desired3) if year == 2016, by(session)
egen mean_adapted_desired = mean(desired5) if year == 2016, by(session)


* --- (6) Within-village inequality changes due to Haiyan ---

* Normalize wealth index for Gini calculation
qui summ wealth_index
gen wealth_norm = (wealth_index - r(min)) / (r(max) - r(min))

* Compute Gini coefficient by village-year
egen subgroup = group(session year)
levels subgroup, local(levels)
gen gini = .
foreach i of local levels {
    ineqdec0 wealth_norm if subgroup == `i'
    replace gini = $S_gini if subgroup == `i'
}

* Change in Gini (2016 - 2012)
sort panel_id year
gen gini16_12 = gini - l4.gini if year < 2022
lab var gini16_12 "Positive values indicate that income inequality decreased after Haiyan (2016-2012)"

gen gini_increased = (gini16_12 >= 0) if returner == 1
replace gini_increased = . if year != 2016

* Normalize to 0-100
su gini16_12, meanonly
gen gini_norm = (gini16_12 - r(min)) / (r(max) - r(min))
replace gini_norm = gini_norm * 100
lab var gini_norm "Change in Gini (0-100)"

* Median split
egen median_gini16_12 = median(gini16_12) if returner == 1 & year == 2016
gen low_ineq = (gini16_12 < median_gini16_12) if returner == 1 & year == 2016
lab var low_ineq "Above median reduction in wealth inequality (based on gini)."
lab def ini 0 "Below (n=240)" 1 "Above (n=210)", replace
lab val low_ineq ini

* Tertile split
xtile n_gini = gini_norm, nq(100)
gen gini_cat = 1 if n_gini <= 25
replace gini_cat = 2 if n_gini > 25 & n_gini < 75
replace gini_cat = 3 if n_gini >= 75
replace gini_cat = . if gini_norm == .
lab val gini_cat cats_new

* Individual wealth rank changes within village
egen wealth_rank_12 = rank(wealth_index) if year == 2012, by(session)
replace wealth_rank_12 = wealth_rank_12[_n-1] if wealth_rank_12 == .

egen wealth_rank_16 = rank(wealth_index) if year == 2016, by(session)
replace wealth_rank_16 = wealth_rank_16[_n+1] if wealth_rank_16 == .

gen d_wealth_rank16_12 = wealth_rank_16 - wealth_rank_12
replace d_wealth_rank16_12 = . if returner == 0

* Normalize to 0-100
su d_wealth_rank16_12, meanonly
gen wealth_rank_norm = (d_wealth_rank16_12 - r(min)) / (r(max) - r(min))
replace wealth_rank_norm = wealth_rank_norm * 100
lab var wealth_rank_norm "Change in village wealth rank (0-100)"

gen negative_wealth_rank = (d_wealth_rank16_12 < 0) if returner == 1
lab var negative_wealth_rank "Participant relatively worse off than other participants"


* --- (7) Conflict over scarce relief resources ---

su h7confl, meanonly
gen conflict_aid_norm = (h7confl - r(min)) / (r(max) - r(min))
replace conflict_aid_norm = conflict_aid_norm * 100
lab var conflict_aid_norm "Conflict over scarce aid (0-100)"

* Median split
egen median_conflict = median(conflict_aid_norm) if year == 2016 & returner == 1
gen high_conflict = (conflict_aid_norm >= median_conflict) if year == 2016 & returner == 1
lab var high_conflict "High conflict over aid (>median)"
lab def confie 0 "Below (n=199)" 1 "Above (n=251)", replace
lab val high_conflict confie

* Tertile split
xtile n_conflict = conflict_aid_norm, nq(100)
gen conflict_cat = 1 if n_conflict <= 25
replace conflict_cat = 2 if n_conflict > 25 & n_conflict < 75
replace conflict_cat = 3 if n_conflict >= 75
replace conflict_cat = . if conflict_aid_norm == .
lab val conflict_cat cats_new


/*------------------------------------------------------------------------------
16. 2022 FOLLOW-UP: MAJOR LIFE EVENTS AND TRAUMA
------------------------------------------------------------------------------*/

* Generate dummies for major life events mentioned
tab mle1_type, gen(major1_)
tab mle2_type, gen(major2_)
tab mle3_type, gen(major3_)

* Indicator if Yolanda (Haiyan) mentioned as major life event
egen yolanda = rowmax(major1_1 major2_1 major3_1)
egen natural_disaster = rowmax(major1_2 major2_2 major3_2)
egen birth = rowmax(major1_3 major2_3 major3_3)
egen marriage = rowmax(major1_4 major2_4 major3_4)
egen death = rowmax(major1_5 major2_5 major3_5)
egen divorce = rowmax(major1_6 major2_6 major3_6)
egen violence = rowmax(major1_7 major2_7 major3_7)
egen covid = rowmax(major1_8 major2_8 major3_8)
egen other = rowmax(major1_9 major2_9 major3_9)
egen none = rowmax(major1_10)

* Village-level share mentioning Yolanda
egen share_yolanda_2022 = mean(yolanda) if year == 2022, by(village)

* Stressful life events
tab sle1_type, gen(stressful1_)
tab sle2_type, gen(stressful2_)
tab sle3_type, gen(stressful3_)

egen yolanda2 = rowmax(stressful1_1 stressful2_1 stressful3_1)
egen natural_disaster2 = rowmax(stressful1_2 stressful2_2 stressful3_2)
egen death2 = rowmax(stressful1_5 stressful2_5 stressful3_3)
egen divorce2 = rowmax(stressful1_6 stressful2_6 stressful3_4)
egen covid2 = rowmax(stressful1_8 stressful2_8 stressful3_5)
egen other2 = rowmax(stressful1_9 stressful2_9 stressful3_6)
egen none2 = rowmax(stressful1_10)

egen haiyan_stressful = rowmax(yolanda2 natural_disaster2) if year==2022
replace haiyan_stressful = 0 if year==2022 & haiyan_stressful==.
tab haiyan_stressful if particip_12_16_22==1
egen share_haiyan_stressful_2022 = mean(yolanda2) if year == 2022, by(village)
egen share_yolanda2_2022 = mean(yolanda2) if year == 2022, by(village)


* Yolanda trauma scale
alpha yolanda_trauma1 yolanda_trauma2 yolanda_trauma3 yolanda_trauma4 ///
      yolanda_trauma5 yolanda_trauma6 yolanda_trauma7, gen(yolanda_trauma)
egen average_yolanda_trauma = mean(yolanda_trauma) if year == 2022, by(village)

global vars2022 average_yolanda_trauma yolanda_rec_econ yolanda_rec_emot yolanda yolanda2 haiyan_stressful
foreach x of varlist  $vars2022 {
    replace `x'=`x'[_n+2] if year==2012
	replace `x'=`x'[_n+1] if year==2016
}
xtile aaq2_quart = average_yolanda_trauma, nq(4)
gen high_aaq2 = 1 if aaq2_quart==4
replace high_aaq2 = 0 if aaq2_quart < 4


egen z_recip = std(recip_pos)
lab var z_recip "Reciprocity (SD)"
gen d22_a3gov= a3gov-l6.a3gov
gen d22_wealth= wealth_index-l6.wealth_index

/*------------------------------------------------------------------------------
17. INVERSE PROBABILITY WEIGHTS
------------------------------------------------------------------------------*/
global balance1 mean_transfer exp_transfer distance_storm_km_reversed  a3gov wealth_index age gender hh_head single hh_size edu_1
* IPW for attrition (probability of returning in 2016)
probit returner $balance1 if year == 2012, vce(cluster session)
predict p_return
gen ipw_return = 1 / p_return if returner == 1
replace ipw_return = 1 / (1 - p_return) if returner == 0

* IPW for selection into experiment (anchor vs. random stranger)
gen anchor = (type < 4) if type != .
probit anchor $balance1 if year == 2012, vce(cluster session)
predict p_anchor, pr
gen ipw_anchor = 1 / p_anchor if anchor == 1
replace ipw_anchor = 1 / (1 - p_anchor) if anchor == 0


/*------------------------------------------------------------------------------
18. ADDITIONAL SOCIODEMOGRAPHIC VARIABLES
------------------------------------------------------------------------------*/

* Recode single status
drop single
gen single = (status == 1)

* Variable labels
lab var age "Age (years)"
lab var gender "Female (=1)"
lab var hh_head "Household head (=1)"
lab var single "Single (=1)"
lab var hh_size "Household size"
lab var edu_1 "Education: Elementary (=1)"
lab var d_exp_transfer "Change in expected transfer"
lab var d_a3gov "Change in trust national government"
lab var d_a1trust "Change in generalized trust"
lab var d_a1opti "Change in general optimism"
lab var d_wealth "Change in wealth index (PCA)"

* Value labels for disaster effects
lab def effect_soli 1 "Care more about myself" 2 "Care more about others" 3 "No effect", replace
lab val le_disaster_solidarity effect_soli

lab def effect_worry 1 "More worried" 2 "Less worried" 3 "No effect", replace
lab val le_disaster_worries effect_worry



* Save analysis-ready dataset
save "$working_ANALYSIS/processed/analysis_rdy.dta", replace




* End of file
