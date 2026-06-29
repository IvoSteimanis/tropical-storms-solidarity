/*==============================================================================
Project:     Moderate disaster exposure divides communities; severe exposure does not
File:        03_analysis_SI.do
Purpose:     Reproduce all results reported in the SI
Input:       analysis_rdy.dta, litfe_u_shape.dta
Output:      Figs. S1-S10, Tables S1-S33
Authors:     Steimanis
Date:        Created: 2024 | Last modified: March 2026


CONTENTS:

  S1. LITERATURE REVIEW AND CONCEPTUAL FRAMEWORK (~80)
    ~82:    Fig. S1     Mechanisms (created in PowerPoint)
    ~86:    Fig. S2     Forest plot

  S2. ADDITIONAL METHODS (~200)
    ~206:   Table S1    Summary statistics (survey studies; created in Excel)
    ~224:   Table S2    Summary by attrition status
    ~230:   Fig. S3     Differential attrition
    ~247:   Table S3    Determinants of attrition
    ~262:   Table S4    Lee (2009) bounds for attrition
    ~285:   Table S5    Out-migration (descriptive; created in Excel)
    ~289:   Table S6    Pseudo-treatment (placebo) effects
    ~310:   Table S7    PCA details: post-disaster experience
    ~345:   Table S8    PCA loadings: post-disaster experience
    ~356:   Table S9    PCA: Perceived Aid Corruption
    ~383:   Table S10   PCA loadings: Aid Corruption
    ~389:   Table S11   PCA: Post-Disaster Social Support
    ~417:   Table S12   PCA loadings: Social Support
    ~423:   Table S13   PCA: Social Network Damage
    ~450:   Table S14   PCA loadings: Network Damage
    ~456:   Fig. S4     Distributions of PCA variables
    ~529:   Fig. S5     Baseline trends by post-Haiyan experience
    ~540:   Fig. S6     Distribution of solidarity transfers

  S3. ADDITIONAL ANALYSIS AND ROBUSTNESS CHECKS (~560)
    ~567:   Table S15   Main effects: U-shaped impact on transfers
    ~634:   Table S16   Quadratic model (full regression)
    ~638:   Table S17   Wind-speed categories (full table)
    ~642:   Table S18   Robustness: post-treatment controls
    ~705:   Table S19   Solidarity response by vulnerability group
    ~758:   Table S19b  Wealth-tertile robustness (S2.X.4)
    ~795:   Table S19c  PCA-tertile robustness (S2.X.2)
    ~835:   Fig. S7     Long-term effects: 2022 evidence
    ~771:   Table S20   Long-term (2022): Reciprocity & stressful event
    ~801:   Fig. S8     Lab U-shape + diffusion of responsibility
    ~854:   Table S21   Lab-in-the-field: Diffusion regressions
    ~906:   Table S22   Role of aid satisfaction
    ~936:   Fig. S9     Post-disaster perceptions (6-panel)
    ~1008:  Table S23   Perceptions regression table
    ~1014:  Table S24   Controlling for post-Haiyan (individual)
    ~1068:  Table S25   Controlling for post-Haiyan (village level)
    ~1131:  Table S26   Anchor vs. friend transfers
    ~1191:  Table S27   Without baseline & full 2016 sample
    ~1202:  Table S28   Excluding medium-damage villages
    ~1212:  Table S29   Affectedness index instead of distance
    ~1221:  Table S30   Damage levels (using affectedness index)
    ~1231:  Table S31   Tobit model for censored transfers
    ~1240:  Fig. S10    Sensitivity contour plots
    ~1320:  Table S33   Effect on solidarity expectations
    ~1365:  Table S32   Multiple testing adjustments (Benjamini-Hochberg; runs last)
==============================================================================*/
*Load dataset
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
lab var windspeed_predicted "Wind speed (kn)"
lab var windspeed_sqr "Wind speed squared"

* select balanced sample
drop if returner== 0
sort panel_id year

*Setup for regression analysis
global x_changed                                               // Primary spec: no post-treatment controls (d_a3gov d_wealth moved to robustness)
global imbalances age gender hh_head single edu_1 stranger     // Time-invariant controls (attrition/selection)
global imbalances2 age gender hh_head single high_educ stranger // Alternative education specification





*-----------------------------------------------------------
** Section S1.	Literature review and conceptual framework
*-----------------------------------------------------------
*Fig. S1.	Mechanisms and theory of change
* --> created in powerpoint


*Fig. S2.	Experimental evidence of the effects of environmental hazards on prosociality
preserve
import excel "$working_ANALYSIS/data/!Lit_RQ2.xlsx", sheet("All studies") cellrange(A3:BD50) case(lower) firstrow clear
destring use, replace
keep if use == 1 
keep id authors journal country countryshort disaster disasterabbr timepassed game incentivized measure maxsend unit diff settest lci95ttest uci95ttest coefficient se lci95 uci95 diff lci95 uci95 sddependentv sdindependentv samplesize samplesizeused
order id authors journal country countryshort disaster disasterabbr timepassed game incentivized measure maxsend unit diff settest lci95ttest uci95ttest coefficient se lci95 uci95 diff lci95 uci95 sddependentv sdindependentv samplesize samplesizeused
encode authors, gen(paper)
destring *, replace

/* Transform absolute amounts to share of endowment
Some studies report effect sizes in absolute monetary units (e.g., PHP, CLP)
rather than as shares of the endowment. To make all effects comparable, we
divide by the maximum sendable amount (maxsend) for those studies.

The condition identifies studies needing rescaling automatically:
  - game type        : only Dictator, Trust, and Public Good games (not
                       Solidarity Games, which use a different payout structure,
                       or survey-based measures)
  - maxsend != .     : only studies with a defined endowment
  - unit != "Share*" : only studies NOT already reporting in share terms

This currently applies to: Cassar et al. (ID 11, Trust Game, 6 CLP notes),
Afzal et al. (ID 25, Public Good Game, 100 units), and Kuroishi & Sawada
(ID 39, Dictator Game, 1000 PHP). Studies already in shares (e.g., ID 9, 48),
this paper's solidarity game estimates (ID 0), and survey-based studies are
unaffected.
*/
foreach var in coefficient se lci95 uci95 diff lci95ttest uci95ttest settest {
	replace `var' = `var' / maxsend if inlist(game, "Dictator Game", "Trust Game", "Public Good Game", "Ultimatum Game") & maxsend != . & !regexm(lower(unit), "share")
}
replace se = 0.0001 if id == 59 // SE needs to be positive; however reported as 0 in study
replace samplesize = samplesizeused if samplesizeused != .

// Add diff of ttest for Cassar et al. & Veszteg (only for t-test CIs reported)
replace coefficient = diff if id == 7 | id == 50
replace lci95 = lci95ttest if id == 7 | id == 50
replace uci95 = uci95ttest if id == 7 | id == 50
replace se = settest if id == 7 | id == 50 // t-tests used


// Generate abbreviation for game type
gen game_abb = ""
replace game_abb = "DG" if game == "Dictator Game"
replace game_abb = "DG (H)" if game == "Dictator Game" & id == 9
replace game_abb = "TG:S" if game == "Trust Game" & measure == "Trust"
replace game_abb = "TG:R" if game == "Trust Game" & measure == "Trustworthiness" 
replace game_abb = "TG:S (H)" if game == "Trust Game" & measure == "Trust" & id == 50 // Veszteg et al 
replace game_abb = "TG:R (H)" if game == "Trust Game" & measure == "Trustworthiness"  & id == 50 // Veszteg et al 
replace game_abb = "UG" if game == "Ultimatum Game"
replace game_abb = "PGG" if game == "Public Good Game"
replace game_abb = "SG" if game == "Solidarity Game"

// Generate variable for Game vs. Survey
gen type = ""
replace type = "Survey" if game == "Survey"
replace type = "Game" if game != "Survey"

gen type2 = ""
replace type2 = "Survey" if game == "Survey"
replace type2 = "Incentivized Game" if game != "Survey" & incentivized == 1
replace type2 = "Hypothetical Game" if game != "Survey" & incentivized == 0



// Define dataset as meta dataset
meta set coefficient se, studylabel(authors) studysize(samplesize)

// Adjust CIs for Vardy and Atkinson (to fit as reported in paper)
replace _meta_cil = -0.61 if id == 48
replace _meta_ciu =  0.08 if id == 48

// Generate variables containing Coefficient, LCI & UCI
foreach var in coefficient _meta_cil _meta_ciu {
	tostring `var', generate(`var'_str) force format(%5.2f)
}

gen eff_cis=coefficient_str + " [" + _meta_cil_str + ", " + _meta_ciu_str + "]" if coefficient < 0 & _meta_cil < 0 & _meta_ciu < 0
replace eff_cis=coefficient_str + " [" + _meta_cil_str + ",  " + _meta_ciu_str + "]" if coefficient < 0 & _meta_cil < 0 & _meta_ciu > 0
replace eff_cis= "  " + coefficient_str + " [" + _meta_cil_str + ",  " + _meta_ciu_str + "]" if coefficient > 0 & _meta_cil < 0 & _meta_ciu > 0
replace eff_cis= "  " + coefficient_str + " [ " + _meta_cil_str + ",  " + _meta_ciu_str + "]" if coefficient > 0 & _meta_cil > 0 & _meta_ciu > 0
lab var eff_cis "Effect size with 95-CI"


gen fp_groups= 0 if game == "Solidarity Game"
replace fp_groups = 1 if game== "Dictator Game" | game == "Ultimatum Game" | game == "Public Good Game" 		
replace fp_groups = 2 if game== "Trust Game"
lab def trusti 0 "This paper: Solidarity" 1 "Fairness, Altruism" 2 "Trust*worthiness" , replace
lab val fp_groups trusti
// All-in-one

meta forestplot _id _plot eff_cis countryshort disasterabbr timepassed samplesize game_abb if type == "Game" , subgroup(fp_groups)  ///
sort(coefficient, ascending) nonotes nullrefline ciopts(lwidth(0.3 1)  msize() recast(rcap)) ///
noosigtest noohomtest nogwhomtests nogbhomtests noghetstats ///
nometashow noohetstats nooverall noomarker nogmarker nowmarkers /// leave away calculation of overall effects
columnopts(eff_cis, title("Effect size" "with 95 CI")) ///
columnopts(countryshort, title("Country")) ///
columnopts(disasterabbr, title("DIS")) ///
columnopts(samplesize, title("N")) ///
columnopts(game_abb, title("EXP")) ///
columnopts(timepassed, title("{&Delta}Months")) ///
xtitle("Effect of exposure on share sent (in % of endowment)") ///
markeropts(msize(medium)) ///
xla(-.4 "-40%" -.2 "-20%" 0 "0%" .2 "20%") crop(-.4 .4) // Cutting the CI-Intervals; Otherwise CIs of Fleming (2014) to far

// adjust col
gr_edit .plotregion1.column4.items[9].xoffset = 0.7
gr_edit .plotregion1.column4.items[10].xoffset = 0.7
gr_edit .plotregion1.column4.items[11].xoffset = 0.7
gr_edit .plotregion1.column4.items[12].xoffset = 1
gr_edit .plotregion1.column4.items[13].xoffset = 1
gr_edit .plotregion1.column4.items[14].xoffset = 1
gr_edit .plotregion1.column4.items[15].xoffset = 1
gr_edit .plotregion1.column4.items[16].xoffset = 1
gr_edit .plotregion1.column4.items[17].xoffset = 1
gr save  "$working_ANALYSIS/results/intermediate/figureS2_forestplot_literature.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS2_forestplot_literature.png", replace width(3165)
restore


*Table S1.	Natural hazards and prosociality across survey studies
*created in excel




*----------------------------------
** Section S2.	Additional Methods
*----------------------------------
*Overview all data collections (unbalanced)
preserve
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
lab var windspeed_predicted "Wind speed (kn)"
lab var windspeed_sqr "Wind speed squared"

global summary mean_transfer exp_transfer windspeed_predicted a3gov wealth_index age gender hh_head single hh_size edu_1

estpost tabstat $summary, by(year) statistics(count mean sd) columns(statistics)
esttab . using "$working_ANALYSIS/results/tables/tableS1_summary.tex", cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress not nostar unstack nomtitle nonumber nonote booktabs replace label

* Table S2.	Summary statistics by attrition status (2012)
global balance1 mean_transfer exp_transfer windspeed_predicted  a3gov wealth_index age gender hh_head single hh_size edu_1
iebaltab $balance1 if year==2012, vce(cluster session) grpvar(returner) ftest fmissok rowvarlabels format(%9.2f) savetex("$working_ANALYSIS/results/tables/tableS2_selective_participation") replace
eststo attrition: reg returner $balance1 if year==2012, cluster(session)


* Fig. S3.	Differences in returning rates
*over calibrated wind speed
replace returner=returner*100
egen share_returners = mean(returner) if year==2012, by(session)
lpoly share_returners windspeed_predicted, xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) lineopts(lwidth(medthick)) msize(medlarge) mcolor(*.5) legend(off) title("{bf:a} Continuous wind speed") xtitle("Sustained wind speed (knots)") ytitle("Share") bwidth(6)  xla(70(20)150) yla(0(20)100) note("") scale(1.2) xsize(3.165) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/attrition_windspeed.gph", replace

*over wind-speed categories
cibar returner if year==2012, over1(ws_cat3)  bargap(50) gap(10) barlabel(on) blpos(11) blgap(0.01) blfmt(%9.1f) graphopts(title("{bf:b} Wind-speed categories")  xsize(3.165) ysize(2) yla(0(20)100,  nogrid) xla(,nogrid) legend(ring(1) pos(6) rows(3) size(8pt))  ytitle(Share of participants returned in 2016, size(6pt))  scale(1.2)  xtitle(, size(6pt))) ciopts(lpattern(dash) lcolor(black))
gr save "$working_ANALYSIS/results/intermediate/attrition_distance_levels.gph", replace

gr combine "$working_ANALYSIS/results/intermediate/attrition_windspeed.gph" "$working_ANALYSIS/results/intermediate/attrition_distance_levels.gph", rows(1) scale(1.2)
gr save "$working_ANALYSIS/results/intermediate/figS3_differential_attrition.gph", replace
gr export "$working_ANALYSIS/results/figures/figS3_differential_attrition.png", replace width(4000)

ttest returner if ws_cat3!=3 & year==2012, by(ws_cat3)

*Table S3.	Determinants of attrition
global balance2 mean_transfer exp_transfer a3gov wealth_index age gender hh_head single hh_size edu_1
eststo attrition: reg returner i.ws_cat3 $balance2 if year==2012, cluster(session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo attrition2: reg returner windspeed_predicted $balance2 if year==2012, cluster(session)

esttab attrition attrition2 using "$working_ANALYSIS/results/tables/tableS3_determinants_of_attrition.tex",  keep(2.ws_cat3 3.ws_cat3 windspeed_predicted $balance2)  label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Returner(=1)", pattern(1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomtitles b(%4.2f) stats(N N_clust r2_a Fstat , labels("N" "Cluster" "Adjusted R-squared" "F-Test: medium exposure = high exposure") fmt(%9.0fc %9.0fc %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


** Table S4.	Lee (2009) bounds for treatment effects under differential attrition
* Bounds on treatment effect when attrition may differ across treatment groups
* Treatment: wind-speed category (medium vs low, high vs low)
* Outcome: mean_transfer in 2016
* Selection indicator: returner (observed in both 2012 and 2016 waves)
gen returner2 = 1 if returner==100
replace returner2= 0 if returner==0
* Comparison 1: Medium exposure (Cat 3: 96-112 kn) vs. Low exposure (<96 kn)
gen treat_med_vs_low = (ws_cat3 == 2) if ws_cat3 == 1 | ws_cat3 == 2
eststo lee_med: leebounds mean_transfer treat_med_vs_low if year == 2016 & treat_med_vs_low != ., ///
    select(returner2) vce(analytic) cieffect

* Comparison 2: High exposure (>=113 kn) vs. Low exposure (<96 kn)
gen treat_high_vs_low = (ws_cat3 == 3) if ws_cat3 == 1 | ws_cat3 == 3
eststo lee_high: leebounds mean_transfer treat_high_vs_low if year == 2016 & treat_high_vs_low != ., ///
    select(returner2) vce(analytic) cieffect

drop treat_med_vs_low treat_high_vs_low

esttab lee_med lee_high using "$working_ANALYSIS/results/tables/tableS4_lee_bounds.tex", ///
    label b(%4.2f) se(%4.2f) ///
    mtitles("Medium vs. Low exposure" "High vs. Low exposure") ///
    stats(N, labels("N") fmt(%9.0fc)) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    varlabels(lower "Lower bound" upper "Upper bound") ///
    eqlabels("Solidarity transfers") ///
    nonotes ///
    booktabs replace


restore



**Table S5.	Out-migration based on official documents
*excel based on official data


** Table S6.	Pseudo-treatment effects of Haiyan
* Placebo mirrors the quadratic main specification (3 columns, as in the pre-conversion SI);
* no categorical placebo existed pre-conversion, so none is added here.
eststo tableS6_1:reg mean_transfer windspeed_predicted windspeed_sqr if year==2012 & returner==1, cluster(session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
eststo tableS6_2:reg mean_transfer windspeed_predicted windspeed_sqr exp_transfer if year==2012 & returner==1, cluster(session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
eststo tableS6_3:reg mean_transfer windspeed_predicted windspeed_sqr exp_transfer age gender hh_head single edu_1 a3gov wealth_index if year==2012 & returner==1, cluster(session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab tableS6_1 tableS6_2 tableS6_3  using "$working_ANALYSIS/results/tables/tableS6_pseudo_treatment.tex", order(windspeed_predicted windspeed_sqr exp_transfer age gender hh_head single edu_1 a3gov wealth_index) label se(%4.2f) transform(ln*: exp(@) exp(@)) mgroups("Average transfers 2012", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  nomtitles b(%4.2f) stats(N N_clust r2_a Fstat, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²") fmt(%9.0fc %9.0fc %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace



**Table S7.	Post-disaster experience PCA
pca h7dis h7amo h7org h11promi h11rot h11wom, comp(2)

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"

// I don't know a neat way of doing a cumulative sum
matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
matrix list t

estadd matrix table = t

esttab ., ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nogap noobs nonumber nomtitle
esttab . using "$working_ANALYSIS/results/tables/tableS7_PCA_details.tex", booktabs replace ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nonotes nogap noobs nonumber nomtitle

	
	
**Table S8.	Loadings of first component and unexplained variation of each item	
esttab ., ///
    cells("L[Comp1](t fmt(2)) L[Comp2](t fmt(2)) L[Comp3](t fmt(2)) Psi[Unexplained]" ) ///
   nogap noobs nonumber nomtitle
esttab . using "$working_ANALYSIS/results/tables/tableS8_PCA_details2.tex", booktabs replace ///
    cells("L[Comp1](t fmt(2)) L[Comp2](t fmt(2)) L[Comp3](t fmt(2)) Psi[Unexplained]") ///
    nonotes nogap noobs nonumber nomtitle label
	
	


**Table S9.	Perceived Aid Corruption PCA
pca h11rela h11soon h11vuln h11pay h11enti h11eno h11corr h11self h11extra, comp(1)
matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"

matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
estadd matrix table = t

esttab . using "$working_ANALYSIS/results/tables/tableS9_PCA_corruption.tex", booktabs replace ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nonotes nogap noobs nonumber nomtitle


**Table S10.	Loadings: Perceived Aid Corruption
esttab . using "$working_ANALYSIS/results/tables/tableS10_PCA_corruption_loadings.tex", booktabs replace ///
    cells("L[Comp1](t fmt(2)) Psi[Unexplained]") ///
    nonotes nogap noobs nonumber nomtitle label


**Table S11.	Post-Disaster Social Support PCA
pca h6friend h6neigh h6natgov h6locgov h6coun h6natngo h6intngo h6church, comp(2)

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"

matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
estadd matrix table = t

esttab . using "$working_ANALYSIS/results/tables/tableS11_PCA_social_support.tex", booktabs replace ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nonotes nogap noobs nonumber nomtitle


**Table S12.	Loadings: Post-Disaster Social Support
esttab . using "$working_ANALYSIS/results/tables/tableS12_PCA_social_support_loadings.tex", booktabs replace ///
    cells("L[Comp1](t fmt(2)) L[Comp2](t fmt(2)) Psi[Unexplained]") ///
    nonotes nogap noobs nonumber nomtitle label


**Table S13.	Social Network Damage PCA
pca h3neigh h3friend h3fam h3friendoth h3famoth, comp(1)
matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"

matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
estadd matrix table = t

esttab . using "$working_ANALYSIS/results/tables/tableS13_PCA_network_damage.tex", booktabs replace ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nonotes nogap noobs nonumber nomtitle


**Table S14.	Loadings: Social Network Damage
esttab . using "$working_ANALYSIS/results/tables/tableS14_PCA_network_damage_loadings.tex", booktabs replace ///
    cells("L[Comp1](t fmt(2)) Psi[Unexplained]") ///
    nonotes nogap noobs nonumber nomtitle label


** Fig. S4.	Distributions of Key Experiential and Perceptual Variables
* Panel A: Perceived Aid Satisfaction
hist satisfaction_aid_norm, percent ///
    title("{bf:a} Perceived aid satisfaction", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_a.gph", replace

* Panel B: Perceived Aid Mismanagement
hist mismanagement_aid_norm, percent ///
    title("{bf:b} Perceived aid mismanagement", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_b.gph", replace

* Panel C: Perceived Aid Corruption
hist corruption_aid_norm, percent ///
    title("{bf:c} Perceived aid corruption", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_c.gph", replace

* Panel D: Perceived Volume of Support
hist help_volume_norm, percent ///
    title("{bf:d} Perceived volume of support", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_d.gph", replace

* Panel E: Perceived Source of Support
hist help_source_norm, percent ///
    title("{bf:e} Perceived source of support", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_e.gph", replace

* Panel F: Perceived Social Network Damage
hist social_network_damage_norm, percent ///
    title("{bf:f} Perceived social network damage", size(medium)) ///
    xtitle("Normalized score", size(small)) ytitle("Percent", size(small)) ///
    xla(0(20)100, labsize(small)) yla(, nogrid labsize(small)) ///
    lcolor(none) fcolor(bluishgray) ///
    xsize(2) ysize(2) graphregion(color(white))
gr save "$working_ANALYSIS/results/intermediate/figS4_f.gph", replace

* Combine all 6 panels
gr combine "$working_ANALYSIS/results/intermediate/figS4_a.gph" ///
    "$working_ANALYSIS/results/intermediate/figS4_b.gph" ///
    "$working_ANALYSIS/results/intermediate/figS4_c.gph" ///
    "$working_ANALYSIS/results/intermediate/figS4_d.gph" ///
    "$working_ANALYSIS/results/intermediate/figS4_e.gph" ///
    "$working_ANALYSIS/results/intermediate/figS4_f.gph", ///
    cols(3) xsize(4) ysize(3) graphregion(color(white))
gr save  "$working_ANALYSIS/results/intermediate/figS4_PCA_distributions.gph", replace
gr export "$working_ANALYSIS/results/figures/figS4_PCA_distributions.png", replace width(4000)




*----------------------------------------------------------
* Section S3.	Additional analysis and robustness checks
*----------------------------------------------------------

** Fig. S5.	Different baseline trends depending on experiences made after Haiyan
twoway (lpoly diff_baseline_to_mean windspeed_predicted if h6need==1, lwidth(thick) bwidth(6))  (lpoly diff_baseline_to_mean windspeed_predicted if high_external==1, lwidth(thick) lpattern(dash) bwidth(6)), ytitle("Difference in transfers to village average (PHP)") ///
		xtitle("Sustained wind speed (knots)")  ///
		yline(0, lpattern(solid))  ///
		xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) ///
		xlabel(70(20)150, nogrid) scale(1.2) xsize(3.465) ysize(2) yla(-5(1)5, nogrid) legend(rows(1) order(1 "Not in need of aid after Haiyan" 2 "Above median aid satisfaction"))
ttest baseline_mean if returner==1, by(h6need)
ttest baseline_mean if returner==1, by(high_external)
gr save  "$working_ANALYSIS/results/intermediate/figS5_baseline_trends.gph", replace
gr export "$working_ANALYSIS/results/figures/figS5_baseline_trends.png", replace width(3165)


*Fig. S6.	Distribution of solidarity transfers
bysort year: sum mean_transfer 
ttest mean_transfer, by(year)

*panel A
stripplot mean_transfer if year<2022 , over(year)  yla(0(10)70, nogrid) xtitle("") vertical center cumul cumprob  bar boffset(0) refline(lw(medium) lc(gs10) lp(dash))  reflinestretch(0.1) height(1.8) xla(, noticks) yla(, ang(h)) jitter(0.5) aspect(1.2) scale(1.2) title("{bf:a} Solidarity transfers")
gr save  "$working_ANALYSIS/results/intermediate/figure4_a.gph", replace

*panel B
hist d_mean_transfer, xline(-10 10, lpattern(dash)) barwidth(6) percent lcolor(none) yla(0(5)20, nogrid) xla(-70(10)70, nogrid) title("{bf:b} {&Delta} Transfers") xtitle("Transfer2016 - Transfer2012") ytitle("Percent")   aspect(1.2)  scale(1.2)
gr save  "$working_ANALYSIS/results/intermediate/figure4_b.gph", replace
sort panel_id year
reg mean_transfer l4.mean_transfer
pwcorr mean_transfer l4.mean_transfer, sig
tab d_mean_transfer if d_mean_transfer >=-10 & d_mean_transfer<=10
tab d_mean_transfer if d_mean_transfer <-10
tab d_mean_transfer if d_mean_transfer >10 
tab d_mean_transfer


gr combine "$working_ANALYSIS/results/intermediate/figure4_a.gph" "$working_ANALYSIS/results/intermediate/figure4_b.gph", rows(1) xsize(3.465) ysize(2)
gr_edit SetAspectRatio 0.8
gr save  "$working_ANALYSIS/results/intermediate/FigS6_delta_main_outcomes.gph", replace
gr export "$working_ANALYSIS/results/figures/FigS6_delta_main_outcomes.png", replace width(3165)



** Table S15.	Main effects: U-shaped impact on solidarity transfers
eststo table2_1: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer, vce(cluster session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
* Predicted profile: factor-notation twin so margins traces the quadratic (windspeed_sqr must move with windspeed)
quietly reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer, vce(cluster session)
margins, at(windspeed_predicted = (70(5)145))

eststo table2_2: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer, vce(cluster session)
test windspeed_predicted=windspeed_sqr=0 
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
* Predicted profile: factor-notation twin (see note above)
quietly reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer, vce(cluster session)
margins, at(windspeed_predicted = (70(5)145))

eststo table2_3: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
* Predicted profile: factor-notation twin (see note above)
quietly reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
margins, at(windspeed_predicted = (70(5)145))

eststo table2_4: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances [pweight=ipw_return], vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
* Predicted profile: factor-notation twin (see note above)
quietly reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances [pweight=ipw_return], vce(cluster session)
margins, at(windspeed_predicted = (70(5)145))

* Wind-speed categorical specification (ws_cat3: <96 / 96-113 / >=113 kn; built in 01_clean_data.do).
* The partition coincides exactly with the former distance categories on this sample (cross-tab
* diagonal: 14/2/14 villages), so estimates match the published dummy specification; only the
* exposure metric of the category labels changes.
eststo table2_5: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
margins, at(ws_cat3 = (1(1)3))

eststo table2_6: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
margins, at(ws_cat3 = (1(1)3))


eststo table2_7: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
margins, at(ws_cat3 = (1(1)3))

eststo table2_8: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances [pweight=ipw_return], vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))
margins, at(ws_cat3 = (1(1)3))


*effect size in SD
reg d_mean_transfer_std c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
margins, at(windspeed_predicted = (70(5)145)) 


esttab table2_1 table2_2 table2_3 table2_4 table2_5 table2_6 table2_7 table2_8 using "$working_ANALYSIS/results/tables/tableS15_main_effects_transfers.tex",  keep(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer $x_changed) order(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer $x_changed) label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0 0 0 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  nomtitles posthead("& \multicolumn{4}{c}{Quadratic specification} & \multicolumn{4}{c}{Dummy specification} \\" "\cmidrule(lr){2-5}\cmidrule(lr){6-9}" "\midrule") b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: medium damages = high damages") fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


** Table S16.	Quadratic model: Effect of Haiyan on solidarity transfers (full)
esttab table2_1 table2_2 table2_3 using "$working_ANALYSIS/results/tables/tableS16_main_effects_full.tex",    label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitles b(%4.2f) stats(N N_clust r2_a Fstat, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²") fmt(%9.0fc %9.0fc %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


** Table S17.	Solidarity Transfers: Wind-speed categories (full table)
esttab table2_5 table2_6 table2_7 using "$working_ANALYSIS/results/tables/tableS17_main_effects_full2.tex",    label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitles b(%4.2f) stats(N N_clust r2_a Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: medium = high damages") fmt(%9.0fc %9.0fc %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


** Table S18.	Robustness: Including post-treatment controls (Change in trust in government, Change in wealth)
* These variables are potentially affected by disaster exposure, so excluded from primary specification
* Shown here for comparison: results are qualitatively unchanged

eststo robust_pt1: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer d_a3gov d_wealth $imbalances, vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo robust_pt2: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer d_a3gov d_wealth $imbalances [pweight=ipw_return], vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo robust_pt3: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer d_a3gov d_wealth $imbalances, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo robust_pt4: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer d_a3gov d_wealth $imbalances [pweight=ipw_return], vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

esttab robust_pt1 robust_pt2 robust_pt3 robust_pt4 using "$working_ANALYSIS/results/tables/tableS18_robustness_post_treatment.tex",  keep(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer d_a3gov d_wealth) order(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer d_a3gov d_wealth) label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  mtitles("Quadratic" "Quadratic (IPW)" "Categorical" "Categorical (IPW)") b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: medium = high damages") fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


* ==============================================================================
* Economic-vulnerability index PCA detail tables (match the S7/S8 post-disaster
* PCA style): (1) components (eigenvalues/proportion/cumulative) and (2) PC1
* loadings, oriented so higher = more vulnerable. vuln_group is built from this
* same PCA in 01_clean_data.do.
* ==============================================================================
global coping hh_size child_under7 elderly_present adj_log_ymonth s_1000php debt_5000 meals
pca $coping if year==2012 & returner==1, comp(1)
* --- components table (eigenvalues) ---
matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"
matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"
matrix p = ev[1...,1] / e(trace)
matrix colnames p = "Proportion"
matrix c = J(e(trace),1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/`e(trace)' {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
}
matrix colnames c = "Cumulative"
matrix t = ( ev , d , p , c )
estadd matrix table = t
esttab . using "$working_ANALYSIS/results/tables/table_PCA_coping_components.tex", booktabs replace ///
    cells("table[Eigenvalue](t fmt(2)) table[Difference](t fmt(2)) table[Proportion](t fmt(2)) table[Cumulative](t fmt(2))") ///
    nonotes nogap noobs nonumber nomtitle
* --- PC1 loadings table (oriented so higher = more vulnerable) ---
matrix L   = e(L)
matrix Psi = e(Psi)
local sgn = cond(L[4,1] > 0, -1, 1)   // row 4 = adj_log_ymonth; orient so income loads negative
local labs `" "Household size" "Children under 7" "Elderly present" "Adjusted log income" "Savings (above 1{,}000 PHP)" "Debt (above 5{,}000 PHP)" "Food insecurity" "'
file open _lf using "$working_ANALYSIS/results/tables/table_PCA_coping_loadings.tex", write replace text
file write _lf "\begin{tabular}{lcc}" _n "\toprule" _n
file write _lf " & Comp1 & Unexplained \\" _n "\midrule" _n
forvalues i=1/`=rowsof(L)' {
    local lab : word `i' of `labs'
    local lo  = `sgn'*L[`i',1]
    local lof : di %5.2f `lo'
    local unf : di %5.2f Psi[1,`i']
    file write _lf "`lab' & `lof' & `unf' \\" _n
}
file write _lf "\bottomrule" _n "\end{tabular}" _n
file close _lf
capture copy "$working_ANALYSIS/results/tables/table_PCA_coping_components.tex" "$working_ANALYSIS/../submission/2026 - NCC/tables/table_PCA_coping_components.tex", replace
capture copy "$working_ANALYSIS/results/tables/table_PCA_coping_loadings.tex"   "$working_ANALYSIS/../submission/2026 - NCC/tables/table_PCA_coping_loadings.tex", replace

* ==============================================================================
* Table S20: Solidarity response by economic vulnerability (replaces the former
* S19/S19b/S19c pre-disaster vulnerability tables). vuln_group (1=least, 2=middle,
* 3=most) is built in 01_clean_data.do as tertiles of a baseline-2012 coping-PCA
* economic-vulnerability index (unsupervised, pre-treatment). This table reports the
* PRIMARY continuous wind-speed-by-vulnerability interaction (coefficients on wind,
* wind^2, the index, and the two interaction terms); Figure 2c plots the implied
* curves at -1/0/+1 SD of the index. Replaces the former tertile split table.
* ==============================================================================
eststo clear
eststo hetcont: reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted##c.vuln_cont ///
    l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
testparm c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont
estadd local Fint = "`: di %4.2f r(F)'"
estadd local pint = "`: di %5.3f r(p)'"
esttab hetcont using "$working_ANALYSIS/results/tables/tableS20_het_continuous.tex", ///
    se(%4.2f) b(%4.2f) booktabs replace nonotes ///
    keep(windspeed_predicted c.windspeed_predicted#c.windspeed_predicted vuln_cont c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont L4.mean_transfer d_exp_transfer) ///
    order(windspeed_predicted c.windspeed_predicted#c.windspeed_predicted vuln_cont c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont L4.mean_transfer d_exp_transfer) ///
    coeflabels(windspeed_predicted "Wind speed (kn)" c.windspeed_predicted#c.windspeed_predicted "Wind speed squared" vuln_cont "Vulnerability index (SD)" c.windspeed_predicted#c.vuln_cont "Wind speed {\(\times\)} Vulnerability" c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont "Wind speed squared {\(\times\)} Vulnerability" L4.mean_transfer "Lagged solidarity transfer" d_exp_transfer "Change in expected transfer") ///
    mtitles("Change in transfers") ///
    stats(N N_clust r2_a Fint pint, labels("N" "Clusters" "Adjusted R-squared" "F: wind-vulnerability interaction" "p: wind-vulnerability interaction") fmt(%9.0fc %9.0fc %9.3f %s %s)) ///
    star(* 0.10 ** 0.05 *** 0.01)
capture copy "$working_ANALYSIS/results/tables/tableS20_het_continuous.tex" "$working_ANALYSIS/../submission/2026 - NCC/tables/tableS20_het_continuous.tex", replace

* Heterogeneity tests. PRIMARY = continuous interaction of the wind-speed quadratic
* with the continuous vulnerability index (vuln_cont): the U-shape deepens with
* vulnerability, suggestive (p~0.10). SECONDARY = the tertile interaction (coarser;
* non-monotone because of binning, hence the flat middle tertile) and an exposure-
* balance check. Reported in the SI text and the main-text Fig. 2c caption.
* --- secondary: tertile interaction + exposure balance ---
reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted##i.vuln_group l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
testparm i.vuln_group#c.windspeed_predicted i.vuln_group#c.windspeed_predicted#c.windspeed_predicted
di _newline "Tertile heterogeneity test (secondary): F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p)
reg windspeed_predicted i.vuln_group if e(sample), vce(cluster session)
testparm i.vuln_group
di _newline "Exposure balance (wind ~ vuln_group): F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p)
* --- PRIMARY: continuous interaction ---
reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted##c.vuln_cont l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
testparm c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont
di _newline "Continuous vulnerability interaction (PRIMARY): F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p)
* exposure balance for the CONTINUOUS index: wind speed should be unrelated to vulnerability
reg windspeed_predicted c.vuln_cont if e(sample), vce(cluster session)
test vuln_cont
di _newline "Exposure balance (wind ~ vuln_cont, continuous): F(1," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p)


** Fig. S7.	Long-term effects: Haiyan still matters (2022)
preserve
clear
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
lab var windspeed_predicted "Wind speed (kn)"
lab var windspeed_sqr "Wind speed squared"

sort panel_id year

** SOME NEW ANALYSIS BASED ON AFFECTEDNESS REPORTED IN 2022
global vars2022 average_yolanda_trauma yolanda_rec_econ yolanda_rec_emot yolanda yolanda2

*Setup for regression analysis
global x_changed                                               // Primary spec: no post-treatment controls
global imbalances2 age gender hh_head single edu_1
global x_changed2                                               // Primary spec: no post-treatment controls (d22_a3gov d22_wealth in SI robustness)

eststo main_recip1: reg z_recip c.windspeed_predicted##c.windspeed_predicted if particip_12_16_22==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))


eststo main_recip2: reg z_recip c.windspeed_predicted##c.windspeed_predicted $x_changed2 $imbalances2 if particip_12_16_22==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, xtitle("Sustained wind speed (knots)") yline(0, lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) title("{bf:a} Positive reciprocity: Quadratic") xla(70(20)150, nogrid) yla(-.5(.25).5, nogrid) ytitle("Predicted reciprocity in SD") recastci(rarea) ciopts(lw(none) fcolor(%50))  xsize(3.165) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/figS7_a.gph", replace

* Wild cluster bootstrap-t for 2022 reciprocity (G=30 villages)
reg z_recip c.windspeed_predicted##c.windspeed_predicted $x_changed2 $imbalances2 if particip_12_16_22==1, vce(cluster session)
boottest ///
    c.windspeed_predicted ///
    c.windspeed_predicted#c.windspeed_predicted, ///
    cluster(session) reps(1000) seed(12345)


*Haiyan: Major life event
lpoly share_haiyan_stressful_2022 windspeed_predicted if particip_12_16_22==1,  xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) lineopts(lwidth(medthick)) msize(medlarge) mcolor(*.5) legend(off) title("{bf:c} Haiyan stressful") xtitle("Sustained wind speed (knots)") yla(0 "0%" 0.2 "20%" .4 "40%" .6 "60%" .8 "80%", nogrid) ytitle("Share") bwidth(6)  xla(70(20)150, nogrid) xsize(3.165) ysize(2) note("")
gr save  "$working_ANALYSIS/results/intermediate/figS7_c.gph", replace


eststo main_recip3: reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances if haiyan_stressful==0 &  particip_12_16_22==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
* note: margins saving() files are margins DATASETS (read by combomarginsplot), not graphs, despite the .gph extension
margins, at(windspeed_predicted = (70(5)145)) saving("$working_ANALYSIS/results/intermediate/2022_stress1.gph", replace)

eststo main_recip4: reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances if haiyan_stressful==1 &  particip_12_16_22==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145)) saving("$working_ANALYSIS/results/intermediate/2022_stress2.gph", replace)

combomarginsplot "$working_ANALYSIS/results/intermediate/2022_stress1.gph" "$working_ANALYSIS/results/intermediate/2022_stress2.gph" , labels("No (n=266)" "Yes (n=64)")  title("{bf:d} Change in average transfers by stressful") xtitle("Sustained wind speed (knots)") yline(0, lpattern(solid)) xla(70(20)150, nogrid) ytitle("Predicted change in PHP") yla(-20(20)40, nogrid) recastci(rarea) file1opts(msymbol(S)) file2opts() fileci1opts(recast(rarea) lw(none) fcolor(%70))  fileci2opts(recast(rarea) lw(none) fcolor(%30)) legend(rows(1)) xsize(3.165) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/figS7_d.gph", replace



*Table S20.	Long-term effects (2022): Reciprocity and stressful event
* Wind-speed categorical specification (ws_cat3); partition identical to the former distance categories.
eststo main_recip5: reg z_recip ib1.ws_cat3 if particip_12_16_22==1, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))


eststo main_recip6: reg z_recip ib1.ws_cat3 $x_changed2 $imbalances2 if particip_12_16_22==1, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(ws_cat3 = (1(1)3))  
marginsplot, xtitle("") yline(0, lpattern(solid)) title("{bf:b} Positive reciprocity: Wind-speed categories") xla(1(1)3) yla(-.5(.25).5) ytitle("Predicted reciprocity in SD") recastci(rarea) ciopts(lw(none) fcolor(%50)) xsize(3.165) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/figS7_b.gph", replace

gr combine "$working_ANALYSIS/results/intermediate/figS7_a" "$working_ANALYSIS/results/intermediate/figS7_b" "$working_ANALYSIS/results/intermediate/figS7_c" "$working_ANALYSIS/results/intermediate/figS7_d", xsize(4) ysize(3) cols(2) scale(1.1)
gr save  "$working_ANALYSIS/results/intermediate/figS7_longterm_2022.gph", replace
gr export "$working_ANALYSIS/results/figures/figS7_longterm_2022.png", replace width(3465)


esttab main_recip1 main_recip2  main_recip5 main_recip6 main_recip3 main_recip4 using "$working_ANALYSIS/results/tables/tableS20_longterm_2022.tex", label se(%4.2f) transform(ln*: exp(@) exp(@)) mgroups("Reciprocity: Quadratic" "Reciprocity: Wind-speed categories" "Change in Average transfers", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomtitles b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: Medium = high damages") fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace

* Console-only diagnostic on the wind-speed categories (not exported)
reg z_recip ib1.ws_cat3 $x_changed2 $imbalances2 if particip_12_16_22==1, vce(cluster session)

restore



* Fig. S8.	U-shaped transfers and diffusion of responsibility
* NOTE: This figure shows the lab experiment results in more detail than
* the main-text Figure 3. Panel A shows full sample (left) and by shock
* status (right). 

preserve
use "$working_ANALYSIS/data/litfe_u_shape.dta", clear

mylabels 0(10)60, myscale(@) local(pctlabel) suffix("%")

* Panel A left: Full sample
reg rel_trans100 c.damages##c.damages i.BarangayID, robust
margins, at(damages=(0(50)400))
marginsplot, ///
    title("{bf:a} Full sample", size(large)) ///
    ytitle("Share of resources transferred", size(medium)) ///
    xtitle("Resources in Group", size(medium)) ///
    ylabel(`pctlabel', nogrid labsize(medsmall) angle(0)) ///
    xlabel(, nogrid labsize(medsmall)) ///
    legend(off) ///
    recastci(rarea) ///
    ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(white)) ///
    plotregion(margin(small))
gr save "$working_ANALYSIS/results/intermediate/figS8_a.gph", replace

* Panel B: By shock status (unaffected vs. affected senders)
* Run interaction model to get both groups in one margins call
reg rel_trans100 c.damages##c.damages##i.shock i.BarangayID, robust
margins shock,at(damages=(0(50)400))
marginsplot, ///
    title("{bf:b} By shock status", size(large)) ///
    ytitle("Share of resources transferred", size(medium)) ///
    xtitle("Resources in Group", size(medium)) ///
    ylabel(`pctlabel', nogrid labsize(medsmall) angle(0)) ///
    xlabel(, nogrid labsize(medsmall)) ///
    recastci(rarea) ///
    ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(white)) ///
    plotregion(margin(medsmall)) ///
    legend(order(3 "Sender without shock" 4 "Sender with shock") rows(1) size(medium))
gr save "$working_ANALYSIS/results/intermediate/figS8_b.gph", replace

* Combine panels
gr combine "$working_ANALYSIS/results/intermediate/figS8_a.gph" ///
    "$working_ANALYSIS/results/intermediate/figS8_b.gph", ///
    cols(2) xsize(4) ysize(2) graphregion(color(white))
gr save  "$working_ANALYSIS/results/intermediate/figS8_lab_diffusion.gph", replace
gr export "$working_ANALYSIS/results/figures/figS8_lab_diffusion.png", replace width(4000)


** Table S21.  Lab-in-the-field: Diffusion of responsibility
* Model 1: Full sample
eststo tableS21_1: reg rel_trans100 c.damages##c.damages i.BarangayID, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

* Model 2: No shock (unaffected senders only)
eststo tableS21_2: reg rel_trans100 c.damages##c.damages i.BarangayID if shock==0, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

* Model 3: Shock (affected senders only)
eststo tableS21_3: reg rel_trans100 c.damages##c.damages i.BarangayID if shock==1, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

* Model 4: Excluding situation B observations (high diffusion: 1 catastrophic shock)
eststo tableS21_4: reg rel_trans100 c.damages##c.damages i.BarangayID if one_cat!=1, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

* Model 5: Excluding situation A observations (low diffusion: 2 medium shocks)
eststo tableS21_5: reg rel_trans100 c.damages##c.damages i.BarangayID if two_med!=1, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab tableS21_1 tableS21_2 tableS21_3 tableS21_4 tableS21_5 ///
	using "$working_ANALYSIS/results/tables/tableS21_lab_diffusion.tex", booktabs replace ///
	se(%9.3f) b(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
	keep(damages c.damages#c.damages) ///
	order(damages c.damages#c.damages) ///
	coeflabels(damages "Available group resources" c.damages#c.damages "Available group resources squared") ///
	scalars("N Observations" "r2_a Adjusted R-squared" "Fstat F-Test: Damages and Damages2") ///
	mtitles("Full sample" "No shock" "Shock" "Excl. situation B" "Excl. situation A") ///
	nonotes ///
	nonumber
	
restore


* Table S22.	Role of aid satisfaction
eststo house_aid1: reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 if severely_damaged==0 & returner==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145)) saving("$working_ANALYSIS/results/intermediate/aid_satisfaction_no_house_damage.gph", replace)
nlcom (x_star: -_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted]))
*dynamic turning point and sample range (replaces hardcoded 0 / 57.77 / 119.07 on the distance axis)
local xstar = -_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted])
summ windspeed_predicted if e(sample)
local xmin = r(min)
local xmax = r(max)
nlcom (in_range_min: (-_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted])) - `xmin')
nlcom (in_range_max: `xmax' - (-_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted])))

*how big is the dip in solidarity? (evaluate at sample min, turning point, sample max)
margins, at(windspeed_predicted=(`xmin' `xstar' `xmax')) vsquish post

* Differences: turning point vs ends
lincom _b[2._at] - _b[1._at]   // y(x*) - y(xmin)
lincom _b[2._at] - _b[3._at]   // y(x*) - y(xmax)

eststo house_aid2: reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 if severely_damaged==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab house_aid1 house_aid2 using "$working_ANALYSIS/results/tables/tableS22_aid_satisfaction.tex",  label se(%4.2f) transform(ln*: exp(@) exp(@)) mgroups("Aid Satisfaction", pattern(1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) mtitles("Not severe damage" "Severely damaged") b(%4.2f) stats(N N_clust r2_a Fstat , labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²") fmt(%9.0fc %9.0fc %9.3f %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


*Fig. S9.	Haiyan exposure and post-disaster social and institutional perceptions
sum satisfaction_aid_norm mismanagement_aid_norm corruption_aid_norm help_volume_norm help_source_norm social_network_damage_norm rank1_norm
* Capture sample means for the reference lines (never hardcode; analysis_rdy is rebuilt upstream)
foreach v in satisfaction_aid_norm mismanagement_aid_norm corruption_aid_norm help_volume_norm help_source_norm social_network_damage_norm {
	quietly sum `v'
	local mean_`v' = r(mean)
}

*A:  perceived Aid Satisfaction
eststo experience1: reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:a} Perceived satisfaction with aid") xtitle("Sustained wind speed (knots)") yline(`mean_satisfaction_aid_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_A.gph", replace


*B: Perceived Aid Mismanagement
eststo experience2: reg mismanagement_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:b} Perceived aid mismanagement") xtitle("Sustained wind speed (knots)") yline(`mean_mismanagement_aid_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_B.gph", replace


*C: Perceived Aid Corruption
eststo experience3: reg corruption_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:c} Perceived aid corruption") xtitle("Sustained wind speed (knots)") yline(`mean_corruption_aid_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_C.gph", replace

*D: Perceived Volume of Support
eststo experience4: reg help_volume_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:d} Perceived volume of support") xtitle("Sustained wind speed (knots)") yline(`mean_help_volume_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_D.gph", replace

*E: Perceived Source of Support
eststo experience5: reg help_source_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:e} Perceived source of support") xtitle("Sustained wind speed (knots)") yline(`mean_help_source_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_E.gph", replace

*F: Perceived Social Network Damage
eststo experience6: reg social_network_damage_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 , vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(windspeed_predicted = (70(5)145))
marginsplot, title("{bf:f} Perceived social network damage") xtitle("Sustained wind speed (knots)") yline(`mean_social_network_damage_norm', lpattern(solid)) xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid)  yla(0(20)100)  ytitle("Predicted Score") recastci(rarea) ciopts(lw(none) fcolor(%50)) 
gr save  "$working_ANALYSIS/results/intermediate/experience_F.gph", replace



gr combine "$working_ANALYSIS/results/intermediate/experience_A.gph" "$working_ANALYSIS/results/intermediate/experience_B.gph" "$working_ANALYSIS/results/intermediate/experience_C.gph" "$working_ANALYSIS/results/intermediate/experience_D.gph" "$working_ANALYSIS/results/intermediate/experience_E.gph" "$working_ANALYSIS/results/intermediate/experience_F.gph" , xsize(5) ysize(3) rows(2) scale(1.2)
gr save  "$working_ANALYSIS/results/intermediate/FigS9_u_shape_perceptions.gph", replace
gr export "$working_ANALYSIS/results/figures/FigS9_u_shape_perceptions.png", replace width(4000)


*Table S23.	Haiyan exposure and post-disaster social and institutional perceptions
esttab experience1 experience2 experience3 experience4 experience5 experience6  using "$working_ANALYSIS/results/tables/tableS23_non_linearity_perceptions_post_haiyan.tex",  label se(%4.2f) transform(ln*: exp(@) exp(@)) mtitles("Satisfaction Aid" "Mismanagement Aid" "Corruption Aid" "Support Volume" "Support Source" "Network Damage") b(%4.2f) stats(N N_clust r2_a Fstat , labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²") fmt(%9.0fc %9.0fc %9.3f %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace




*Table S24.	Controlling for post-Haiyan individual perceptions and experiences
global post_haiyan satisfaction_aid_norm mismanagement_aid_norm corruption_aid_norm help_volume_norm help_source_norm social_network_damage_norm  
*full sample
eststo reg_post1: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

eststo reg_post2: reg d_mean_transfer windspeed_predicted windspeed_sqr $post_haiyan l4.mean_transfer d_exp_transfer $x_changed $imbalances , vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
testparm $post_haiyan
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab reg_post1 reg_post2 using "$working_ANALYSIS/results/tables/tableS24_controlling_for_post_haiyan_experiences.tex", keep(windspeed_predicted windspeed_sqr  $post_haiyan) label se(%4.2f) transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomtitles posthead("& \multicolumn{2}{c}{Full sample} \\" "\cmidrule(lr){2-3}" "\midrule") b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: Post-Haiyan") fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace



*Table S25.	Controlling for village level post-Haiyan perceptions and experiences
foreach x of varlist $post_haiyan  {
	egen `x'_v = mean(`x'), by(village)
	local lbl : variable label `x'
	lab var `x'_v "`lbl'"
}

global post_haiyan2 satisfaction_aid_norm_v mismanagement_aid_norm_v corruption_aid_norm_v help_volume_norm_v help_source_norm_v social_network_damage_norm_v 

*full sample
eststo reg_post6: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

eststo reg_post7: reg d_mean_transfer windspeed_predicted windspeed_sqr $post_haiyan2 l4.mean_transfer d_exp_transfer $x_changed $imbalances , vce(cluster session)
test windspeed_predicted=windspeed_sqr=0
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
testparm $post_haiyan2
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab  reg_post6 reg_post7 using "$working_ANALYSIS/results/tables/tableS25_controlling_for_post_haiyan_experiences_village_level.tex", keep(windspeed_predicted windspeed_sqr  $post_haiyan2) label se(%4.2f) transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomtitles posthead("& \multicolumn{2}{c}{Full sample} \\" "\cmidrule(lr){2-3}" "\midrule") b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: Post-Haiyan") fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace



*------------------------------------------------------------
*3.5.	Robustness checks of changes in solidarity transfers
*------------------------------------------------------------
** Table S26.	Differences between randomly invited participants and friends
sort panel_id year   // restore panel order: upstream egen by(village) re-sorted; [_n+1] fill must run on panel order
replace anchor = anchor[_n+1] if anchor == . & year!=2022
replace ipw_anchor = ipw_anchor[_n+1] if ipw_anchor == . & year!=2022

eststo anchor1: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances , vce(cluster session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor2: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances [pweight=ipw_anchor], vce(cluster session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor3: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances if anchor==0, vce(cluster session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor4: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances if anchor==1, vce(cluster session)
testparm windspeed_predicted windspeed_sqr
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

* Wind-speed categorical specification (ws_cat3); partition identical to the former distance categories.
eststo anchor5: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor6: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances [pweight=ipw_anchor], vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor7: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances if anchor==0, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

eststo anchor8: reg d_mean_transfer ib1.ws_cat3 l4.mean_transfer d_exp_transfer $x_changed $imbalances if anchor==1, vce(cluster session)
testparm 2.ws_cat3 3.ws_cat3, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)'''")))

esttab anchor1 anchor2 anchor3 anchor4 anchor5 anchor6 anchor7 anchor8 using "$working_ANALYSIS/results/tables/tableS26_anchor_vs_friend.tex",  keep(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer $x_changed) order(windspeed_predicted windspeed_sqr 2.ws_cat3 3.ws_cat3 L4.mean_transfer d_exp_transfer $x_changed) label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Quadratic specification" "Levels specification", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  mtitles("Full sample"  "IPW" "Invitees" "Randomly invited" "Full sample"  "IPW" "Invitees" "Randomly invited") b(%4.2f) stats(N N_clust r2_a Fstat Fstat2, labels("N" "Cluster" "Adjusted R-squared" "F-Test: wind speed and wind speedÃ‚Â²" "F-Test: medium damages = high damages" ) fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


preserve
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
lab var windspeed_predicted "Wind speed (kn)"
lab var windspeed_sqr "Wind speed squared"

global x_1 age gender hh_head single edu_1 a3gov wealth_index


** Table S27.	Effects of Haiyan without baseline and full 2016 sample
eststo tableS27_1: reg mean_transfer windspeed_predicted windspeed_sqr if year==2016, vce(cluster session)
eststo tableS27_2: reg mean_transfer windspeed_predicted windspeed_sqr exp_transfer if year==2016, vce(cluster session)
eststo tableS27_3: reg mean_transfer windspeed_predicted windspeed_sqr exp_transfer $x_1 stranger if year==2016, vce(cluster session)

esttab tableS27_1 tableS27_2 tableS27_3  using "$working_ANALYSIS/results/tables/tableS27_without_baseline.tex",  order(windspeed_predicted windspeed_sqr) label se(%4.2f) transform(ln*: exp(@) exp(@)) nomtitles b(%4.2f) stats(N N_clust r2_a, labels("N" "Cluster" "Adjusted R-squared" ) fmt(%9.0fc %9.0fc %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace
restore




** Table S28.	Solidarity Transfers: Excluding the two villages that experienced medium damages
eststo tableS28_1: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer if medium_affected_village==0, vce(cluster session)
eststo tableS28_2: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer if medium_affected_village==0, vce(cluster session)
eststo tableS28_3: reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances if medium_affected_village==0, vce(cluster session)
* Predicted profile: factor-notation twin so margins traces the quadratic (windspeed_sqr must move with windspeed)
quietly reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $x_changed $imbalances if medium_affected_village==0, vce(cluster session)
margins, at(windspeed_predicted = (70(5)145))


esttab tableS28_1 tableS28_2 tableS28_3 using "$working_ANALYSIS/results/tables/tableS28_exclusion_medium.tex", order(windspeed_predicted windspeed_sqr L4.mean_transfer d_exp_transfer $x_changed $imbalances) label se(%4.2f)  transform(ln*: exp(@) exp(@))  nomtitles b(%4.2f) stats(N N_clust r2_a, labels("N" "Cluster" "Adjusted R-squared" ) fmt(%9.0fc %9.0fc %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace

 
** Table S29.	Solidarity Transfers: Affectedness index instead of distance
eststo cont_damages1: reg d_mean_transfer affectedness_index affectedness_index_sqr l4.mean_transfer, vce(cluster session)
eststo cont_damages2: reg d_mean_transfer affectedness_index affectedness_index_sqr l4.mean_transfer d_exp_transfer, vce(cluster session)
eststo cont_damages3: reg d_mean_transfer affectedness_index affectedness_index_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
	
esttab cont_damages1 cont_damages2 cont_damages3 using "$working_ANALYSIS/results/tables/tableS29_damages_cont.tex",   order(affectedness_index affectedness_index_sqr L4.mean_transfer d_exp_transfer $x_changed $imbalances) label se(%4.2f)  transform(ln*: exp(@) exp(@))  nomtitles b(%4.2f) stats(N N_clust r2_a, labels("N" "Cluster" "Adjusted R-squared" ) fmt(%9.0fc %9.0fc %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace



*Table S30.	Solidarity Transfers: Levels of damages (using affectedness index)
xtile affectedness_cat = affectedness_index, nq(5)

eststo level_damages1: reg d_mean_transfer i.affectedness_cat l4.mean_transfer, vce(cluster session)
eststo level_damages2: reg d_mean_transfer i.affectedness_cat l4.mean_transfer d_exp_transfer, vce(cluster session)
eststo level_damages3: reg d_mean_transfer i.affectedness_cat l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)

esttab level_damages1 level_damages2 level_damages3 using "$working_ANALYSIS/results/tables/tableS30_damage_levels.tex",drop(1.affectedness_cat)  order(2.affectedness_cat 3.affectedness_cat 4.affectedness_cat 5.affectedness_cat L4.mean_transfer d_exp_transfer $x_changed) label se(%4.2f)  transform(ln*: exp(@) exp(@)) mgroups("Change in Average transfers", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitles b(%4.2f) stats(N N_clust r2_a,  labels("N" "Cluster" "Adjusted R-squared" ) fmt(%9.0fc %9.0fc %9.3f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace


** Table S31.	Solidarity Transfers: Tobit model to account for censoring
eststo tableS31_1: tobit d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer,ll(-70) ul(70) vce(cluster session)
eststo tableS31_2: tobit d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer,ll(-70) ul(70) vce(cluster session)
eststo tableS31_3: tobit d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $x_changed $imbalances, ll(-70) ul(70) vce(cluster session)
	
esttab tableS31_1 tableS31_2 tableS31_3 using "$working_ANALYSIS/results/tables/tableS31_tobit.tex", keep(windspeed_predicted windspeed_sqr L4.mean_transfer d_exp_transfer $x_changed $imbalances _cons) order(windspeed_predicted windspeed_sqr L4.mean_transfer d_exp_transfer $x_changed $imbalances) label se(%4.2f)  transform(ln*: exp(@) exp(@))  nomtitles b(%4.2f) stats(N N_clust r2_p, labels("N" "Cluster" "Pseudo R-squared" ) fmt(%9.0fc %9.0fc %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace





*Figure S10.
* DISTANCE-BASED ROBUSTNESS (retained deliberately): sensemakr requires a binary treatment; we keep the
* medium-vs-low distance-category contrast (dist_cat2, excluding high). A wind-speed-based binary split
* would redefine the treated group rather than re-express it, so the original definition is preserved.
*bounded sensitivity: benchmarking against baseline transfers
sensemakr d_mean_transfer dist_cat2 L4.mean_transfer d_exp_transfer $x_changed $imbalances if distance_cat!=3, treat(dist_cat2) benchmark(l4.mean_transfer)  kd(0.5(0.5)2) contourplot
gr_edit title.text = {}
gr_edit title.text.Arrpush {bf:a} Benchmark against baseline transfers
gr save  "$working_ANALYSIS/results/intermediate/figS10_sensitive_a.gph", replace


*extreme scenarios
sensemakr d_mean_transfer dist_cat2 L4.mean_transfer d_exp_transfer $x_changed $imbalances if distance_cat!=3, treat(dist_cat2) extremeplot
gr_edit legend.DragBy 59.08255581799587 -33.16378349023947
gr_edit title.text = {}
gr_edit title.text.Arrpush {bf:b} Extreme scenario
gr_edit legend.style.editstyle boxstyle(shadestyle(color(none))) editcopy
gr_edit legend.Edit, style(labelstyle(color(custom)))
gr_edit legend.Edit, style(labelstyle(color(custom)))
gr save  "$working_ANALYSIS/results/intermediate/figS10_sensitive_b.gph", replace


gr combine "$working_ANALYSIS/results/intermediate/figS10_sensitive_a" "$working_ANALYSIS/results/intermediate/figS10_sensitive_b", xsize(4) ysize(2) scale(1.3) cols(2) imargin(high)
gr export "$working_ANALYSIS/results/figures/figS10_sensitivity.png", replace width(3165)




** Table S33.	Effect of Haiyan on solidarity expectations
eststo expectations1: reg d_exp_transfer windspeed_predicted windspeed_sqr, vce(cluster session)
testparm windspeed_predicted windspeed_sqr, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

eststo expectations2: reg d_exp_transfer windspeed_predicted windspeed_sqr l4.exp_transfer, vce(cluster session)
testparm windspeed_predicted windspeed_sqr, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

eststo expectations3: reg d_exp_transfer windspeed_predicted windspeed_sqr l4.exp_transfer $x_changed $imbalances, vce(cluster session)
testparm windspeed_predicted windspeed_sqr, equal
estadd local Fstat2 = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))

esttab expectations1 expectations2 expectations3 using "$working_ANALYSIS/results/tables/tableS33_expectations.tex", order(windspeed_predicted windspeed_sqr L4.exp_transfer $x_changed $imbalances) label se(%4.2f) transform(ln*: exp(@) exp(@)) nomtitles posthead("& \multicolumn{3}{c}{Dependent variable: \(\Delta\) Expected transfer} \\" "\midrule") mgroups("Full Sample", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) b(%4.2f) stats(N N_clust Fstat2 r2_a, labels("N" "Cluster" "F-Test: wind speed and wind speedÃ‚Â²" "Adjusted R-squared" ) fmt(%9.0fc %9.0fc %9.3f %9.3f) ) star(* 0.10 ** 0.05 *** 0.01) varlabels(_cons "Constant", elist(weight:_cons "{break}{hline @width}")) nonotes booktabs replace




*==============================================================================
** Table S32.	Multiple testing adjustments (Benjamini-Hochberg)
*==============================================================================
* Primary confirmatory test: pooled-sample quadratic specification (joint F-test)
* Exploratory tests adjusted using BH procedure:
*   1. Economic-vulnerability heterogeneity (wind quadratic x continuous index)
*   2. Ambiguity gap regression
*   3. Aid satisfaction F-test (not severely damaged)
*   4. Aid satisfaction F-test (severely damaged)

* Reload data for clean estimation
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
lab var windspeed_predicted "Wind speed (kn)"
lab var windspeed_sqr "Wind speed squared"

drop if returner == 0
sort panel_id year
global x_changed
global imbalances age gender hh_head single edu_1 stranger

* Collect p-values from exploratory tests
matrix pvals = J(4, 2, .)
matrix colnames pvals = "raw_p" "q_value"
matrix rownames pvals = "Het_vulnerability" "Ambiguity_gap" "AidSat_notsevere" "AidSat_severe"

* 1. Heterogeneity: wind-speed quadratic x continuous economic-vulnerability index
reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted##c.vuln_cont l4.mean_transfer d_exp_transfer $x_changed $imbalances, vce(cluster session)
testparm c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont
matrix pvals[1,1] = r(p)

* 2. Ambiguity gap regression
reg diff_severe_damage_aid c.windspeed_predicted##c.windspeed_predicted if year == 2016, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
matrix pvals[2,1] = r(p)

* 3. Aid satisfaction: not severely damaged
reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 if severely_damaged==0, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
matrix pvals[3,1] = r(p)

* 4. Aid satisfaction: severely damaged
reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted $x_changed age gender hh_head single edu_1 if severely_damaged==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
matrix pvals[4,1] = r(p)

* Benjamini-Hochberg procedure
* Step 1: Sort p-values (manually since Stata matrices don't sort easily)
local m = 4
* Create temporary dataset for BH calculation
preserve
clear
set obs `m'
gen test_id = _n
gen raw_p = .
forvalues i = 1/`m' {
    replace raw_p = pvals[`i',1] in `i'
}
sort raw_p
gen rank = _n
gen bh_critical = (rank / `m') * 0.05
gen q_value = min(raw_p * `m' / rank, 1)

* Enforce monotonicity: adjusted p-values must be non-decreasing from largest rank down
gsort -rank
replace q_value = min(q_value, q_value[_n-1]) if _n > 1
gsort rank

* Display results
list test_id rank raw_p bh_critical q_value, sep(0)

* Store back to matrix
sort test_id
forvalues i = 1/`m' {
    local bh_p = q_value[`i']
    matrix pvals[`i', 2] = `bh_p'
}
restore

* Display final results
di _newline(2) "============================================="
* Display and export results
matrix list pvals, format(%6.2f) title("Multiple Testing Adjustments")

esttab matrix(pvals, fmt(%6.2f)) using "$working_ANALYSIS/results/tables/tableS32_bh_adjustments.tex", ///
	coeflabels(Het_vulnerability "Economic-vulnerability interaction" Ambiguity_gap "Ambiguity gap" AidSat_notsevere "Aid satisfaction: not severely damaged" AidSat_severe "Aid satisfaction: severely damaged") ///
	mlabels(none) collabels("Raw \(p\)-value" "\(q\)-value") nonumbers ///
    nonotes ///
    booktabs replace



* ============================================================================
* Merged from 04_robustness_windspeed.do: exposure-equivalence diagnostics
* (TCE-DAT wind speed vs calibrated wind speed; log output only)
* ============================================================================
global working_ANALYSIS : pwd
cap mkdir "${working_ANALYSIS}/scripts/logs"
cap mkdir "${working_ANALYSIS}/results/tables"
* ---------- Load and merge ----------
use "${working_ANALYSIS}/processed/analysis_rdy.dta", clear

merge m:1 session year using "${working_ANALYSIS}/data/windspeed_tcedat.dta", keep(1 3)
tab _merge
drop _merge

* Fill windspeed for all years (it's session-level, from Haiyan)
bys session (windspeed_kn): replace windspeed_kn = windspeed_kn[_N] if missing(windspeed_kn)
* Fill remaining missings across years
bys session: egen _ws = max(windspeed_kn)
replace windspeed_kn = _ws if missing(windspeed_kn)
drop _ws

* Sort for lag operators (same approach as 02_analysis_main.do)
sort panel_id year
tsset panel_id year
gen windspeed_kn_sqr = windspeed_kn^2

* Controls
global imbalances age gender hh_head single edu_1 stranger
global imbalances2 age gender hh_head single edu_1   // S20 control set: no stranger/high_educ (not in 2022 wave); matches 03's S20 block redefinition

* ---------- Summary ----------
di _n "=== WINDSPEED SUMMARY (year==2016) ==="
sum windspeed_kn if year == 2016, detail
corr distance_storm_km_reversed windspeed_kn if year == 2016

* ---------- PANEL A: Medium-term solidarity ----------
di _n(3) "=============================================="
di "PANEL A: d_mean_transfer (2012-2016 change)"
di "=============================================="

di _n "--- Main spec (DISTANCE, for reference) ---"
reg d_mean_transfer distance_storm_km_reversed distance_rev_sqr ///
    l4.mean_transfer d_exp_transfer $imbalances if year == 2016, vce(cluster session)
local tp_dist = -_b[distance_storm_km_reversed] / (2 * _b[distance_rev_sqr])
di "Turning point (distance_reversed): " round(`tp_dist', 0.1) " km"

di _n "--- Robustness (WINDSPEED, quadratic) ---"
reg d_mean_transfer windspeed_kn windspeed_kn_sqr ///
    l4.mean_transfer d_exp_transfer $imbalances if year == 2016, vce(cluster session)
local tp_wind = -_b[windspeed_kn] / (2 * _b[windspeed_kn_sqr])
di "Turning point (windspeed): " round(`tp_wind', 0.1) " kn"

di _n "--- Robustness (WINDSPEED, categorical) ---"
gen ss_cat = .
replace ss_cat = 1 if windspeed_kn >= 34 & windspeed_kn < 64
replace ss_cat = 2 if windspeed_kn >= 64 & windspeed_kn < 83
replace ss_cat = 3 if windspeed_kn >= 83 & windspeed_kn < 137
replace ss_cat = 4 if windspeed_kn >= 137
label define ss_lbl 1 "TS" 2 "Cat 1" 3 "Cat 2-4" 4 "Cat 5"
label values ss_cat ss_lbl
tab ss_cat if year == 2016

reg d_mean_transfer i.ss_cat l4.mean_transfer ///
    d_exp_transfer $imbalances if year == 2016, vce(cluster session)

* ---------- PANEL B: Long-term reciprocity ----------
di _n(3) "=============================================="
di "PANEL B: z_recip (2022 long-term reciprocity)"
di "=============================================="

di _n "--- Main spec (DISTANCE) ---"
reg z_recip c.distance_storm_km_reversed##c.distance_storm_km_reversed ///
    $imbalances2 if particip_12_16_22 == 1, vce(cluster session)   // no year filter: stranger/high_educ not in 2022 wave; matches S20 + script 10 Panel B sample
local tp_dist2 = -_b[distance_storm_km_reversed] / (2 * _b[c.distance_storm_km_reversed#c.distance_storm_km_reversed])
di "Turning point (distance_reversed): " round(`tp_dist2', 0.1) " km"

di _n "--- Robustness (WINDSPEED, quadratic) ---"
reg z_recip c.windspeed_kn##c.windspeed_kn ///
    $imbalances2 if particip_12_16_22 == 1, vce(cluster session)   // no year filter: stranger/high_educ not in 2022 wave; matches S20 + script 10 Panel B sample
local tp_wind2 = -_b[windspeed_kn] / (2 * _b[c.windspeed_kn#c.windspeed_kn])
di "Turning point (windspeed): " round(`tp_wind2', 0.1) " kn"

di _n "--- Robustness (WINDSPEED, categorical) ---"
reg z_recip i.ss_cat ///
    $imbalances2 if particip_12_16_22 == 1, vce(cluster session)   // no year filter: stranger/high_educ not in 2022 wave; matches S20 + script 10 Panel B sample

di _n(3) "=============================================="
di "SUMMARY"
di "=============================================="
di "Panel A turning point (distance_reversed): " round(`tp_dist', 0.1) " km"
di "Panel A turning point (windspeed):         " round(`tp_wind', 0.1) " kn"
di "Panel B turning point (distance_reversed): " round(`tp_dist2', 0.1) " km"
di "Panel B turning point (windspeed):         " round(`tp_wind2', 0.1) " kn"


* ============================================================================
* Merged from 06_ambiguity_gap_robustness.do: Figure S11 (ambiguity-gap robustness)
* ============================================================================
local ssline xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))
use "${working_ANALYSIS}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${working_ANALYSIS}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws

* --- severe-damage variants (individual level, 2016) ---
gen sev_d1 = (h2house==1) if y16==1 & !missing(h2house)
gen sev_d2 = (damage_TOTAL > 4500 & h2house==1) if y16==1 & !missing(h2house) & !missing(damage_TOTAL)
gen sev_d3 = (damage_TOTAL > 2250 & h2house==1) if y16==1 & !missing(h2house) & !missing(damage_TOTAL)
gen sev_d4 = (damage_TOTAL > 9000 & h2house==1) if y16==1 & !missing(h2house) & !missing(damage_TOTAL)
qui summ damage_TOTAL if y16==1 & h2house==1, detail
local med = r(p50)
di as result "Median damage among house-damaged (2016): `med' PHP"
gen sev_d5 = (damage_TOTAL > `med' & h2house==1) if y16==1 & !missing(h2house) & !missing(damage_TOTAL)

* --- village shares over ALL 2016 respondents; gaps in pp ---
forval d = 1/5 {
    egen sh_d`d' = mean(sev_d`d') if y16==1, by(session)
    gen gap_d`d' = (share_need_aid - sh_d`d')*100
}

* --- Figure 3 sample: returners ---
drop if returner== 0

* --- tables: severe share and gap by Saffir-Simpson category (one obs per village) ---
egen tag_vil = tag(session) if y16==1
gen ss_cat = 1 if windspeed_predicted < 83
replace ss_cat = 2 if windspeed_predicted >= 83  & windspeed_predicted < 96
replace ss_cat = 3 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace ss_cat = 4 if windspeed_predicted >= 113 & windspeed_predicted < 137
replace ss_cat = 5 if windspeed_predicted >= 137 & !missing(windspeed_predicted)
di as result _n "=== Severe-damage SHARE by definition and SS category (village means) ==="
tabstat sh_d1 sh_d2 sh_d3 sh_d4 sh_d5 if y16==1 & tag_vil, by(ss_cat) stat(mean) format(%9.2f)
di as result _n "=== Ambiguity GAP (pp) by definition and SS category (village means) ==="
tabstat gap_d1 gap_d2 gap_d3 gap_d4 gap_d5 if y16==1 & tag_vil, by(ss_cat) stat(mean) format(%9.1f)

* --- overlay figure ---
twoway ///
    (lpoly gap_d2 windspeed_predicted if y16==1, bwidth(6) lcolor("120 94 240") lwidth(thick)) ///
    (lpoly gap_d1 windspeed_predicted if y16==1, bwidth(6) lcolor("100 143 255") lwidth(medthin) lpattern(dash)) ///
    (lpoly gap_d3 windspeed_predicted if y16==1, bwidth(6) lcolor("220 38 127") lwidth(medthin) lpattern(shortdash)) ///
    (lpoly gap_d4 windspeed_predicted if y16==1, bwidth(6) lcolor("254 97 0") lwidth(medthin) lpattern(longdash)) ///
    (lpoly gap_d5 windspeed_predicted if y16==1, bwidth(6) lcolor("255 176 0") lwidth(medthin) lpattern(dash_dot)) ///
    , `ssline' yline(0, lcolor(gs10) lwidth(thin)) ///
    text(-17 76 "Cat 1", size(small) color(gs8)) ///
    text(-17 89.5 "Cat 2", size(small) color(gs8)) ///
    text(-17 104.5 "Cat 3", size(small) color(gs8)) ///
    text(-17 125 "Cat 4", size(small) color(gs8)) ///
    text(-17 144 "Cat 5", size(small) color(gs8)) ///
    title("Ambiguity gap: robustness to severe-damage definition", size(medium)) ///
    xtitle("Sustained wind speed (knots)", size(medsmall)) ///
    ytitle("Stated need {&minus} severe damage (pp)", size(medsmall)) ///
    yla(-20(20)60, nogrid labsize(small)) xla(70(20)150, nogrid labsize(small)) ///
    legend(order(1 "Damage > 4,500 PHP & house damaged (paper)" ///
                 2 "Any house damage" ///
                 3 "Damage > 2,250 PHP & house damaged" ///
                 4 "Damage > 9,000 PHP & house damaged" ///
                 5 "Above-median damage & house damaged") ///
           rows(5) size(small) pos(6)) ///
    xsize(3.2) ysize(3) graphregion(color(white))
gr export "${working_ANALYSIS}/results/figures/fig_ambiguity_gap_robustness.png", replace width(2100)

di as result _n "06 ROBUSTNESS DONE"


* ============================================================================
* SECTION S4 -- Pre-submission robustness additions (Referee-1 review, 2026-06)
*   Fig. S12  Raw village-level local-polynomial fits (no parabola imposed)
*   Table S34 Leave-one-village-out jackknife of the U-test + turning point
*   Table S35 Lind-Mehlum U-test detail + percentile bootstrap turning-point CIs
*   Table S36 Balance along the wind-speed gradient (baseline orthogonality)
* Self-contained: reloads analysis_rdy; does NOT drop returner==0 (z_recip lives
* on 2022 rows coded returner==0); reciprocity selects on particip_12_16_22.
* ============================================================================
local INT "${working_ANALYSIS}/results/intermediate"
cap mkdir "${working_ANALYSIS}/results/intermediate"
cap mkdir "${working_ANALYSIS}/results/tables"
cap mkdir "${working_ANALYSIS}/results/figures"

* ---- data setup (mirrors 03_analysis_SI.do) ----
use "${working_ANALYSIS}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${working_ANALYSIS}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2
* pre-computed static lag, so the cluster bootstrap (which duplicates villages and
* would break tsset for an l4. operator -> "repeated time values") runs cleanly.
gen L4_mean_transfer = l4.mean_transfer
* NOTE: do NOT `drop if returner==0` globally. z_recip (2022 reciprocity) is non-
* missing only on 2022 rows, which are coded returner==0; dropping them empties the
* reciprocity sample (0-obs clustered reg -> hangs). Transfers regs self-select the
* returner-2016 rows via d_mean_transfer; reciprocity regs select on particip_12_16_22.
global imbalances  age gender hh_head single edu_1 stranger
global imbalances2 age gender hh_head single edu_1

* ============================================================================
* Fig. S12 -- raw village-level fits (lowess; no parabola imposed)
* ============================================================================
preserve
  collapse (mean) y=d_mean_transfer ws=windspeed_predicted, by(session)
  twoway (scatter y ws, msymbol(Oh) msize(medium) mcolor(gs6)) ///
         (lowess y ws, bwidth(0.6) lwidth(medthick) lcolor("220 38 127")) ///
         , xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) ///
         yline(0, lcolor(gs10) lwidth(thin)) ///
         title("{bf:a} Transfers (3 years), village means", size(medium)) ///
         xtitle("Sustained wind speed (knots)", size(medsmall)) ///
         ytitle("{&Delta} Transfers in PHP", size(medsmall)) ///
         xla(70(20)150, nogrid labsize(small)) yla(, nogrid labsize(small) angle(0)) ///
         legend(off) note("") xsize(3.165) ysize(2) scale(1.1)
  gr save "`INT'/figS12_a.gph", replace
restore
preserve
  keep if particip_12_16_22==1
  collapse (mean) y=z_recip ws=windspeed_predicted, by(session)
  twoway (scatter y ws, msymbol(Oh) msize(medium) mcolor(gs6)) ///
         (lowess y ws, bwidth(0.6) lwidth(medthick) lcolor("100 143 255")) ///
         , xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) ///
         yline(0, lcolor(gs10) lwidth(thin)) ///
         title("{bf:b} Reciprocity (9 years), village means", size(medium)) ///
         xtitle("Sustained wind speed (knots)", size(medsmall)) ///
         ytitle("Reciprocity in SD", size(medsmall)) ///
         xla(70(20)150, nogrid labsize(small)) yla(, nogrid labsize(small) angle(0)) ///
         legend(off) note("") xsize(3.165) ysize(2) scale(1.1)
  gr save "`INT'/figS12_b.gph", replace
restore
gr combine "`INT'/figS12_a.gph" "`INT'/figS12_b.gph", rows(1) xsize(6.5) ysize(2.2)
gr export "${working_ANALYSIS}/results/figures/figS12_raw_village_fits.png", replace width(2100)
di as result _n "FIG S12 DONE"

* ============================================================================
* Table S34 -- leave-one-village-out jackknife of U-test + turning point
* ============================================================================
qui levelsof session, local(sess)
matrix S34 = J(8, 2, .)
matrix rownames S34 = full_TP full_Utest_t full_Utest_p jk_min_TP jk_med_TP jk_max_TP jk_n_sig30 jk_min_t
matrix colnames S34 = Transfers2016 Reciprocity2022

* ---- 2016 transfers ----
qui reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
qui utest windspeed_predicted windspeed_sqr
matrix S34[1,1]=r(extr)
matrix S34[2,1]=r(t)
matrix S34[3,1]=r(p)
matrix JK16 = J(30,4,.)
matrix colnames JK16 = sess tp t p
local i=0
foreach s of local sess {
  local ++i
  qui reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $imbalances if session!=`s', vce(cluster session)
  qui utest windspeed_predicted windspeed_sqr
  matrix JK16[`i',1]=`s'
  matrix JK16[`i',2]=r(extr)
  matrix JK16[`i',3]=r(t)
  matrix JK16[`i',4]=r(p)
}
preserve
  clear
  svmat double JK16, names(col)
  qui summ tp, detail
  matrix S34[4,1]=r(min)
  matrix S34[5,1]=r(p50)
  matrix S34[6,1]=r(max)
  qui count if p<0.05 & !missing(p)
  matrix S34[7,1]=r(N)
  qui summ t
  matrix S34[8,1]=r(min)
  di as result _n "JK 2016 per-village (sess tp t p):"
  list sess tp t p, sep(0) noobs
restore

* ---- 2022 reciprocity ----
qui reg z_recip windspeed_predicted windspeed_sqr $imbalances2 if particip_12_16_22==1, vce(cluster session)
qui utest windspeed_predicted windspeed_sqr
matrix S34[1,2]=r(extr)
matrix S34[2,2]=r(t)
matrix S34[3,2]=r(p)
matrix JK22 = J(30,4,.)
matrix colnames JK22 = sess tp t p
local i=0
foreach s of local sess {
  local ++i
  cap qui reg z_recip windspeed_predicted windspeed_sqr $imbalances2 if particip_12_16_22==1 & session!=`s', vce(cluster session)
  if _rc continue
  cap qui utest windspeed_predicted windspeed_sqr
  if _rc continue
  matrix JK22[`i',1]=`s'
  matrix JK22[`i',2]=r(extr)
  matrix JK22[`i',3]=r(t)
  matrix JK22[`i',4]=r(p)
}
preserve
  clear
  svmat double JK22, names(col)
  drop if missing(tp)
  qui summ tp, detail
  matrix S34[4,2]=r(min)
  matrix S34[5,2]=r(p50)
  matrix S34[6,2]=r(max)
  qui count if p<0.05 & !missing(p)
  matrix S34[7,2]=r(N)
  qui summ t
  matrix S34[8,2]=r(min)
  di as result _n "JK 2022 per-village (sess tp t p):"
  list sess tp t p, sep(0) noobs
restore

matrix list S34, format(%9.3f)
esttab matrix(S34, fmt(%9.2f)) using "${working_ANALYSIS}/results/tables/tableS34_jackknife.tex", ///
    mlabels(none) collabels("Transfers (2016)" "Reciprocity (2022)") nonumbers ///
    varlabels(full_TP "Turning point, full sample (kn)" ///
              full_Utest_t "Lind-Mehlum U-test (t)" ///
              full_Utest_p "U-test p-value" ///
              jk_min_TP "Jackknife turning point: minimum (kn)" ///
              jk_med_TP "Jackknife turning point: median (kn)" ///
              jk_max_TP "Jackknife turning point: maximum (kn)" ///
              jk_n_sig30 "Leave-one-out fits significant (of 30)" ///
              jk_min_t "Jackknife U-test t: minimum") ///
    booktabs replace
di as result _n "TABLE S34 DONE"

* ============================================================================
* Table S35 -- Lind-Mehlum U-test detail + bootstrap turning-point CIs
*   rows: N, slope_low, slope_high, Utest_t, Utest_p, TP, TPd_lo, TPd_hi, TPb_lo, TPb_hi
*   cols: Transfers2016, Reciprocity2022, Lab, AmbiguityGap
* ============================================================================
matrix S35 = J(10, 4, .)
matrix rownames S35 = N slope_low slope_high Utest_t Utest_p TP TP_lo_delta TP_hi_delta TP_lo_boot TP_hi_boot
matrix colnames S35 = Transfers2016 Reciprocity2022 Lab AmbiguityGap

* helper: fill column `c' from current active reg (terms lin=`l', sq=`q') + bootstrap spec is run separately
* ---- col 1: transfers 2016 ----
qui reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
matrix S35[1,1]=e(N)
qui utest windspeed_predicted windspeed_sqr
matrix S35[2,1]=r(s_l)
matrix S35[3,1]=r(s_u)
matrix S35[4,1]=r(t)
matrix S35[5,1]=r(p)
matrix S35[6,1]=r(extr)
qui nlcom (tp: -_b[windspeed_predicted]/(2*_b[windspeed_sqr]))
matrix b=r(b)
matrix V=r(V)
matrix S35[7,1]=b[1,1]-1.96*sqrt(V[1,1])
matrix S35[8,1]=b[1,1]+1.96*sqrt(V[1,1])
* clear tsset so cluster-resampling (duplicated villages) does not trip the panel check
tsset, clear
qui bootstrap tp=(-_b[windspeed_predicted]/(2*_b[windspeed_sqr])), cluster(session) reps(1000) seed(12345) nodots saving("`INT'/boot_t.dta", replace): ///
    reg d_mean_transfer windspeed_predicted windspeed_sqr L4_mean_transfer d_exp_transfer $imbalances

* ---- col 2: reciprocity 2022 ----
qui reg z_recip windspeed_predicted windspeed_sqr $imbalances2 if particip_12_16_22==1, vce(cluster session)
matrix S35[1,2]=e(N)
qui utest windspeed_predicted windspeed_sqr
matrix S35[2,2]=r(s_l)
matrix S35[3,2]=r(s_u)
matrix S35[4,2]=r(t)
matrix S35[5,2]=r(p)
matrix S35[6,2]=r(extr)
qui nlcom (tp: -_b[windspeed_predicted]/(2*_b[windspeed_sqr]))
matrix b=r(b)
matrix V=r(V)
matrix S35[7,2]=b[1,1]-1.96*sqrt(V[1,1])
matrix S35[8,2]=b[1,1]+1.96*sqrt(V[1,1])
qui bootstrap tp=(-_b[windspeed_predicted]/(2*_b[windspeed_sqr])), cluster(session) reps(1000) seed(12345) nodots saving("`INT'/boot_r.dta", replace): ///
    reg z_recip windspeed_predicted windspeed_sqr $imbalances2 if particip_12_16_22==1

* ---- col 3: lab (separate dataset, non-clustered) ----
preserve
  use "${working_ANALYSIS}/data/litfe_u_shape.dta", clear
  gen damages_sqr = damages^2
  qui reg rel_trans100 damages damages_sqr i.BarangayID, robust
  matrix S35[1,3]=e(N)
  qui utest damages damages_sqr
  matrix S35[2,3]=r(s_l)
  matrix S35[3,3]=r(s_u)
  matrix S35[4,3]=r(t)
  matrix S35[5,3]=r(p)
  matrix S35[6,3]=r(extr)
  qui nlcom (tp: -_b[damages]/(2*_b[damages_sqr]))
  matrix b=r(b)
  matrix V=r(V)
  matrix S35[7,3]=b[1,1]-1.96*sqrt(V[1,1])
  matrix S35[8,3]=b[1,1]+1.96*sqrt(V[1,1])
  qui bootstrap tp=(-_b[damages]/(2*_b[damages_sqr])), reps(1000) seed(12345) nodots saving("`INT'/boot_lab.dta", replace): ///
      reg rel_trans100 damages damages_sqr i.BarangayID
restore

* ---- col 4: ambiguity gap (village level) ----
preserve
  keep if y16==1
  collapse (mean) share_need_aid share_severe_damage windspeed_predicted, by(session)
  gen gap_pp = (share_need_aid - share_severe_damage)*100
  gen ws = windspeed_predicted
  gen ws2 = ws^2
  qui reg gap_pp ws ws2, robust
  matrix S35[1,4]=e(N)
  qui utest ws ws2
  matrix S35[2,4]=r(s_l)
  matrix S35[3,4]=r(s_u)
  matrix S35[4,4]=r(t)
  matrix S35[5,4]=r(p)
  matrix S35[6,4]=r(extr)
  qui nlcom (tp: -_b[ws]/(2*_b[ws2]))
  matrix b=r(b)
  matrix V=r(V)
  matrix S35[7,4]=b[1,1]-1.96*sqrt(V[1,1])
  matrix S35[8,4]=b[1,1]+1.96*sqrt(V[1,1])
  qui bootstrap tp=(-_b[ws]/(2*_b[ws2])), reps(1000) seed(12345) nodots saving("`INT'/boot_gap.dta", replace): ///
      reg gap_pp ws ws2
restore

* percentile bootstrap turning-point CIs (robust to the weak-curvature outliers that make a
* normal-approx CI explode; a CI spilling past the 69-145 kn sample range flags a weakly
* identified turning point, as for the 2022 reciprocity).
preserve
  use "`INT'/boot_t.dta", clear
  centile tp, centile(2.5 97.5)
  matrix S35[9,1]=r(c_1)
  matrix S35[10,1]=r(c_2)
  use "`INT'/boot_r.dta", clear
  centile tp, centile(2.5 97.5)
  matrix S35[9,2]=r(c_1)
  matrix S35[10,2]=r(c_2)
  use "`INT'/boot_lab.dta", clear
  centile tp, centile(2.5 97.5)
  matrix S35[9,3]=r(c_1)
  matrix S35[10,3]=r(c_2)
  use "`INT'/boot_gap.dta", clear
  centile tp, centile(2.5 97.5)
  matrix S35[9,4]=r(c_1)
  matrix S35[10,4]=r(c_2)
restore

matrix list S35, format(%9.3f)
esttab matrix(S35, fmt(%9.2f)) using "${working_ANALYSIS}/results/tables/tableS35_utest_detail.tex", ///
    mlabels(none) collabels("Transfers (2016)" "Reciprocity (2022)" "Lab" "Ambiguity gap") nonumbers ///
    varlabels(N "Observations" ///
              slope_low "Slope at lower wind-speed bound" ///
              slope_high "Slope at upper wind-speed bound" ///
              Utest_t "Lind-Mehlum U-test (t)" ///
              Utest_p "U-test p-value" ///
              TP "Turning point" ///
              TP_lo_delta "Turning-point CI, delta (lower)" ///
              TP_hi_delta "Turning-point CI, delta (upper)" ///
              TP_lo_boot "Turning-point CI, percentile bootstrap (lower)" ///
              TP_hi_boot "Turning-point CI, percentile bootstrap (upper)") ///
    booktabs replace
di as result _n "TABLE S35 DONE"

* ============================================================================
* Table S36 -- balance along the wind-speed gradient (2012 baseline)
* ============================================================================
global balance1 mean_transfer exp_transfer a3gov wealth_index age gender hh_head single hh_size edu_1
iebaltab $balance1 if year==2012 & returner==1, vce(cluster session) grpvar(ws_cat3) ftest fmissok rowvarlabels ///
    format(%9.2f) savetex("${working_ANALYSIS}/results/tables/tableS36_balance_windgradient") replace
* continuous-gradient joint orthogonality: does wind speed jointly predict baseline covariates?
qui reg windspeed_predicted $balance1 if year==2012 & returner==1, cluster(session)
testparm $balance1
di as result "Continuous wind-gradient joint orthogonality F-test: F=" %6.3f r(F) "  p=" %6.4f r(p)
di as result _n "TABLE S36 DONE"

** EOF
