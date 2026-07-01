/*==============================================================================
Project:     Moderate tropical cyclone exposure erodes solidarity needed for recovery
File:        02_analysis_main.do
Purpose:     Reproduce main-manuscript results produced by this script
Input:       analysis_rdy.dta, litfe_u_shape.dta, windspeed_predicted.dta
Output:      Figure 3 (mechanisms)
Authors:     Steimanis, Landmann
Date:        Created: 2024 | Last modified: June 2026


CONTENTS:
    Data setup  (returner panel + controls)
    Figure 3    Lab-in-the-field: diffusion of responsibility + field mechanisms

NOTE on the other main-text figures:
    Figure 1 (global exposure) and Figure 4 (study sites) are produced by the
    R scripts (scripts/R/11_combined_main_figure.R and
    scripts/R/12_study_site_figure.R). Figure 2 (field U-shape, wind-speed axis)
    and Figure 5 (damage validation, wind-speed axis) are produced by
    07_manuscript_figures.do. The SI forest plot (Fig. S2) is produced by
    03_analysis_SI.do.
==============================================================================*/

*-------------------------------------------------------------------------
** Data setup
*-------------------------------------------------------------------------
*Load dataset
use "$working_ANALYSIS/processed/analysis_rdy.dta", replace

* select balanced sample
drop if returner== 0
sort panel_id year

*Setup for regression analysis
global imbalances age gender hh_head single edu_1 stranger     // Time-invariant controls (attrition/selection)
global imbalances2 age gender hh_head single high_educ stranger // Alternative education specification


*-------------------------------------------------------------------------
**Figure 3.  Diffusion of responsibility + field mechanisms
*-------------------------------------------------------------------------
preserve
use "$working_ANALYSIS/data/litfe_u_shape.dta", clear


* Panel A:
eststo main1: reg rel_trans100 c.damages##c.damages i.BarangayID , robust
testparm c.damages##c.damages
*critique that turning point is outside the sample range not justified, see:
nlcom (x_star: -_b[damages] / (2*_b[c.damages#c.damages]))

summ damages if e(sample)
local xmin = r(min)
local xmax = r(max)

nlcom (in_range_min: (-_b[damages] / (2*_b[c.damages#c.damages])) - `xmin')

nlcom (in_range_max: `xmax' - (-_b[damages] / (2*_b[c.damages#c.damages])))

* marginal effect (slope) of distance across the range
margins, vce(unconditional) dydx(damages) at(damages=(0(10)380)) vsquish
marginsplot, yline(0) ///
    title("Marginal effect of damages on Î” transfers") ///
    xtitle("Damages") ytitle("dy/dx (PHP per damage)")
	
*how big is the dip in solidarity?
margins, at(damages=(0 220 380)) vsquish post

* Differences: min vs ends
lincom _b[2._at] - _b[1._at]   // y(x*) - y(0)
lincom _b[2._at] - _b[3._at]   // y(x*) - y(119)


mylabels 0(10)60, myscale(@) local(pctlabel) suffix("%") 
reg rel_trans100 c.damages##c.damages i.BarangayID, robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(damages=(0(50)400))
marginsplot, ///
    title("{bf:a} Experiment: Short-term effects", size(large)) ///
    ytitle("Transfers (% of endowment)", size(medium)) ///
    xtitle("Group damage severity", size(medium)) ///
	xline(220, lpattern(dash) lcolor(cranberry) lwidth(thin)) ///
    ylabel(`pctlabel', nogrid labsize(medsmall) angle(0)) ///
    xlabel(, nogrid labsize(medsmall)) ///
    legend(off) ///
    recastci(rarea) ///
    ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(none)) ///
    plotregion(margin(small)) ///
    legend(off) ///
    text(30 220 "Turning point @220 damage (95% CI [180, 260])", ///
    size(medsmall) color(cranberry)  just(right)) 
gr save "$working_ANALYSIS/results/intermediate/figure3_lab_short.gph", replace


* Panel B:
*first, generate predictions for the no diffusion vs. diffusion points
reg rel_trans100 c.damages i.BarangayID if  (two_med==1 | one_cat==1), robust
margins, at(damages=(180 200))
eststo diffusion: reg rel_trans100 one_cat i.BarangayID if  (two_med==1 | one_cat==1), robust
estimates table diffusion, p keep(one_cat)
* --> clearly lower transfers with 1 catastrophic vs. 2 medium shocks (diffusion of responsibility)


mylabels 0(10)60, myscale(@) local(pctlabel) suffix("%") 
eststo main1: reg rel_trans100 c.damages##c.damages i.BarangayID , robust
testparm c.damages##c.damages
estadd local Fstat = cond(r(p)<0.01,"`:di %5.3f `=r(F)''***", ///
cond(r(p)<0.05,"`:di %5.3f `=r(F)''**", ///
cond(r(p)<0.1,"`:di %5.3f `=r(F)''*",  "`:di %5.3f `=r(F)''")))
margins, at(damages=(0(50)400))

marginsplot, ///
    title("{bf:b} Experiment: Diffusion of responsibility", size(large)) ///
    ytitle("Transfers (% of endowment)", size(medium)) ///
    xtitle("Group damage severity", size(medium)) ///
    ylabel(`pctlabel', nogrid labsize(medsmall) angle(0)) ///
    xlabel(, nogrid labsize(medsmall)) ///
    legend(off) ///
    recastci(rarea) ///
    ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(none)) ///
    plotregion(margin(small)) ///
    text(45 80 "Low diffusion (1 helper): 20% transferred" ///
                "High diffusion (2 helpers): 2% transferred" ///
                " " ///
                "Difference: -18pp [-33, -2]" ///
                "{it:p} = 0.03", ///
         place(e) size(medsmall) ///
         box just(left) margin(medium) ///
         bexpand bcolor(black) fcolor(white) lwidth(medthick)) ///
    text(40 74 "â—", color(dkgreen) size(large)) ///
    text(35 74 "â—", color(orange) size(large)) ///
    addplot(scatteri 19.82 200, ///
            msymbol(O) msize(vlarge) ///
            mcolor(dkgreen) mlwidth(thick) ///
         || scatteri 2.05 180, ///
            msymbol(O) msize(vlarge) ///
            mcolor(orange) mlwidth(thick))
gr_edit plotregion1.textbox1.DragBy -3.644668180288999 -14.29731343192757
gr_edit plotregion1.textbox1.DragBy 0 4.448053067710799
gr_edit plotregion1.textbox1.DragBy -.0867778138164193 -3.177180762650572
gr_edit plotregion1.textbox2.DragBy -.7810003243476455 -1.588590381325299
gr_edit plotregion1.textbox3.DragBy -.7810003243476525 -1.270872305060242
gr_edit plotregion1.plot3.style.editstyle marker(size(medium)) editcopy
gr_edit plotregion1.plot4.style.editstyle marker(size(medium)) editcopy 
gr save "$working_ANALYSIS/results/intermediate/figure3_lab_diffusion.gph", replace

restore


*Panel C: Information asymmetry
*identify severely affected respondents:  We define severe damage as total asset loss exceeding 4500 PHP, a value approximating one month of average pre-disaster household income in our sample, accounting for inflation between our 2012 (mean=3800 PHP) survey and the 2013 typhoon.

*TWO LPOLY LINES + SHADED AMBIGUITY-GAP BAND on the CALIBRATED WIND-SPEED axis:
*stated aid need vs. severe damage (2016-restricted village shares from 01_clean_data.do, computed over
*ALL 2016 respondents); the shaded band between the lpoly fits is the ambiguity gap. One double-headed
*arrow per Saffir-Simpson category, anchored at the CATEGORY MEANS (returners, nonmissing both vars),
*labeled with the category-mean gap in pp (= share unverifiable claims, since severely damaged
*non-claimants are ~0.4%). Sample: RETURNER panel (drop if returner==0 in the data setup above).
*bwidth(6) on the windspeed span = smoothing-equivalent of bwidth(10) on reversed distance.

*Merge calibrated wind speed (village-level fill)
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws

cap drop pct_need_aid pct_severe_damage _grid _fit_need _fit_sev ss_cat
gen pct_need_aid = share_need_aid*100
gen pct_severe_damage = share_severe_damage*100
gen _grid = 69 + _n if _n <= 81
lpoly pct_need_aid windspeed_predicted if y16==1, bwidth(6) gen(_fit_need) at(_grid) nograph
lpoly pct_severe_damage windspeed_predicted if y16==1, bwidth(6) gen(_fit_sev) at(_grid) nograph
gen ss_cat = 1 if windspeed_predicted < 83
replace ss_cat = 2 if windspeed_predicted >= 83  & windspeed_predicted < 96
replace ss_cat = 3 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace ss_cat = 4 if windspeed_predicted >= 113 & windspeed_predicted < 137
replace ss_cat = 5 if windspeed_predicted >= 137 & !missing(windspeed_predicted)
qui summ windspeed_predicted if y16==1
local xlo1 = r(min) - 1
local xhi5 = r(max) + 1
local xhi1 = 83
local xlo2 = 83
local xhi2 = 96
local xlo3 = 96
local xhi3 = 113
local xlo4 = 113
local xhi4 = 137
local xlo5 = 137
forval c = 1/5 {
	qui summ h6need if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
	local need`c' = 100*r(mean)
	qui summ severely_damaged if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
	local gap`c' : di %2.0f `need`c'' - 100*r(mean)
	local xm`c' = round((`xlo`c'' + `xhi`c'')/2)
}
*Legend sits below the plot so the Cat 3 arrow can sit at the category midpoint.
*Anchor the arrows at the lpoly fits at each arrow's wind speed so they span the shaded
*band exactly (labels remain the CATEGORY-MEAN gaps; fit-at-point and category mean
*differ by <3pp, invisible at print size)
forval c = 1/5 {
	qui summ _fit_need if _grid==`xm`c'', meanonly
	local ya`c' = r(mean)
	qui summ _fit_sev if _grid==`xm`c'', meanonly
	local yb`c' = r(mean)
	local ym`c' = (`ya`c'' + `yb`c'')/2
}
twoway ///
	(rarea _fit_need _fit_sev _grid, color("120 94 240%20") lwidth(none)) ///
	(line _fit_need _grid, lcolor("100 143 255") lwidth(medthick)) ///
	(line _fit_sev _grid, lcolor("220 38 127") lwidth(medthick) lpattern(dash)) ///
	(pcarrowi `ym1' `xm1' `ya1' `xm1', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym1' `xm1' `yb1' `xm1', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym2' `xm2' `ya2' `xm2', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym2' `xm2' `yb2' `xm2', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym3' `xm3' `ya3' `xm3', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym3' `xm3' `yb3' `xm3', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym4' `xm4' `ya4' `xm4', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym4' `xm4' `yb4' `xm4', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym5' `xm5' `ya5' `xm5', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	(pcarrowi `ym5' `xm5' `yb5' `xm5', lcolor(gs4) mcolor(gs4) lwidth(medthin) msize(medsmall)) ///
	, xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12)) ///
	text(`=`ym1'+4' `=`xm1'+1' "`gap1' pp", place(e) size(medsmall) color(black)) ///
	text(`=`ym2'+4' `=`xm2'+1' "`gap2' pp", place(e) size(medsmall) color(black)) ///
	text(`=`ym3'+4' `=`xm3'+1' "`gap3' pp", place(e) size(medsmall) color(black)) ///
	text(`=`ym4'+4' `=`xm4'+1' "`gap4' pp", place(e) size(medsmall) color(black)) ///
	text(`=`ym5'+4' `=`xm5'+1' "`gap5' pp", place(e) size(medsmall) color(black)) ///
	title("{bf:c} Field data: Ambiguity gap", size(large)) ///
	xtitle("Sustained wind speed (knots)", size(medium)) ///
	ytitle("Share (%)", size(medium)) ///
	yla(0(20)100, nogrid labsize(medsmall) angle(0)) ///
	xla(70(20)150, labsize(medsmall) nogrid) ///
	legend(order(2 "Need for aid" 3 "Severe damage" 1 "Ambiguity gap") rows(1) size(vsmall) symxsize(5) keygap(1) ring(1) pos(6)) ///
	note("") xsize(2) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/damage_aid_asymmetry.gph", replace


*Panel D: Aid satisfaction by actual severe damage (wind-speed axis)
ttest damage_TOTAL if year==2016, by(h2house)
* those who experienced damage to their house report significantly higher total damages, i.e. damage to house was the major damage people experienced by haiyan
reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted  age gender hh_head single edu_1 if severely_damaged==0, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
qui count if e(sample)
local nNo = r(N)
margins, at(windspeed_predicted = (70(5)145)) saving("$working_ANALYSIS/results/intermediate/aid_satisfaction_no_house_damage.gph", replace)
nlcom (x_star: -_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted]))
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

eststo house_aid2: reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted  age gender hh_head single edu_1 if severely_damaged==1, vce(cluster session)
testparm c.windspeed_predicted##c.windspeed_predicted
qui count if e(sample)
local nSev = r(N)
margins, at(windspeed_predicted = (70(5)145)) saving("$working_ANALYSIS/results/intermediate/aid_satisfaction_house_damage.gph", replace)
nlcom (x_star: -_b[windspeed_predicted] / (2*_b[c.windspeed_predicted#c.windspeed_predicted]))
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

* sample-mean reference line computed, never hardcoded (analysis_rdy is rebuilt upstream)
qui summ satisfaction_aid_norm
local mean_aidsat = r(mean)
combomarginsplot "$working_ANALYSIS/results/intermediate/aid_satisfaction_no_house_damage.gph" "$working_ANALYSIS/results/intermediate/aid_satisfaction_house_damage.gph" , title("{bf:d} Field data: Aid satisfaction", size(large)) labels("No severe damage (n=`nNo')" "Severe damage (n=`nSev')", size(medium))  xtitle("Sustained wind speed (knots)", size(medium))  yline(`mean_aidsat', lpattern(solid))  xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))  xla(70(20)150, nogrid labsize(medsmall))  yla(0(20)100, nogrid labsize(medsmall) format(%9.0g)) noci  ytitle("Score", size(medium)) recastci(rarea) ciopts(lw(none) fcolor(%50)) legend(rows(2) size(medium)) xsize(2) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/damage_aid_satisfaction.gph", replace

gr combine "$working_ANALYSIS/results/intermediate/figure3_lab_short.gph" "$working_ANALYSIS/results/intermediate/figure3_lab_diffusion.gph" "$working_ANALYSIS/results/intermediate/damage_aid_asymmetry.gph" "$working_ANALYSIS/results/intermediate/damage_aid_satisfaction.gph", xsize(4) ysize(3.465) rows(2)

gr_edit plotregion1.graph1.plotregion1.textbox1.DragBy 15.72863884998703 11.41647185568468
gr_edit style.editstyle declared_ysize(4) editcopy
gr_edit style.editstyle declared_xsize(3) editcopy
gr_edit style.editstyle declared_ysize(3) editcopy
gr_edit style.editstyle declared_xsize(4) editcopy
gr_edit plotregion1.graph1.plotregion1.textbox1.DragBy -.2422210382898441 2.897109089073254
gr_edit plotregion1.graph1.plotregion1.textbox1.DragBy 0 9.657030296910632
gr_edit plotregion1.graph2.plotregion1.textbox1.DragBy 0 -15.45124847505753
gr_edit plotregion1.graph2.plotregion1.textbox1.DragBy .9688841531591125 -24.14257574227729
gr_edit plotregion1.graph2.plotregion1.textbox1.style.editstyle color(none) editcopy
gr_edit plotregion1.graph2.plotregion1.textbox1.style.editstyle color(black) editcopy
gr_edit plotregion1.graph2.plotregion1.textbox1.style.editstyle drawbox(no) editcopy
gr_edit plotregion1.graph2.plotregion1.textbox3.DragBy 14.04882022080842 -34.76530906887906
gr_edit plotregion1.graph2.plotregion1.textbox2.DragBy 13.56437814422875 -33.79960603918752
gr_edit plotregion1.graph2.plotregion1.textbox2.DragBy .4844420765796881 0
gr_edit plotregion1.graph2.plotregion1.textbox2.DragBy .4844420765795562 0
gr_edit plotregion1.graph2.plotregion1.textbox2.DragBy 0 -1.93140605938239
gr_edit plotregion1.graph2.plotregion1.textbox2.DragBy 0 1.93140605938239
gr_edit plotregion1.graph2.xaxis1.title.DragBy -1.555555555555543 -.1111111111110858
gr_edit plotregion1.graph2.plotregion1.textbox3.DragBy .2422210382898441 2.897109089072859
gr_edit plotregion1.graph2.plotregion1.textbox3.DragBy 0 -1.931406059381994
gr_edit plotregion1.graph2.plotregion1.textbox1.DragBy 0 1.931406059381994
gr_edit plotregion1.graph2.plotregion1.textbox1.DragBy 0 1.931406059381994
gr_edit plotregion1.graph2.plotregion1.textbox1.DragBy 0 1.931406059382521
gr_edit plotregion1.graph4.legend.Edit , style(rows(1)) style(cols(0)) keepstyles 
gr_edit plotregion1.graph4.legend.Edit, style(labelstyle(color(custom)))
gr_edit plotregion1.graph4.legend.plotregion1.label[1].style.editstyle size(medsmall) editcopy
gr_edit plotregion1.graph4.legend.plotregion1.label[2].style.editstyle size(medsmall) editcopy
gr_edit plotregion1.graph4.legend.plotregion1.key[1].DragBy .1111111111111086 -7.888888888888864
gr_edit plotregion1.graph4.legend.plotregion1.label[1].DragBy -.1111111111111124 -8.333333333333332
gr save "$working_ANALYSIS/results/intermediate/figure3_mechanism.gph",replace


* ==============================================================================
* Export Figure 3 panel data to CSV for the NCC R rebuild
*   (scripts/R/13_figure2_3_maintext.R re-plots these series; the analysis here
*    stays authoritative). Self-contained: re-loads data and re-runs the exact
*    panel models. Do NOT add `log close` here -- this runs inside run.do's log.
* ==============================================================================
local INT "$working_ANALYSIS/results/intermediate"

* --- Panels A & B: lab-in-the-field U-shape (litfe_u_shape.dta) ---
use "$working_ANALYSIS/data/litfe_u_shape.dta", clear
reg rel_trans100 c.damages##c.damages i.BarangayID, robust
margins, at(damages=(0(50)400)) saving("`INT'/_m3ab.dta", replace)
preserve
    use "`INT'/_m3ab.dta", clear
    keep _at1 _margin _ci_lb _ci_ub
    rename (_at1 _margin _ci_lb _ci_ub) (damages est lo hi)
    export delimited "`INT'/fig3_panelAB_curve.csv", replace
restore

* Panel B annotated diffusion points (predicted transfers at damages 180/200)
reg rel_trans100 c.damages i.BarangayID if (two_med==1 | one_cat==1), robust
margins, at(damages=(180 200)) saving("`INT'/_m3bpts.dta", replace)
preserve
    use "`INT'/_m3bpts.dta", clear
    keep _at1 _margin
    rename (_at1 _margin) (damages est)
    export delimited "`INT'/fig3_panelB_points.csv", replace
restore

* Diffusion difference (1 catastrophic vs 2 medium shocks): one_cat coefficient
reg rel_trans100 one_cat i.BarangayID if (two_med==1 | one_cat==1), robust
local diff    = _b[one_cat]
local diff_se = _se[one_cat]
local diff_df = e(df_r)
local diff_lo = `diff' - invttail(`diff_df',0.025)*`diff_se'
local diff_hi = `diff' + invttail(`diff_df',0.025)*`diff_se'
local diff_p  = 2*ttail(`diff_df', abs(`diff'/`diff_se'))

* --- Panels C & D: returner field panel + calibrated wind speed ---
use "$working_ANALYSIS/processed/analysis_rdy.dta", clear
drop if returner== 0
sort panel_id year
merge m:1 session year using "$working_ANALYSIS/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws

* Panel C: ambiguity gap (two lpoly fits + category-mean gaps)
cap drop pct_need_aid pct_severe_damage _grid _fit_need _fit_sev ss_cat
gen pct_need_aid = share_need_aid*100
gen pct_severe_damage = share_severe_damage*100
gen _grid = 69 + _n if _n <= 81
lpoly pct_need_aid windspeed_predicted if y16==1, bwidth(6) gen(_fit_need) at(_grid) nograph
lpoly pct_severe_damage windspeed_predicted if y16==1, bwidth(6) gen(_fit_sev) at(_grid) nograph
gen ss_cat = 1 if windspeed_predicted < 83
replace ss_cat = 2 if windspeed_predicted >= 83  & windspeed_predicted < 96
replace ss_cat = 3 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace ss_cat = 4 if windspeed_predicted >= 113 & windspeed_predicted < 137
replace ss_cat = 5 if windspeed_predicted >= 137 & !missing(windspeed_predicted)
qui summ windspeed_predicted if y16==1
local xlo1 = r(min) - 1
local xhi1 = 83
local xlo2 = 83
local xhi2 = 96
local xlo3 = 96
local xhi3 = 113
local xlo4 = 113
local xhi4 = 137
local xlo5 = 137
local xhi5 = r(max) + 1
forval c = 1/5 {
    qui summ h6need if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
    local need`c' = 100*r(mean)
    qui summ severely_damaged if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
    local gap`c' = `need`c'' - 100*r(mean)
    local xm`c' = round((`xlo`c'' + `xhi`c'')/2)
}
forval c = 1/5 {
    qui summ _fit_need if _grid==`xm`c'', meanonly
    local ya`c' = r(mean)
    qui summ _fit_sev if _grid==`xm`c'', meanonly
    local yb`c' = r(mean)
    local ym`c' = (`ya`c'' + `yb`c'')/2
}
preserve
    keep if !missing(_grid)
    keep _grid _fit_need _fit_sev
    rename (_grid _fit_need _fit_sev) (grid fit_need fit_sev)
    export delimited "`INT'/fig3_panelC_lines.csv", replace
restore
preserve
    tempname pf
    postfile `pf' cat xm gap ya yb ym using "`INT'/_fig3_gaps.dta", replace
    forval c = 1/5 {
        post `pf' (`c') (`xm`c'') (`gap`c'') (`ya`c'') (`yb`c'') (`ym`c'')
    }
    postclose `pf'
    use "`INT'/_fig3_gaps.dta", clear
    export delimited "`INT'/fig3_panelC_gaps.csv", replace
restore

* Panel D: aid satisfaction by actual severe damage (wind-speed axis)
reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted age gender hh_head single edu_1 if severely_damaged==0, vce(cluster session)
qui count if e(sample)
local nNo = r(N)
margins, at(windspeed_predicted = (70(5)145)) saving("`INT'/_m3d_no.dta", replace)
reg satisfaction_aid_norm c.windspeed_predicted##c.windspeed_predicted age gender hh_head single edu_1 if severely_damaged==1, vce(cluster session)
qui count if e(sample)
local nSev = r(N)
margins, at(windspeed_predicted = (70(5)145)) saving("`INT'/_m3d_sev.dta", replace)
qui summ satisfaction_aid_norm
local mean_aidsat = r(mean)
preserve
    use "`INT'/_m3d_no.dta", clear
    keep _at1 _margin _ci_lb _ci_ub
    gen grp = "No severe damage"
    rename (_at1 _margin _ci_lb _ci_ub) (ws est lo hi)
    tempfile dno
    save `dno'
    use "`INT'/_m3d_sev.dta", clear
    keep _at1 _margin _ci_lb _ci_ub
    gen grp = "Severe damage"
    rename (_at1 _margin _ci_lb _ci_ub) (ws est lo hi)
    append using `dno'
    export delimited "`INT'/fig3_panelD_curves.csv", replace
restore

* Annotation scalars (panel-d meta + diffusion stats)
preserve
    clear
    set obs 1
    gen mean_aidsat = `mean_aidsat'
    gen nNo  = `nNo'
    gen nSev = `nSev'
    gen diff    = `diff'
    gen diff_lo = `diff_lo'
    gen diff_hi = `diff_hi'
    gen diff_p  = `diff_p'
    export delimited "`INT'/fig3_scalars.csv", replace
restore



* ============================================================================
* Merged from 07_manuscript_figures.do: Figure 2 & Figure 5 models, the quoted
* main-text numbers, and the CSV exports the R rebuilds read. Stata no longer
* exports the redundant main-figure PNGs (Figs 2/3/5 are rendered in R).
* ============================================================================
use "${working_ANALYSIS}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${working_ANALYSIS}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year
gen windspeed_sqr = windspeed_predicted^2

global imbalances  age gender hh_head single edu_1 stranger
global imbalances2 age gender hh_head single edu_1   // 2022 set (no stranger/high_educ)
local sscat 83 96 113 137

* ==============================================================================
* (1) NUMBERS: transfers U-shape (Model 2: l4 + d_exp_transfer + imbalances)
* ==============================================================================
di _n(2) "===== TRANSFERS U-SHAPE (windspeed, Model 2) ====="
reg d_mean_transfer windspeed_predicted windspeed_sqr l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
utest windspeed_predicted windspeed_sqr

reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
local tpA = -_b[windspeed_predicted]/(2*_b[c.windspeed_predicted#c.windspeed_predicted])
di "Turning point A: " %5.1f `tpA' " kn"
nlcom (tp: -_b[windspeed_predicted]/(2*_b[c.windspeed_predicted#c.windspeed_predicted]))

di _n "--- margins contrasts: trough vs sample endpoints (70 / 145 kn) ---"
margins, at(windspeed_predicted=(70 `=round(`tpA')' 145)) pwcompare(effects) vsquish

di _n "--- standardized outcome (SD units) ---"
reg d_mean_transfer_std c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
margins, at(windspeed_predicted=(70 `=round(`tpA')' 145)) pwcompare(effects) vsquish

* ---- Panel A: marginsplot ----
reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
margins, vce(unconditional) at(windspeed_predicted=(70(5)145)) vsquish saving("`INT'/_mA.dta", replace)
marginsplot, ///
    title("{bf:a} Solidarity transfers (3 years)", size(large)) ///
    xtitle("Sustained wind speed (knots)", size(medium)) ///
    ytitle("{&Delta} Transfers in PHP", size(medium)) ///
    yline(0, lpattern(solid) lcolor(gs10)) ///
    xline(`sscat', lpattern(shortdash) lcolor(gs13) lwidth(vthin)) ///
    xline(`tpA', lpattern(dash) lcolor(cranberry) lwidth(thin)) ///
    xla(70(20)150, nogrid labsize(medsmall)) ///
    yla(-15(5)5, nogrid labsize(medsmall) angle(0)) ///
    recastci(rarea) ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(none)) plotregion(margin(small)) legend(off) ///
    text(-13 `tpA' "Turning point" "{&asymp} `=round(`tpA')' kn (Cat 4)", ///
         size(medsmall) color(cranberry) place(e))
graph save "`INT'/fig2v4_a.gph", replace

* export panel A margins for the R rebuild (scripts/R/13_figure2_3_maintext.R)
preserve
    use "`INT'/_mA.dta", clear
    export delimited "`INT'/fig2_panelA_margins.csv", replace
restore

* ==============================================================================
* (2) NUMBERS + PANEL B: reciprocity 2022
* ==============================================================================
di _n(2) "===== RECIPROCITY 2022 (windspeed) ====="
reg z_recip windspeed_predicted windspeed_sqr $imbalances2 if particip_12_16_22==1, vce(cluster session)
utest windspeed_predicted windspeed_sqr

reg z_recip c.windspeed_predicted##c.windspeed_predicted $imbalances2 if particip_12_16_22==1, vce(cluster session)
local tpB = -_b[windspeed_predicted]/(2*_b[c.windspeed_predicted#c.windspeed_predicted])
di "Turning point B: " %5.1f `tpB' " kn"
nlcom (tp: -_b[windspeed_predicted]/(2*_b[c.windspeed_predicted#c.windspeed_predicted]))

di _n "--- margins: predicted reciprocity at 70 / trough / 145 kn + contrasts ---"
margins, at(windspeed_predicted=(70 `=round(`tpB')' 145)) vsquish
margins, at(windspeed_predicted=(70 `=round(`tpB')' 145)) pwcompare(effects) vsquish

margins, vce(unconditional) at(windspeed_predicted=(70(5)145)) vsquish saving("`INT'/_mB.dta", replace)
marginsplot, ///
    title("{bf:b} Reciprocity (9 years)", size(large)) ///
    xtitle("Sustained wind speed (knots)", size(medium)) ///
    ytitle("Reciprocity in SD", size(medium)) ///
    yline(0, lpattern(solid) lcolor(gs10)) ///
    xline(`sscat', lpattern(shortdash) lcolor(gs13) lwidth(vthin)) ///
    xline(`tpB', lpattern(dash) lcolor(cranberry) lwidth(thin)) ///
    xla(70(20)150, nogrid labsize(medsmall)) ///
    yla(-.5(.25).5, nogrid labsize(medsmall) angle(0)) ///
    recastci(rarea) ciopts(lw(none) fcolor(%30)) ///
    xsize(2) ysize(2) ///
    graphregion(color(none)) plotregion(margin(small)) legend(off) ///
    text(-.42 `tpB' "Turning point" "{&asymp} `=round(`tpB')' kn (Cat 3)", ///
         size(medsmall) color(cranberry) place(e))
graph save "`INT'/fig2v4_b.gph", replace

* export panel B margins for the R rebuild (scripts/R/13_figure2_3_maintext.R)
preserve
    use "`INT'/_mB.dta", clear
    export delimited "`INT'/fig2_panelB_margins.csv", replace
restore

* ==============================================================================
* (3) PANEL C: solidarity response by economic vulnerability (main-text Fig. 2c)
* PRIMARY test = continuous interaction of the wind-speed quadratic with the
* standardized baseline economic-vulnerability index vuln_cont (built in 01). The
* U-shape deepens with vulnerability; the interaction is suggestive, not conclusive
* (F(2,29)=2.45, p=0.10; see 03), so we frame it as directional. The R rebuild
* (scripts/R/13_figure2_3_maintext.R) draws predicted curves at low/average/high
* vulnerability (-1/0/+1 SD of the index). Tertiles are kept for the SI (Table S20).
* ==============================================================================
reg d_mean_transfer c.windspeed_predicted##c.windspeed_predicted##c.vuln_cont ///
    l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
testparm c.windspeed_predicted#c.vuln_cont c.windspeed_predicted#c.windspeed_predicted#c.vuln_cont
di "Fig2c continuous vulnerability interaction: F(" r(df) "," r(df_r) ") = " %5.2f r(F) ", p = " %5.3f r(p)
* predicted curves at vuln_cont = -1, 0, +1 SD for the R rebuild
capture postclose _pf
tempname _pf
postfile `_pf' str20 grp double(ws est lo hi) using "`INT'/_mC.dta", replace
foreach lv in -1 0 1 {
    local g = cond(`lv'==-1,"Less vulnerable", cond(`lv'==0,"Average","More vulnerable"))
    forvalues w = 70(5)145 {
        margins, at(windspeed_predicted=`w' vuln_cont=`lv') vce(unconditional)
        matrix _r = r(table)
        post `_pf' ("`g'") (`w') (_r[1,1]) (_r[5,1]) (_r[6,1])
    }
}
postclose `_pf'
preserve
    use "`INT'/_mC.dta", clear
    export delimited "`INT'/fig2_panelC_vuln.csv", replace
restore

* ---- combine main-text Figure 2 (windspeed): panels a + b ----
gr combine "`INT'/fig2v4_a.gph" "`INT'/fig2v4_b.gph", ///
    rows(1) xsize(4) ysize(2) scale(1.6)
* ==============================================================================
* (4) NUMBERS: ambiguity gap (corrected 2016-only shares; returner sample as in 02)
* ==============================================================================
di _n(2) "===== AMBIGUITY GAP (corrected, windspeed) ====="
preserve
keep if returner==1
cap drop ss_cat
gen ss_cat = 1 if windspeed_predicted < 83
replace ss_cat = 2 if windspeed_predicted >= 83  & windspeed_predicted < 96
replace ss_cat = 3 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace ss_cat = 4 if windspeed_predicted >= 113 & windspeed_predicted < 137
replace ss_cat = 5 if windspeed_predicted >= 137 & !missing(windspeed_predicted)

di _n "--- category means (y16, nonmissing both): need / severe / gap ---"
forval c = 1/5 {
    qui summ h6need if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
    local need`c' = 100*r(mean)
    qui summ severely_damaged if ss_cat==`c' & y16==1 & !missing(h6need) & !missing(severely_damaged), meanonly
    local sev`c' = 100*r(mean)
    di "Cat `c': need = " %4.0f `need`c'' "%, severe = " %4.0f `sev`c'' "%, gap = " %4.0f `=`need`c''-`sev`c''' " pp"
}

di _n "--- severely damaged non-claimants (FN) among y16 respondents ---"
qui count if y16==1 & severely_damaged==1 & h6need==0 & !missing(h6need) & !missing(severely_damaged)
local nFN = r(N)
qui count if y16==1 & !missing(h6need) & !missing(severely_damaged)
di "FN share: `nFN'/" r(N) " = " %5.1f 100*`nFN'/r(N) "%"

di _n "--- village-level gap quadratic on windspeed: peak + 95% CI ---"
keep if y16==1
collapse (mean) share_need_aid share_severe_damage windspeed_predicted, by(session)
gen gap_pp = (share_need_aid - share_severe_damage)*100
gen ws  = windspeed_predicted
gen ws2 = ws^2
reg gap_pp ws ws2, robust
utest ws ws2
reg gap_pp c.ws##c.ws, robust
nlcom (peak: -_b[ws]/(2*_b[c.ws#c.ws]))
restore

* ==============================================================================
* (5) NUMBERS: Methods exposure validation (per-10-kn effects)
* ==============================================================================
di _n(2) "===== METHODS: per-10-kn validation effects ====="
reg share_houses_damaged windspeed_predicted, cluster(session)
di "Houses damaged: +" %4.1f 100*10*_b[windspeed_predicted] " pp per 10 kn; R2 = " %5.3f e(r2)
qui summ mean_dmg
di "mean_dmg units check: mean = " %8.2f r(mean) " (1,000 PHP if ~10-20)"
reg mean_dmg windspeed_predicted if year<2022, cluster(session)
di "Damage costs: +" %6.0f 1000*10*_b[windspeed_predicted] " PHP per 10 kn"
reg share_need_aid windspeed_predicted, cluster(session)
di "Aid need: +" %4.1f 100*10*_b[windspeed_predicted] " pp per 10 kn"

* --- damage-validation panels (from 05_windspeed_damage_figure.do; .gph intermediates) ---
use "${working_ANALYSIS}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${working_ANALYSIS}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws

* Saffir-Simpson boundaries inside the village wind range (Cat2/3/4/5 lower bounds)
local ssline xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))
local xopt   xla(70(20)150, nogrid)
* bwidth 6 ~= original distance bw10 rescaled to the windspeed span (76 vs 120 km),
* so the wind-axis smoothing matches the original distance figure.
local common lineopts(lwidth(medthick)) msize(medlarge) mcolor(*.5) legend(off) ///
             bwidth(6) note("") scale(1.2) xsize(3.165) ysize(2) ///
             xtitle("Sustained wind speed (knots)")

* A: Houses damaged
lpoly share_houses_damaged windspeed_predicted, `ssline' `common' `xopt' ///
    title("{bf:a} Houses damaged") ytitle("Share") ///
    text(.05 76 "Cat 1", size(small) color(gs8)) text(.05 89.5 "Cat 2", size(small) color(gs8)) ///
    text(.05 104.5 "Cat 3", size(small) color(gs8)) text(.05 125 "Cat 4", size(small) color(gs8)) ///
    text(.05 141 "Cat 5", size(small) color(gs8)) ///
    yla(0 "0%" 0.2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%", nogrid)
gr save "${working_ANALYSIS}/results/intermediate/dmgws_a.gph", replace

* B: Costs of damages
lpoly mean_dmg windspeed_predicted if year<2022, `ssline' `common' `xopt' ///
    title("{bf:b} Costs of damages") ytitle("in 1,000 PHP") yla(0(5)35, nogrid)
gr save "${working_ANALYSIS}/results/intermediate/dmgws_b.gph", replace

* C: Reported need for help
lpoly share_need_aid windspeed_predicted, `ssline' `common' `xopt' ///
    title("{bf:c} Reported need for help") ytitle("Share") ///
    yla(0 "0%" 0.2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%", nogrid)
gr save "${working_ANALYSIS}/results/intermediate/dmgws_c.gph", replace

* D: Major life event in 2022
lpoly share_yolanda_2022 windspeed_predicted, `ssline' `common' `xopt' ///
    title("{bf:d} Major life event in 2022") ytitle("Share") ///
    yla(0 "0%" 0.2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%", nogrid)
gr save "${working_ANALYSIS}/results/intermediate/dmgws_d.gph", replace

di _n(2) "====== DAMAGE PANELS (windspeed) DONE ======"

* --- 6-panel damage-validation assembly (from 07; combined in memory, not exported) ---
* ==============================================================================
* (6) 6-panel Methods damage figure: A-D from 05's intermediates + E/F built here
*     (bwidth 6 = windspeed equivalent of distance bw 9-10)
* ==============================================================================
cap confirm file "`INT'/dmgws_a.gph"
if _rc {
    di as error "dmgws_a-d.gph missing - run 05_windspeed_damage_figure.do first"
    exit 601
}
cap drop iqr_damage IQR_DMG_ratio _dmg1000
* damage_TOTAL is raw PHP; mean_dmg is in 1,000 PHP. Rescale before the IQR so panel E
* is in 1,000 PHP and the F ratio is unitless (mirrors 02_analysis_main.do).
gen _dmg1000 = damage_TOTAL/1000
bys session: egen iqr_damage = iqr(_dmg1000)
gen IQR_DMG_ratio = iqr_damage / mean_dmg

local ssline xline(83 96 113 137, lwidth(0.2) lpattern(dash) lcolor(gs12))
local xopt   xla(70(20)150, nogrid)
local common lineopts(lwidth(medthick)) msize(medlarge) mcolor(*.5) legend(off) ///
             bwidth(6) note("") scale(1.2) xsize(3.165) ysize(2) ///
             xtitle("Sustained wind speed (knots)")

lpoly iqr_damage windspeed_predicted, `ssline' `common' `xopt' ///
    title("{bf:e} Damage inequality (IQR)") ytitle("in 1,000 PHP") yla(, nogrid)
gr save "`INT'/dmgws_e.gph", replace

lpoly IQR_DMG_ratio windspeed_predicted, `ssline' `common' `xopt' ///
    title("{bf:f} IQR/damage ratio") ytitle("Ratio") yla(, nogrid)
gr save "`INT'/dmgws_f.gph", replace

gr combine "`INT'/dmgws_a.gph" "`INT'/dmgws_b.gph" "`INT'/dmgws_c.gph" ///
           "`INT'/dmgws_d.gph" "`INT'/dmgws_e.gph" "`INT'/dmgws_f.gph", ///
           cols(3) xsize(3.465) ysize(2) graphregion(margin(tiny))

* ============================================================================
* Merged from _export_fig2_categorical.do: 3-/5-bin categorical overlays for Fig 2 (R)
* ============================================================================
global W : pwd
cap mkdir "${W}/results/intermediate"
clear
set more off
version 16

use "${W}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${W}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws
sort panel_id year
tsset panel_id year

global imbalances  age gender hh_head single edu_1 stranger
global imbalances2 age gender hh_head single edu_1

* --- bins of the calibrated wind speed (same axis as the continuous curve) ---
gen bin3 = 1 if windspeed_predicted < 96
replace bin3 = 2 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace bin3 = 3 if windspeed_predicted >= 113 & !missing(windspeed_predicted)

gen bin5 = 1 if windspeed_predicted < 83
replace bin5 = 2 if windspeed_predicted >= 83  & windspeed_predicted < 96
replace bin5 = 3 if windspeed_predicted >= 96  & windspeed_predicted < 113
replace bin5 = 4 if windspeed_predicted >= 113 & windspeed_predicted < 137
replace bin5 = 5 if windspeed_predicted >= 137 & !missing(windspeed_predicted)

* --- bin summaries: mean wind speed and number of villages per bin ---
foreach b in bin3 bin5 {
  preserve
    bysort session (year): keep if _n==1
    collapse (mean) ws=windspeed_predicted (count) n_vill=panel_id, by(`b')
    rename `b' bin
    drop if missing(bin)
    export delimited "${W}/results/intermediate/fig2_bins_`b'.csv", replace
  restore
}

* --- transfers (panel A): same controls/sample as the continuous spec ---
foreach b in bin3 bin5 {
  reg d_mean_transfer ib1.`b' l4.mean_transfer d_exp_transfer $imbalances, vce(cluster session)
  gen byte es = e(sample)
  margins i.`b'
  matrix M = r(table)'
  preserve
    clear
    svmat double M, names(col)
    gen bin = _n
    keep bin b ll ul
    export delimited "${W}/results/intermediate/fig2_catA_`b'.csv", replace
  restore
  preserve
    collapse (sum) n_obs=es, by(`b')
    rename `b' bin
    drop if missing(bin)
    export delimited "${W}/results/intermediate/fig2_nobsA_`b'.csv", replace
  restore
  drop es
}

* --- reciprocity (panel B): same controls/sample as the continuous spec ---
foreach b in bin3 bin5 {
  reg z_recip ib1.`b' $imbalances2 if particip_12_16_22==1, vce(cluster session)
  gen byte es = e(sample)
  margins i.`b'
  matrix M = r(table)'
  preserve
    clear
    svmat double M, names(col)
    gen bin = _n
    keep bin b ll ul
    export delimited "${W}/results/intermediate/fig2_catB_`b'.csv", replace
  restore
  preserve
    collapse (sum) n_obs=es, by(`b')
    rename `b' bin
    drop if missing(bin)
    export delimited "${W}/results/intermediate/fig2_nobsB_`b'.csv", replace
  restore
  drop es
}

di _n(2) "====== FIG2 CATEGORICAL EXPORT DONE ======"

* ============================================================================
* Merged from _export_fig5_curves.do: exact lpoly curve CSVs for Fig 5 (R)
* ============================================================================
global W : pwd
cap mkdir "${W}/results/intermediate"
clear
set more off
version 16

use "${W}/processed/analysis_rdy.dta", clear
merge m:1 session year using "${W}/data/windspeed_predicted.dta", keep(1 3) nogen
bys session: egen _ws = max(windspeed_predicted)
replace windspeed_predicted = _ws if missing(windspeed_predicted)
drop _ws

* within-village damage inequality (panels e/f), as in 07_manuscript_figures.do
cap drop iqr_damage IQR_DMG_ratio _dmg1000
gen _dmg1000 = damage_TOTAL/1000
bys session: egen iqr_damage = iqr(_dmg1000)
gen IQR_DMG_ratio = iqr_damage / mean_dmg

* one panel: export the lpoly fit grid + village-mean scatter (no nested preserve)
capture program drop expanel
program define expanel
  args yv tag iff
  preserve
    if "`iff'" != "" keep if `iff'
    cap drop gx gy
    lpoly `yv' windspeed_predicted, bwidth(6) n(200) generate(gx gy) nograph
    export delimited gx gy using "${W}/results/intermediate/fig5_`tag'_curve.csv" ///
        if !missing(gx), replace
    collapse (mean) y=`yv' (mean) ws=windspeed_predicted, by(session)
    drop if missing(y, ws)
    export delimited session ws y using "${W}/results/intermediate/fig5_`tag'_pts.csv", replace
  restore
end

expanel share_houses_damaged a ""
expanel mean_dmg             b "year<2022"
expanel share_need_aid       c ""
expanel share_yolanda_2022   d ""
expanel iqr_damage           e ""
expanel IQR_DMG_ratio        f ""

di _n(2) "====== FIG5 CURVES EXPORT DONE ======"

** EOF
