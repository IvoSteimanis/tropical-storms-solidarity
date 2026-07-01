*-------------------------------------------------------------------------------------------------------
* OVERVIEW
*-------------------------------------------------------------------------------------------------------
*   This script generates tables and figures reported in the manuscript and SOM of the paper:
*   "Moderate tropical cyclone exposure erodes solidarity needed for recovery"
*	Authors: Ivo Steimanis, Maximilian Burger, Bernd Hayo, Andreas Landmann, and Björn Vollan
*   All experimental data are stored in /data
*   All figures reported in the main manuscript and SOM are outputted to /results/figures
*   All tables reported in the main manuscript and SOM are outputted to /results/tables
* TO PERFORM A CLEAN RUN, DELETE THE FOLLOWING TWO FOLDERS:
*   /processed
*   /results
* NOTE: /static_figures holds hand-made manuscript figures (e.g., figureS1_ToC.png = Figure S1)
*   that are NOT produced by any script; it ships with the package and must NOT be deleted.
*-------------------------------------------------------------------------------------------------------


*--------------------------------------------------
* Set global Working Directory
*--------------------------------------------------
* Define this global macro to point where the replication folder is saved locally that includes this run.do script.
* Robust auto-detection: locate the replication root by finding the folder that holds the sentinel data file,
* checking the current directory and walking up to four parents. This makes run.do work no matter where Stata was
* launched from; launching it from the Do-file editor while pwd was .../scripts otherwise yields a doubled
* scripts/scripts/logs path and r(603) on the first `log using`. (c(filename) is unreliable here: it is empty in
* batch mode and points at a temp file for partial-selection runs, so a sentinel search is used instead.)
local root_found 0
forvalues up = 0/4 {
    capture confirm file "data/PHI_Panel_12_16_22.dta"
    if !_rc {
        local root_found 1
        continue, break
    }
    qui cd ".."
}
if !`root_found' {
    di as error "ERROR: could not locate the replication root."
    di as error "The sentinel file data/PHI_Panel_12_16_22.dta was not found in the current directory"
    di as error "or up to four parent folders. cd into the replication_package folder and rerun run.do."
    exit 601
}
global working_ANALYSIS : pwd
di "Auto-detected directory: $working_ANALYSIS"

* Verify this is the correct directory by checking for expected files
capture confirm file "$working_ANALYSIS/data/PHI_Panel_12_16_22.dta"
if _rc != 0 {
    di as error " "
    di as error "ERROR: Cannot find expected data files."
    di as error "Make sure you run this script from the project root directory."
    di as error "Current directory: $working_ANALYSIS"
    exit 601
}

di as text "Directory validated successfully."

*--------------------------------------------------
* Program Setup
*--------------------------------------------------
* Initialize log and record system parameters
clear
set more off
cap mkdir "$working_ANALYSIS/scripts/logs"
cap log close
local datetime : di %tcCCYY.NN.DD!-HH.MM.SS `=clock("$S_DATE $S_TIME", "DMYhms")'
local logfile "$working_ANALYSIS/scripts/logs/`datetime'.log.txt"
log using "`logfile'", text

di "Begin date and time: $S_DATE $S_TIME"
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `=cond( c(MP),"MP",cond(c(SE),"SE",c(flavor)) )'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"

*   Analyses were run on Windows using Stata version 16
version 16              // Set Version number for backward compatibility

* All required Stata packages are available in the /libraries/stata folder
tokenize `"$S_ADO"', parse(";")
while `"`1'"' != "" {
  if `"`1'"'!="BASE" cap adopath - `"`1'"'
  macro shift
}
adopath ++ "$working_ANALYSIS/scripts/libraries/stata"
mata: mata mlib index

* Create directories for output files
cap mkdir "$working_ANALYSIS/processed"
cap mkdir "$working_ANALYSIS/results"
cap mkdir "$working_ANALYSIS/results/intermediate"
cap mkdir "$working_ANALYSIS/results/tables"
cap mkdir "$working_ANALYSIS/results/figures"
* -------------------------------------------------

* Set general graph style
set scheme swift_red //select one scheme as reference scheme to work with
grstyle init 
{
*Background color
grstyle set color white: background plotregion graphregion legend box textbox //

*Main colors (note: swift_red only defines 8 colors. Multiplying the color, that is "xx yy zz*0.5" reduces/increases intensity and "xx yy zz%50" reduces transparency)
grstyle set color 	"100 143 255" "120 94 240" "220 38 127" "254 97 0" "255 176 0" /// 5 main colors
					"100 143 255*0.4" "120 94 240*0.4" "220 38 127*0.4" "254 97 0*0.4" "255 176 0*0.4" ///
					"100 143 255*1.7" "120 94 240*1.7" "220 38 127*1.7" "254 97 0*1.7" "255 176 0*1.7" ///
					: p# p#line p#lineplot p#bar p#area p#arealine p#pie histogram 

*margins
grstyle set compact

*Font size
grstyle set size 10pt: heading //titles
grstyle set size 8pt: subheading axis_title //axis titles
grstyle set size 8pt: p#label p#boxlabel body small_body text_option axis_label tick_label minortick_label key_label //all other text

}
* -------------------------------------------------

*--------------------------------------------------
* Run processing and analysis scripts
*--------------------------------------------------
* Three numbered scripts run in order:
*   01_clean_data.do    -> processed/analysis_rdy.dta
*   02_analysis_main.do -> main-text models, quoted numbers, and the margin/curve
*                          CSV exports that the R scripts read to render Figs 2/3/5
*   03_analysis_SI.do   -> SI tables (S1-S33) and SI figures (S2-S11)
* IMPORTANT: run this BEFORE the R pipeline (run_R.R). The R rebuilds of Figures
* 2, 3 and 5 read the CSVs written by 02 into results/intermediate/.
do "$working_ANALYSIS/scripts/01_clean_data.do"
do "$working_ANALYSIS/scripts/02_analysis_main.do"
do "$working_ANALYSIS/scripts/03_analysis_SI.do"

di "End date and time (full pipeline): $S_DATE $S_TIME"
log close
 
 
 
** EOF