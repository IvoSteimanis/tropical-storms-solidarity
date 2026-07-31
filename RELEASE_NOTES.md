# Release notes

## v1.0.0

**Replication package for "Moderate tropical cyclone exposure erodes solidarity needed for
recovery"** (Ivo Steimanis, Maximilian Burger, Bernd Hayo, Andreas Landmann, Björn Vollan).

**Contents.** De-identified three-wave panel (2012, 2016, 2022) and lab-in-the-field experiment
data, the full Stata and R analysis pipeline, bundled Stata packages, and all output tables and
figures. The package reproduces every statistical result, all tables, and Figures 2-5 with no
external downloads. Figure 1 (the global cyclone-exposure map) additionally requires the public
TCE-DAT dataset (Geiger et al. 2018, GFZ Potsdam), which is too large to redistribute.

**Inference.** The design has 30 village clusters, so analytic cluster-robust inference is
anti-conservative. Wild cluster bootstrap-t p-values are therefore reported alongside conventional
standard errors throughout. `results/tables/tableS37_boottest_ladder.tex` attaches the bootstrap to
every column of the main-effects and long-term tables: the primary quadratic specification remains
significant throughout (bootstrap p between 0.009 and 0.018), while the categorical specification
does not survive the correction (0.064 to 0.129), which is the expected consequence of identifying
the moderate category from two villages. `results/tables/tableS38_linear_vs_quadratic.tex` reports
adjusted R-squared for both specifications, the change in AIC and BIC from adding the squared term,
a likelihood-ratio test, and the bootstrap p-value on the quadratic term alone.

**Reproduce.** Run the Stata stage first (`do run.do`), then the R stage (`source("run_R.R")`); on
Windows, `run_replication.bat` runs both in order. Requires Stata 16+ (last reproduced on StataNow 19
and Stata 16/MP; all user-written packages are bundled, no internet needed) and R 4.3+. Total
runtime about 10 minutes.

**Data and ethics.** Participant names, contact details and household addresses are removed;
households are keyed to anonymized village IDs (V01-V30). The coordinates of the 30 study sites are
retained in `data/map/StudySitePoints.dta`, since they are required to reproduce the study-site map.
The study was approved by the ethics commission of the School of Business, Economics and Society at
Friedrich-Alexander University Erlangen-Nürnberg and, for the 2022 wave, by the Philippine Social
Science Council-Social Science Ethics Review Board (PSSC-SSERB, CC-22-54); informed consent was
obtained from all participants.

**Citation.** Please cite the paper and this package (Zenodo concept DOI:
10.5281/zenodo.21099059), which always resolves to the latest version.
