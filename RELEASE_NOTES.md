# Release notes

**Replication package for "Moderate tropical cyclone exposure erodes solidarity needed for
recovery"** (Ivo Steimanis, Maximilian Burger, Bernd Hayo, Andreas Landmann, Björn Vollan).

**Contents.** De-identified three-wave panel (2012, 2016, 2022) and lab-in-the-field experiment
data, the full Stata and R analysis pipeline, bundled Stata packages, and all output tables and
figures. The package reproduces every statistical result, all tables, and Figures 2-5 with no
external downloads. Figure 1 (the global cyclone-exposure map) additionally requires the public
TCE-DAT dataset (Geiger et al. 2018, GFZ Potsdam), which is too large to redistribute.

**Reproduce.** Run the Stata stage first (`do run.do`), then the R stage (`source("run_R.R")`); on
Windows, `run_replication.bat` runs both in order. Requires Stata 16+ (last reproduced on StataNow 19
and Stata 16/MP; all user-written packages are bundled, no internet needed) and R 4.3+. Total
runtime about 10 minutes.

**Data and ethics.** Direct identifiers (names, contacts, addresses) are removed; households are
keyed to anonymized village IDs (V01-V30). The study was approved by the ethics commission of the
School of Business, Economics and Society at Friedrich-Alexander University Erlangen-Nürnberg and,
for the 2022 wave, by the Philippine Social Science Council-Social Science Ethics Review Board
(PSSC-SSERB, CC-22-54); informed consent was obtained from all participants.

**Citation.** Please cite the paper and this package (Zenodo DOI: 10.5281/zenodo.21099059).
