# run_R.R — Master R script for the global tropical-cyclone exposure analysis
# Part of: "Moderate disaster exposure divides communities; severe exposure does not"
# Authors: Ivo Steimanis, Maximilian Burger, Bernd Hayo, Andreas Landmann, Bjorn Vollan
#
# Figures 1 and 4 are independent of Stata; Figures 2 and 3 re-plot Stata margin
# exports, so run.do must run before this script can build them (it skips them
# gracefully otherwise -- see the guarded step 13 below).
# Working directory must be the replication_package/ root.
#
# Produces:
#   Main text Figure 1 (global exposure)    -> results/figures/figure1_global_combined.png
#   Main text Figure 4 (study site map)     -> results/figures/figure4_study_site.png
#   Main text Figures 2 & 3 (post-run.do)   -> results/figures/figure2_field_ushape_windspeed_R.png,
#                                              figure3_mechanisms_R.png
#   Headline numbers for manuscript text    -> results/R_output/headline_numbers.txt
#   Supporting tables for SI               -> results/R_output/*.csv
#   Extended analysis figures              -> results/R_output/*.png
#
# Software: R 4.3+ with packages listed below.
# Estimated runtime: ~5 minutes on a standard desktop.

# --- check working directory ---
if (!file.exists("data/tce_dat/raw/extracted/events_2015/TCE-DAT_single_events_2015/TC_data")) {
  stop("Working directory must be the replication_package/ root.\n",
       "  Current: ", getwd(), "\n",
       "  Expected: a directory containing data/tce_dat/raw/extracted/events_2015/")
}

# --- check required packages ---
required <- c("data.table", "dplyr", "sf", "ggplot2", "rnaturalearth",
              "patchwork", "haven", "scales", "showtext", "ggspatial")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing R packages: ", paste(missing, collapse = ", "), "\n",
       "Install with: install.packages(c(\"",
       paste(missing, collapse = "\", \""), "\"))")
}

# --- create output directories ---
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/R_output", recursive = TRUE, showWarnings = FALSE)
dir.create("data/tce_dat/processed", recursive = TRUE, showWarnings = FALSE)

cat("=== R pipeline started:", format(Sys.time()), "===\n\n")

# --- core pipeline (data extraction + aggregation + headline numbers) ---
cat("--- 01: Extracting TCE-DAT events globally...\n")
source("scripts/R/01_extract_se_asia.R")

cat("\n--- 02: Aggregating to grid cells...\n")
source("scripts/R/02_aggregate.R")

cat("\n--- 03: Building global map figure...\n")
source("scripts/R/03_build_map.R")

cat("\n--- 04: Computing headline numbers...\n")
source("scripts/R/04_headline_numbers.R")

# --- extended analysis ---
cat("\n--- 05: Relative and multi-exposure statistics...\n")
source("scripts/R/05_relative_and_multiexposure.R")

cat("\n--- 06: Exposure by category and frequency...\n")
source("scripts/R/06_exposure_by_category_frequency.R")

cat("\n--- 07: Exposure evolution over time...\n")
source("scripts/R/07_exposure_evolution.R")

cat("\n--- 08: Population decomposition...\n")
source("scripts/R/08_pop_decomposition.R")

cat("\n--- 09: Global map (standalone)...\n")
source("scripts/R/09_global_map_only.R")

cat("\n--- 10: Population decomposition by category...\n")
source("scripts/R/10_pop_decomposition_by_category.R")

# --- main-text figures ---
cat("\n--- 11: Figure 1 (global combined) -> results/figures/\n")
source("scripts/R/11_combined_main_figure.R")

cat("\n--- 12: Figure 4 (study site map) -> results/figures/\n")
source("scripts/R/12_study_site_figure.R")

# --- Figures 2, 3 & 5 (field U-shape, mechanisms, damage validation) ---
# These re-plot Stata exports written by run.do (script 02_analysis_main.do), so
# they can only run AFTER the Stata pipeline. RUN run.do IN STATA FIRST. If any of
# the required CSVs are absent (e.g. this script is run before run.do), all three
# figures are skipped with an explicit message rather than erroring mid-run.
stata_inputs <- c("results/intermediate/fig2_panelA_margins.csv",
                  "results/intermediate/fig2_panelB_margins.csv",
                  "results/intermediate/fig2_catA_bin3.csv",
                  "results/intermediate/fig3_scalars.csv",
                  "results/intermediate/fig5_a_curve.csv")
if (all(file.exists(stata_inputs))) {
  cat("\n--- 13: Figures 2 & 3 (field U-shape + mechanisms) -> results/figures/\n")
  source("scripts/R/13_figure2_3_maintext.R")
  cat("\n--- 14: Figure 5 (damage validation) -> results/figures/\n")
  source("scripts/R/14_figure5_impacts.R")
} else {
  miss <- stata_inputs[!file.exists(stata_inputs)]
  cat("\n--- 13-14: SKIPPED -- Figures 2, 3 & 5 need the Stata exports from run.do.\n",
      "        Missing:", paste(basename(miss), collapse = ", "), "\n",
      "        Run run.do in Stata FIRST, then re-run this script.\n")
}

cat("\n=== R pipeline complete:", format(Sys.time()), "===\n")
