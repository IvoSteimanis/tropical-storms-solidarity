# 04_headline_numbers.R
# Compute the headline statistics for NCC caption + manuscript text + cover letter.
# Uses GLOBAL TCE-DAT aggregation (all basins, 1950-2015).
# Outputs: results/R_output/headline_numbers.txt and supporting CSVs

suppressPackageStartupMessages({
  library(data.table)
})

root <- "data/tce_dat"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
events <- as.data.table(readRDS(file.path(root, "processed/global_events_long.rds")))

out_lines <- c()
ad <- function(x) out_lines <<- c(out_lines, x)

ad("HEADLINE NUMBERS FOR NCC MANUSCRIPT (GLOBAL)")
ad(paste("Generated:", Sys.time()))
ad(paste(rep("=", 60), collapse = ""))

# --- (1) Global population by regime ---
by_regime <- cells[, .(
  cells = .N,
  pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1),
  assets_Bn = round(sum(exposed_assets, na.rm = TRUE) / 1e9, 1)
), by = regime][order(-pop_M)]
ad("")
ad("1. Global exposure by regime (1950-2015)")
ad(paste(capture.output(print(by_regime)), collapse = "\n"))

# --- (2) By income tier ---
by_income <- cells[, .(
  cells = .N,
  pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)
), by = income_tier][order(-pop_M)]
ad("")
ad("2. Global exposure by income tier")
ad(paste(capture.output(print(by_income)), collapse = "\n"))

# --- (3) Cat 2-4 regime by income tier ---
mod_by_income <- cells[regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), .(
  pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)
), by = income_tier][order(-pop_M)]
ad("")
ad("3. Cat 2-4 regime population by income tier")
ad(paste(capture.output(print(mod_by_income)), collapse = "\n"))

# --- (4) Top countries in Cat 2-4 regime ---
mod_by_country <- cells[regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), .(
  pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)
), by = .(ISO, income_tier)][order(-pop_M)]
ad("")
ad("4. Top 15 countries in Cat 2-4 regime")
ad(paste(capture.output(print(mod_by_country[1:15])), collapse = "\n"))

# --- (5) Event counts ---
ad("")
ad("5. Event counts (global)")
n_events_total <- events[, length(unique(storm_id))]
events_max <- events[, .(max_wind = max(windspeed_kn, na.rm = TRUE)), by = storm_id]
n_cat5 <- events_max[max_wind >= 137, .N]
n_cat24_peak <- events_max[max_wind >= 83 & max_wind < 137, .N]
ad(paste("  Total events globally 1950-2015:", n_events_total))
ad(paste("  Events reaching Cat 5 (>=137 kn):", n_cat5))
ad(paste("  Events peaking at Cat 2-4 (83-136 kn):", n_cat24_peak))

# --- (6) Philippines drill-down ---
ad("")
ad("6. Philippines drill-down")
ph_total <- cells[ISO == "PHL", round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)]
ph_mod <- cells[ISO == "PHL" & regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"),
                round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)]
ph_cat5 <- cells[ISO == "PHL" & regime == "Catastrophic (Cat 5)",
                 round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)]
ad(paste("  Philippine TC-exposed population:", ph_total, "M"))
ad(paste("  Philippine Cat 2-4 regime:", ph_mod, "M"))
ad(paste("  Philippine Cat 5 regime:", ph_cat5, "M"))

# --- (7) Inclusive Cat 2-4 (including cells that also saw Cat 5) ---
ad("")
ad("7. Inclusive Cat 2-4 exposure (cells with any Cat 2-4 event, even if also Cat 5)")
inclusive <- cells[n_events_cat2to4 >= 1, .(
  cells = .N, pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1))]
ad(paste("  Cells:", inclusive$cells, "  Population:", inclusive$pop_M, "M"))

# --- Headline aggregates ---
mod_global_M <- round(sum(cells[regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), exposed_pop], na.rm = TRUE) / 1e6, 1)
mod_lmic_M <- round(sum(cells[regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)") & income_tier %in% c("Lower-middle", "Low"), exposed_pop], na.rm = TRUE) / 1e6, 1)
grand_total_M <- round(sum(cells$exposed_pop, na.rm = TRUE) / 1e6, 1)

# --- Manuscript-ready paragraphs ---
ad("")
ad(paste(rep("-", 60), collapse = ""))
ad("MANUSCRIPT-READY PARAGRAPHS")
ad(paste(rep("-", 60), collapse = ""))

ad("")
ad("Caption (Figure 1):")
ad(paste0(
  "Global tropical cyclone exposure, 1950-2015 (data: TCE-DAT, Geiger et al. 2018; 2015 population baseline). ",
  "(a) Panay Island study area with Typhoon Haiyan track (red), 30 study villages (blue), and the 40-80 km moderate-exposure annulus (dashed pink). ",
  "(b) Coastal grid cells (0.1 degree) colored by the maximum Saffir-Simpson category experienced over the period. ",
  "Cells in the Cat 2-4 wind regime (pink/magenta), where damage heterogeneity within communities is highest, are the regime in which our mechanism operates. ",
  "(c) Cumulative population (2015 baseline) first exposed to tropical cyclone winds over 1950-2015, by maximum wind regime. ",
  "Approximately ", mod_global_M, " million people globally live in the Cat 2-4 regime (", mod_lmic_M, " million in low- and lower-middle-income countries). ",
  "Projected cyclone intensification (Knutson et al. 2020; IPCC AR6) is expected to expand this population."))

ad("")
ad("Intro paragraph (climate scope):")
ad(paste0(
  "Globally, approximately ", mod_global_M, " million coastal residents live in areas that have experienced ",
  "tropical cyclone winds in the Cat 2-4 range over 1950-2015 (Fig. 1b); ",
  mod_lmic_M, " million of these are in low- and lower-middle-income countries where communities ",
  "rely primarily on informal networks for post-disaster risk sharing."))

ad("")
ad("Discussion paragraph:")
ad(paste0(
  "Globally, around ", mod_global_M, " million residents live in areas that have experienced the Cat 2-4 wind regime ",
  "where the mechanism we document is plausibly operative (Fig. 1b,c). ",
  "Of these, ", mod_lmic_M, " million are in low- and lower-middle-income countries where informal ",
  "risk-sharing networks are the primary safety net. ",
  "Projected intensification of tropical cyclones under climate change (Knutson et al. 2020; IPCC AR6 Ch11) ",
  "is expected to expand the population exposed to this regime, ",
  "while aid-attention allocation continues to concentrate on the most visibly devastated areas."))

ad("")
ad("Cover letter sentence:")
ad(paste0(
  "Around ", mod_global_M, " million coastal residents globally live in areas historically ",
  "exposed to the Cat 2-4 wind regime in which we identify this mechanism; ",
  mod_lmic_M, " million of these are in low- and lower-middle-income countries."))

# Write
out_txt <- file.path("results/R_output/headline_numbers.txt")
writeLines(out_lines, out_txt)
cat("Saved:", out_txt, "\n")

fwrite(by_regime, file.path("results/R_output/headline_by_regime.csv"))
fwrite(mod_by_country, file.path("results/R_output/headline_by_country_regime.csv"))
fwrite(mod_by_income, file.path("results/R_output/headline_mod_by_income.csv"))
cat("Saved CSVs to results/R_output/\n")

cat("\n=== KEY NUMBERS ===\n")
cat("Global Cat 2-4 regime:          ", mod_global_M, "M\n")
cat("Low/LMIC Cat 2-4 regime:        ", mod_lmic_M, "M\n")
cat("Philippines Cat 2-4 regime:     ", ph_mod, "M\n")
cat("Total TC-exposed globally:      ", grand_total_M, "M\n")
