# 05_relative_and_multiexposure.R
# Two questions raised for the NCC framing / E4E talk:
#   (1) From ABSOLUTE affected to RELATIVE: express the 758.5M Cat 2-4 regime
#       population as a share of transparent, named denominators -- all at the
#       same fixed-2015 population baseline, so the share is free of population
#       growth by construction.
#   (2) MULTIPLE exposures: among people in the moderate (Cat 2-4) regime, how
#       many were exposed once vs. repeatedly, and what is the cumulative
#       person-exposure count.
#
# Reuses existing processed data (no re-extraction).
# Output: results/R_output/relative_multiexposure.csv + console summary.

suppressPackageStartupMessages({
  library(data.table)
})

root <- "data/tce_dat"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
annual <- as.data.table(readRDS(file.path(root, "processed/global_annual_exposure.rds")))

# Regimes that make up the exclusive moderate (Cat 2-4, no Cat 5) population
mod_regimes <- c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)")

out <- list()
add <- function(metric, value, unit, note = "") {
  out[[length(out) + 1]] <<- data.table(metric = metric, value = value, unit = unit, note = note)
}

# =====================================================================
# (1) ABSOLUTE -> RELATIVE
# =====================================================================
mod_pop      <- sum(cells[regime %in% mod_regimes, exposed_pop], na.rm = TRUE)
mod_lmic_pop <- sum(cells[regime %in% mod_regimes & income_tier %in% c("Low", "Lower-middle"), exposed_pop], na.rm = TRUE)
tc_exposed   <- sum(cells$exposed_pop, na.rm = TRUE)            # any TC wind, ever (all regimes)
# Inclusive Cat 2-4 (any Cat 2-4 event, even if cell also saw Cat 5)
incl_pop     <- sum(cells[n_events_cat2to4 >= 1, exposed_pop], na.rm = TRUE)

# External denominator: world mid-year population 2015 (UN WPP 2015 revision).
# Stated as an explicit constant; the TCE-DAT baseline is also 2015, so numerator
# and denominator share the same year -> the share is population-growth-free.
world_pop_2015 <- 7.379e9   # UN World Population Prospects, 2015 mid-year (~7.379 bn)

cat("\n=== (1) ABSOLUTE -> RELATIVE (all fix-2015 baseline) ===\n")
cat(sprintf("Cat 2-4 regime population (exclusive):      %.1f M\n", mod_pop/1e6))
cat(sprintf("  of which low/lower-middle income:         %.1f M (%.1f%% of regime)\n",
            mod_lmic_pop/1e6, 100*mod_lmic_pop/mod_pop))
cat(sprintf("Total TC-exposed population (any wind):     %.1f M\n", tc_exposed/1e6))
cat(sprintf("Inclusive Cat 2-4 (incl. Cat 5 cells):      %.1f M\n", incl_pop/1e6))
cat("\nShares of the 758.5M-style headline against named denominators:\n")
cat(sprintf("  as %% of world population 2015 (~7.379 bn): %.2f%%\n", 100*mod_pop/world_pop_2015))
cat(sprintf("  as %% of all TC-exposed people (%.2f bn):   %.2f%%\n",
            tc_exposed/1e9, 100*mod_pop/tc_exposed))

add("mod_cat24_pop_exclusive_M",       round(mod_pop/1e6, 1),        "million people", "max regime Cat 2-4, no Cat 5")
add("mod_cat24_lmic_M",                round(mod_lmic_pop/1e6, 1),   "million people", "low + lower-middle income")
add("tc_exposed_total_M",              round(tc_exposed/1e6, 1),     "million people", "any TC wind ever, all regimes")
add("mod_cat24_pop_inclusive_M",       round(incl_pop/1e6, 1),       "million people", "any Cat 2-4 event incl. Cat 5 cells")
add("world_pop_2015_M",                round(world_pop_2015/1e6, 1), "million people", "UN WPP 2015 mid-year (external constant)")
add("share_of_world_pct",              round(100*mod_pop/world_pop_2015, 2), "percent", "Cat 2-4 regime / world pop 2015")
add("share_of_tc_exposed_pct",         round(100*mod_pop/tc_exposed, 2),     "percent", "Cat 2-4 regime / all TC-exposed")

# =====================================================================
# (2) MULTIPLE EXPOSURES (within the exclusive moderate regime)
# =====================================================================
mod <- cells[regime %in% mod_regimes]
mod[, exp_band := fifelse(n_events_cat2to4 <= 1, "1x",
                  fifelse(n_events_cat2to4 == 2, "2x", ">=3x"))]

freq <- mod[, .(pop_M = round(sum(exposed_pop, na.rm = TRUE)/1e6, 1),
                cells = .N), by = exp_band]
freq[, share_of_regime_pct := round(100 * pop_M / sum(pop_M), 1)]
setorder(freq, -pop_M)

# Cumulative person-exposures (each Cat 2-4 event re-exposes the cell's fixed-2015 pop)
person_events  <- sum(mod$exposed_pop * mod$n_events_cat2to4, na.rm = TRUE)
unique_persons <- sum(mod$exposed_pop, na.rm = TRUE)
mean_freq      <- person_events / unique_persons

cat("\n=== (2) MULTIPLE EXPOSURES (within Cat 2-4 regime, 758.5M) ===\n")
print(freq)
cat(sprintf("\nUnique persons (counted once):              %.1f M\n", unique_persons/1e6))
cat(sprintf("Cumulative person-exposures (sum events):   %.1f M\n", person_events/1e6))
cat(sprintf("Mean Cat 2-4 exposures per person:          %.2f\n", mean_freq))

# Cross-check vs annual file: total Cat 2-4 person-exposures counted by event category
ann_cat24_cum <- annual[cat_group == "Cat 2-4" & year == max(year), cum_person_exp]
cat(sprintf("[cross-check] annual-file cumulative Cat 2-4 person-exposures (event-classified): %.1f M\n",
            ann_cat24_cum/1e6))

for (i in seq_len(nrow(freq))) {
  add(paste0("multiexp_", gsub(">=","ge",gsub("x","",freq$exp_band[i])), "x_pop_M"),
      freq$pop_M[i], "million people", paste0("share ", freq$share_of_regime_pct[i], "% of regime"))
}
add("unique_persons_M",        round(unique_persons/1e6, 1), "million people", "Cat 2-4 regime, counted once")
add("person_exposures_M",      round(person_events/1e6, 1),  "person-exposures (M)", "sum over Cat 2-4 events in regime")
add("mean_exposures_per_person", round(mean_freq, 2),        "events/person", "person-exposures / unique persons")

# =====================================================================
# Write CSV
# =====================================================================
res <- rbindlist(out)
out_csv <- file.path("results/R_output/relative_multiexposure.csv")
fwrite(res, out_csv)
cat("\nSaved:", out_csv, "\n")
print(res)
