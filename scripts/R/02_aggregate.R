# 02_aggregate.R
# For each (LAT, LON) grid cell globally, aggregate over all events 1950-2015:
#   - max_windspeed_kn  : maximum wind experienced (knots)
#   - max_cat           : Saffir-Simpson category derived from max_windspeed_kn
#   - n_events_total    : count of events that touched the cell at >=34 kn
#   - n_events_cat2to4  : count of events with wind in Cat 2-4 range
#   - year_first_event  : year of first TC event affecting the cell
#   - year_max_event    : year of the event producing the max wind
#   - exposed_pop       : population (consistent under fix2015)
#   - ISO               : country
#
# Saffir-Simpson thresholds (sustained 1-min wind, knots):
#   TS:    34-63
#   Cat 1: 64-82
#   Cat 2: 83-95
#   Cat 3: 96-112
#   Cat 4: 113-136
#   Cat 5: 137+
#
# Output: data/tce_dat/processed/global_cells_aggregated.rds
#         data/tce_dat/processed/global_annual_exposure.rds

suppressPackageStartupMessages({
  library(data.table)
})

root <- "data/tce_dat"
in_path  <- file.path(root, "processed/global_events_long.rds")
out_path <- file.path(root, "processed/global_cells_aggregated.rds")

dt <- as.data.table(readRDS(in_path))
cat("Loaded", format(nrow(dt), big.mark = ","), "event-cell rows.\n")

cat_from_knots <- function(w) {
  fcase(
    is.na(w) | w < 34, 0L,
    w < 64,  1L,   # TS
    w < 83,  2L,   # Cat 1
    w < 96,  3L,   # Cat 2
    w < 113, 4L,   # Cat 3
    w < 137, 5L,   # Cat 4
    w >= 137, 6L   # Cat 5
  )
}

dt[, ss_code := cat_from_knots(windspeed_kn)]
dt <- dt[ss_code >= 1]
cat("After dropping sub-TS rows:", format(nrow(dt), big.mark = ","), "\n")

dt[, year := as.integer(substr(storm_id, 1, 4))]

cell <- dt[, .(
  max_windspeed_kn  = max(windspeed_kn, na.rm = TRUE),
  n_events_total    = .N,
  n_events_ts       = sum(ss_code == 1L),
  n_events_cat1     = sum(ss_code == 2L),
  n_events_cat2     = sum(ss_code == 3L),
  n_events_cat3     = sum(ss_code == 4L),
  n_events_cat4     = sum(ss_code == 5L),
  n_events_cat5     = sum(ss_code == 6L),
  n_events_cat2to4  = sum(ss_code %in% 3:5),
  year_first_event  = min(year),
  year_max_event    = year[which.max(windspeed_kn)],
  exposed_pop       = max(exposed_pop, na.rm = TRUE),
  exposed_assets    = max(exposed_assets, na.rm = TRUE),
  ISO               = ISO[1]
), by = .(LAT, LON)]

cell[, max_cat := cat_from_knots(max_windspeed_kn)]

ss_label <- function(code) {
  fcase(
    code == 1L, "TS",
    code == 2L, "Cat 1",
    code == 3L, "Cat 2",
    code == 4L, "Cat 3",
    code == 5L, "Cat 4",
    code == 6L, "Cat 5"
  )
}
cell[, max_cat_label := ss_label(max_cat)]

cell[, regime := fcase(
  max_cat == 6L, "Catastrophic (Cat 5)",
  max_cat %in% 3:5 & n_events_cat2to4 >= 3, "Moderate, repeated (>=3 events)",
  max_cat %in% 3:5, "Moderate (Cat 2-4)",
  max_cat == 2L, "Low (Cat 1)",
  max_cat == 1L, "Tropical Storm only"
)]

# World Bank 2015 fiscal year income classification for all 98 ISOs in TCE-DAT
income_tier_map <- data.table(
  ISO = c(
    "ABW","AIA","AND","ANT","ASM","ATG","AUS","BGD","BHS","BLZ",
    "BRA","BRB","BRN","CAN","CHN","COL","COM","CPV","CRI","CUB",
    "DMA","DOM","DZA","ESP","FJI","FRA","FRO","GBR","GIN","GLP",
    "GNB","GRD","GTM","HKG","HND","HTI","IDN","IMN","IND","IRL",
    "IRN","ISL","JAM","JPN","KHM","KNA","KOR","LAO","LCA","LKA",
    "MAR","MDG","MEX","MMR","MOZ","MSR","MTQ","MUS","MWI","MYS",
    "MYT","NCL","NIC","NOR","NZL","OMN","PAK","PAN","PHL","PNG",
    "PRI","PRK","PRT","REU","RUS","SAU","SGP","SLB","SLV","SOM",
    "SPM","TCA","THA","TLS","TON","TTO","TWN","TZA","USA","VCT",
    "VEN","VIR","VNM","VUT","WSM","YEM","ZAF","ZWE"
  ),
  income_tier = c(
    "High","High","High","High","Upper-middle","High","High","Lower-middle","High","Upper-middle",
    "Upper-middle","High","High","High","Upper-middle","Upper-middle","Low","Lower-middle","Upper-middle","Upper-middle",
    "Upper-middle","Upper-middle","Upper-middle","High","Upper-middle","High","High","High","Low","High",
    "Low","Upper-middle","Lower-middle","High","Lower-middle","Low","Lower-middle","High","Lower-middle","High",
    "Upper-middle","High","Upper-middle","High","Lower-middle","High","High","Lower-middle","Upper-middle","Lower-middle",
    "Lower-middle","Low","Upper-middle","Lower-middle","Low","High","High","Upper-middle","Low","Upper-middle",
    "High","High","Lower-middle","High","High","High","Lower-middle","Upper-middle","Lower-middle","Lower-middle",
    "High","Lower-middle","High","High","Upper-middle","High","High","Lower-middle","Lower-middle","Low",
    "High","High","Upper-middle","Lower-middle","Upper-middle","High","High","Low","High","Upper-middle",
    "Upper-middle","High","Lower-middle","Lower-middle","Lower-middle","Lower-middle","Upper-middle","Low"
  )
)
cell <- merge(cell, income_tier_map, by = "ISO", all.x = TRUE)
cell[is.na(income_tier), income_tier := "Unclassified"]

cat("\nCell counts by regime:\n")
print(cell[, .N, by = regime][order(-N)])

cat("\nCell counts by ISO (top 20):\n")
print(cell[, .N, by = ISO][order(-N)][1:20])

cat("\nPopulation by regime (millions):\n")
print(cell[, .(pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)), by = regime][order(-pop_M)])

cat("\nPopulation by income tier (millions):\n")
print(cell[, .(pop_M = round(sum(exposed_pop, na.rm = TRUE) / 1e6, 1)), by = income_tier][order(-pop_M)])

cat("\nUnclassified ISOs:\n")
print(cell[income_tier == "Unclassified", unique(ISO)])

saveRDS(cell, out_path)
cat("\nSaved:", out_path, "(", format(file.size(out_path) / 1e6, digits = 3), "MB)\n")
cat("Total cells:", format(nrow(cell), big.mark = ","), "\n")

# --- Annual person-exposure aggregation ---
cat("\n=== Annual exposure aggregation ===\n")
dt[, cat_group := fcase(
  ss_code == 1L, "Tropical Storm",
  ss_code == 2L, "Cat 1",
  ss_code %in% 3:5, "Cat 2-4",
  ss_code == 6L, "Cat 5"
)]

annual_exp <- dt[, .(person_exp = sum(exposed_pop, na.rm = TRUE)),
                 by = .(year, cat_group)]
setorder(annual_exp, year, cat_group)
annual_exp[, cum_person_exp := cumsum(person_exp), by = cat_group]

annual_out <- file.path(root, "processed/global_annual_exposure.rds")
saveRDS(annual_exp, annual_out)
cat("Saved:", annual_out, "\n")
cat("Years:", min(annual_exp$year), "-", max(annual_exp$year), "\n")
cat("Total cumulative person-exposures (all categories):",
    format(sum(annual_exp$person_exp), big.mark = ","), "\n")
cat("Cumulative Cat 2-4 person-exposures:",
    format(sum(annual_exp[cat_group == "Cat 2-4", person_exp]), big.mark = ","), "\n")
