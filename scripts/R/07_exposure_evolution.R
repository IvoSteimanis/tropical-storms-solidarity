# 07_exposure_evolution.R
# Relative "evolution" figure: composition of the TC-exposed population by storm
# category, as SHARES summing to 1 each year, 1950-2015. Relative analog of the
# absolute cumulative panel (c) in 03_build_map.R (cumulative UNIQUE persons
# first-exposed, classified by the cell's MAXIMUM category).
#
# CAVEAT (printed + for the caption): pre-satellite-era best-track records
# (roughly pre-1980) under-detect weaker/remote storms, so early-period
# composition and any trend are partly a detection artifact, not a clean climate
# signal. The fix-2015 population baseline removes demographic growth, so the
# series reflects the hazard mix, not population.
#
# Output: results/R_output/figure_exposure_evolution.png
# Console: decadal cumulative-share AND annual-new-share tables (to compare).

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales)
})

root <- "data/tce_dat"; ff <- "sans"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
cells[, regime_group := fcase(
  regime == "Tropical Storm only", "Tropical Storm",
  regime == "Low (Cat 1)", "Cat 1",
  regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), "Cat 2-4",
  regime == "Catastrophic (Cat 5)", "Cat 5")]
lev <- c("Cat 5", "Cat 2-4", "Cat 1", "Tropical Storm")   # stack order (severity on top)
cells[, regime_group := factor(regime_group, levels = lev)]
cat_colors <- c("Tropical Storm" = "#7BAF6B", "Cat 1" = "#E8A020",
                "Cat 2-4" = "#DC267F", "Cat 5" = "#1A1A1A")

# ---- cumulative unique persons first-exposed, by year and category (panel-c logic) ----
annual <- cells[, .(pop_M = sum(exposed_pop, na.rm = TRUE)/1e6),
                by = .(year = year_first_event, regime_group)]
grid <- CJ(year = seq(min(cells$year_first_event), max(cells$year_first_event)),
           regime_group = factor(lev, levels = lev))
annual <- merge(grid, annual, by = c("year","regime_group"), all.x = TRUE)
annual[is.na(pop_M), pop_M := 0]
setorder(annual, regime_group, year)
annual[, cum_pop_M := cumsum(pop_M), by = regime_group]
annual[, cum_share := cum_pop_M / sum(cum_pop_M), by = year]          # cumulative composition (sums to 1)
annual[, ann_share := pop_M / sum(pop_M), by = year]                  # annual new-exposure composition

# ---- diagnostics: decadal shares both ways ----
dec <- function(col) {
  d <- annual[year %in% c(1955,1965,1975,1985,1995,2005,2015),
              .(share = get(col)[1]), by = .(year, regime_group)]  # placeholder
  dcast(annual[year %in% seq(1955,2015,10)], year ~ regime_group, value.var = col)
}
cat("\n=== CUMULATIVE composition (share of ever-exposed), by decade ===\n")
print(dcast(annual[year %in% seq(1955,2015,10)], year ~ regime_group, value.var = "cum_share")[, lapply(.SD, function(x) round(x,3))])
cat("\n=== ANNUAL new-exposure composition (share of that year's new cells), by decade ===\n")
print(dcast(annual[year %in% seq(1955,2015,10)], year ~ regime_group, value.var = "ann_share")[, lapply(.SD, function(x) round(x,3))])

# ---- figure: 100% stacked cumulative composition over time ----
p <- ggplot(annual, aes(year, cum_share, fill = regime_group)) +
  geom_area(alpha = 0.9, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 1980, linetype = "dashed", color = "grey35", linewidth = 0.3) +
  annotate("text", x = 1980, y = 1.03, label = "satellite era →", hjust = 0,
           size = 3.2, color = "grey35", family = ff) +
  scale_fill_manual(values = cat_colors, name = NULL,
                    breaks = c("Tropical Storm","Cat 1","Cat 2-4","Cat 5")) +
  scale_x_continuous(breaks = seq(1950,2010,10), expand = expansion(mult = c(0.01,0.03))) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0,0.06))) +
  labs(x = NULL, y = "Share of TC-exposed population") +
  theme_minimal(base_size = 13, base_family = ff) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom")

ggsave(file.path("results/R_output/figure_exposure_evolution.png"), p, width = 8, height = 4.6, dpi = 300, bg = "transparent")
cat("\nSaved figure_exposure_evolution.png\n")
