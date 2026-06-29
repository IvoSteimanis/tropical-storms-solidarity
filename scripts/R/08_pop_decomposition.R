# 08_pop_decomposition.R
# Decompose the rise in people affected by tropical cyclones over 1950-2015 into
# POPULATION GROWTH vs STORM/HAZARD change, by comparing TCE-DAT's two variants:
#   - historical (year-matched population): actual affected = population_t x storms_t
#   - fix2015    (2015 population held constant): storms_t only (population removed)
# The GAP between the two = population-growth contribution; the fix2015 curve's
# trend = storm/detection contribution.
#
# CAVEAT (annotated): the fix2015 curve still mixes genuine storm change with
# improving best-track DETECTION over time (esp. pre-~1980), so it is NOT a clean
# climate trend; the dedicated literature (Knutson 2020; IPCC AR6; Bhatia 2019)
# handles that. This figure's clean message is the POPULATION vs STORM split.
#
# Output: results/R_output/figure_pop_decomposition.png

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })

root <- "data/tce_dat"; ff <- "sans"
hist_dir <- file.path(root, "raw/extracted/events_historical/TCE-DAT_single_events_historical/TC_data")

# ---- fix2015 annual total person-exposures (from already-extracted long table) ----
ev <- as.data.table(readRDS(file.path(root, "processed/global_events_long.rds")))
ev[, year := as.integer(substr(storm_id, 1, 4))]
annual_fix <- ev[, .(pop_M = sum(exposed_pop, na.rm = TRUE)/1e6), by = year]
annual_fix[, variant := "Fixed 2015 population (storms only)"]

# ---- historical annual total (loop event CSVs; read only exposed_pop) ----
hfiles <- list.files(hist_dir, pattern = "_hist\\.csv$", full.names = TRUE)
cat("Historical event files:", length(hfiles), "\n")
agg_one <- function(f) {
  yr <- as.integer(substr(basename(f), 1, 4))
  dt <- tryCatch(fread(f, select = "exposed_pop", showProgress = FALSE), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(data.table(year = yr, pop = 0))
  data.table(year = yr, pop = sum(dt$exposed_pop, na.rm = TRUE))
}
hl <- rbindlist(lapply(hfiles, agg_one))
annual_hist <- hl[, .(pop_M = sum(pop, na.rm = TRUE)/1e6), by = year]
annual_hist[, variant := "Year-matched population (actual)"]

# ---- validation ----
cat("\n[validation] 1950 affected (M): hist =", round(annual_hist[year==1950, pop_M],1),
    " fix2015 =", round(annual_fix[year==1950, pop_M],1), "(hist should be << fix2015)\n")
cat("[validation] 2013 affected (M): hist =", round(annual_hist[year==2013, pop_M],1),
    " fix2015 =", round(annual_fix[year==2013, pop_M],1), "(should be similar by ~2015)\n")

dat <- rbind(annual_fix, annual_hist)
dat[, variant := factor(variant, levels = c("Year-matched population (actual)",
                                            "Fixed 2015 population (storms only)"))]
setorder(dat, variant, year)

# ---- decadal means for clean trend lines ----
dat[, decade := (year %/% 10) * 10]

cols <- c("Year-matched population (actual)" = "#B91C1C",
          "Fixed 2015 population (storms only)" = "#1D4ED8")

p <- ggplot(dat, aes(year, pop_M, color = variant, fill = variant)) +
  geom_line(alpha = 0.30, linewidth = 0.4) +                                   # raw annual
  geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 1.4) +     # trend
  geom_vline(xintercept = 1980, linetype = "dashed", color = "grey45", linewidth = 0.3) +
  annotate("text", x = 1981, y = Inf, label = "satellite era", hjust = 0, vjust = 1.6,
           size = 3.1, color = "grey45", family = ff) +
  scale_color_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = seq(1950, 2010, 10), expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_comma(suffix = " M"), expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "People affected per year (millions)") +
  theme_minimal(base_size = 13, base_family = ff) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom")

ggsave(file.path("results/R_output/figure_pop_decomposition.png"), p, width = 8.5, height = 4.7, dpi = 300, bg = "transparent")
cat("\nSaved figure_pop_decomposition.png\n")

# decadal summary for the talk note
dec <- dat[, .(pop_M = sum(pop_M)/10), by = .(decade, variant)]   # mean per year within decade
cat("\nDecadal MEAN affected/year (M):\n")
print(dcast(dec, decade ~ variant, value.var = "pop_M")[, lapply(.SD, function(x) round(x,1))])
