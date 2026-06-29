# 10_pop_decomposition_by_category.R
# By-CATEGORY version of the population-vs-storms decomposition, SATELLITE ERA ONLY
# (1980-2015), where best-track intensity/category assignment is reliable.
# Per Saffir-Simpson group, two curves: year-matched population (actual) vs
# fixed-2015 population (storms only). The gap is population growth.
#
# CAVEAT (annotated): category-specific SLOPES are NOT clean climate trends even
# post-1980 (35 yr << interdecadal variability; intensity still has uncertainty).
# The robust read is the population-vs-storms split WITHIN each category. The
# climate trend (rising share of intense storms) is the literature's (slide 5).
# fix2015 categories match 02_aggregate (TS 34-63, Cat 1 64-82, Cat 2-4 83-136, Cat 5 >=137).
#
# Output: results/R_output/figure_decomp_by_category_1980.png

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })

root <- "data/tce_dat"; ff <- "sans"
hist_dir <- file.path(root, "raw/extracted/events_historical/TCE-DAT_single_events_historical/TC_data")
YMIN <- 1980

# ---- fix2015 by category (already aggregated) ----
fix <- as.data.table(readRDS(file.path(root, "processed/global_annual_exposure.rds")))
fix <- fix[, .(year, cat_group, pop_M = person_exp/1e6)]
fix[, variant := "Fixed 2015 population (storms only)"]

# ---- historical by category (re-extract: windspeed -> category, sum exposed_pop) ----
hfiles <- list.files(hist_dir, pattern = "_hist\\.csv$", full.names = TRUE)
cat("Historical files:", length(hfiles), "\n")
agg_one <- function(f) {
  yr <- as.integer(substr(basename(f), 1, 4))
  d <- tryCatch(fread(f, select = c("windspeed", "exposed_pop"), showProgress = FALSE),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- d[windspeed >= 34]
  if (nrow(d) == 0) return(NULL)
  d[, cat_group := fcase(windspeed < 64, "Tropical Storm",
                         windspeed < 83, "Cat 1",
                         windspeed < 137, "Cat 2-4",
                         default = "Cat 5")]
  d[, .(year = yr, person_exp = sum(exposed_pop, na.rm = TRUE)), by = cat_group]
}
hl <- rbindlist(lapply(hfiles, agg_one))
hist <- hl[, .(pop_M = sum(person_exp, na.rm = TRUE)/1e6), by = .(year, cat_group)]
hist[, variant := "Year-matched population (actual)"]

dat <- rbind(fix, hist)[year >= YMIN]
dat[, cat_group := factor(cat_group, levels = c("Tropical Storm", "Cat 1", "Cat 2-4", "Cat 5"))]
dat[, variant := factor(variant, levels = c("Year-matched population (actual)",
                                            "Fixed 2015 population (storms only)"))]
setorder(dat, variant, cat_group, year)

cols <- c("Year-matched population (actual)" = "#B91C1C",
          "Fixed 2015 population (storms only)" = "#1D4ED8")

p <- ggplot(dat, aes(year, pop_M, color = variant)) +
  geom_line(alpha = 0.30, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.6, se = FALSE, linewidth = 1.2) +
  facet_wrap(~ cat_group, scales = "free_y", ncol = 2) +
  scale_color_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = seq(1980, 2010, 10), expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(labels = label_comma(suffix = " M"), expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = "People affected per year (millions)") +
  theme_minimal(base_size = 12, base_family = ff) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# ---- annotate the named Cat 5 events (the Cat 5 band is a handful of individual storms) ----
ann <- data.table(
  cat_group = factor("Cat 5", levels = c("Tropical Storm","Cat 1","Cat 2-4","Cat 5")),
  year  = c(1995, 1991, 2000, 2013),
  yval  = c(17.8, 1.3, 1.7, 3.08),
  label = c("Typhoon Angela (1995)", "Bangladesh\ncyclone (1991)", "Typhoon\nBilis (2000)", "Typhoon\nHaiyan (2013)"),
  hj    = c(1, 0.5, 0.5, 1),
  nx    = c(-0.7, 0, 0, 0.5),
  ny    = c(0, 4.2, 4.6, 3.8)
)
p <- p +
  geom_point(data = ann, aes(year, yval), inherit.aes = FALSE, color = "grey10", size = 1.5) +
  geom_text(data = ann, aes(year + nx, yval + ny, label = label, hjust = hj),
            inherit.aes = FALSE, size = 2.6, color = "grey10", family = ff, lineheight = 0.85)

ggsave(file.path("results/R_output/figure_decomp_by_category_1980.png"), p, width = 9, height = 5.6, dpi = 300, bg = "transparent")
cat("Saved figure_decomp_by_category_1980.png\n")

# decadal means per category/variant (note: 2010s is a partial decade 2010-2015)
dat[, decade := (year %/% 10) * 10]
nyr <- dat[, .(n = uniqueN(year)), by = decade]
dec <- dat[, .(tot = sum(pop_M)), by = .(decade, cat_group, variant)]
dec <- merge(dec, nyr, by = "decade")[, mean_M := round(tot/n, 0)]
cat("\nDecadal MEAN affected/yr by category (M):\n")
print(dcast(dec, decade + cat_group ~ variant, value.var = "mean_M"))
