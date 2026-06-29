# 06_exposure_by_category_frequency.R
# Talk figure (first draft; possibly paper later): how the TC-exposed population
# splits ACROSS storm categories and HOW OFTEN people in the Cat 2-4 regime are hit.
# Expresses both in RELATIVE terms (shares) alongside absolute millions.
# Reuses global_cells_aggregated.rds; palette matches figure_tce_dat_map.png.
# Output: results/R_output/figure_exposure_category_frequency.png

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

root <- "data/tce_dat"
ff <- "sans"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
mod_regimes <- c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)")

cat_colors <- c("Tropical Storm" = "#7BAF6B", "Cat 1" = "#E8A020",
                "Cat 2-4" = "#DC267F", "Cat 5" = "#1A1A1A")
freq_colors <- c("Once" = "#F4A9C6", "Twice" = "#F08AB0", "3+ times" = "#DC267F")

lab_M_pct <- function(M, pct) sprintf("%s M\n(%.0f%%)", comma(round(M)), pct)

theme_bars <- theme_minimal(base_size = 13, base_family = ff) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey35"))

# ---- Panel A: by maximum storm category ----
total <- sum(cells$exposed_pop, na.rm = TRUE)
A <- cells[, .(pop_M = sum(exposed_pop, na.rm = TRUE) / 1e6), by = regime][
  , regime_group := fcase(
      regime == "Tropical Storm only", "Tropical Storm",
      regime == "Low (Cat 1)", "Cat 1",
      regime %in% mod_regimes, "Cat 2-4",
      regime == "Catastrophic (Cat 5)", "Cat 5")][
  , .(pop_M = sum(pop_M)), by = regime_group]
A[, pct := 100 * pop_M / (total/1e6)]
A[, regime_group := factor(regime_group, levels = c("Tropical Storm","Cat 1","Cat 2-4","Cat 5"))]
setorder(A, regime_group)

pA <- ggplot(A, aes(regime_group, pop_M, fill = regime_group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = lab_M_pct(pop_M, pct)), vjust = -0.25, size = 3.6,
            lineheight = 0.85, family = ff) +
  scale_fill_manual(values = cat_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = "Who lives with tropical-cyclone winds",
       subtitle = "Global population by maximum category ever experienced (1950-2015, 2015 baseline)",
       y = "Population (millions)") +
  theme_bars

# ---- Panel B: repeat exposure within the Cat 2-4 regime ----
mod <- cells[regime %in% mod_regimes]
mod[, band := fcase(n_events_cat2to4 <= 1, "Once",
                    n_events_cat2to4 == 2, "Twice",
                    n_events_cat2to4 >= 3, "3+ times")]
B <- mod[, .(pop_M = sum(exposed_pop, na.rm = TRUE) / 1e6), by = band]
B[, pct := 100 * pop_M / sum(pop_M)]
B[, band := factor(band, levels = c("Once","Twice","3+ times"))]
setorder(B, band)

pB <- ggplot(B, aes(band, pop_M, fill = band)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = lab_M_pct(pop_M, pct)), vjust = -0.25, size = 3.6,
            lineheight = 0.85, family = ff) +
  scale_fill_manual(values = freq_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = "...and how often",
       subtitle = "Cat 2-4 regime (758 M): times exposed to Cat 2-4 winds",
       y = NULL) +
  theme_bars

fig <- pA + pB + plot_layout(widths = c(4, 3))

out_png <- file.path("results/R_output/figure_exposure_category_frequency.png")
ggsave(out_png, fig, width = 11, height = 4.3, dpi = 300, bg = "transparent")

cat("Saved:", out_png, "\n")
cat("\nPanel A (by category):\n"); print(A[, .(regime_group, pop_M = round(pop_M,1), pct = round(pct,1))])
cat("\nPanel B (frequency):\n");  print(B[, .(band, pop_M = round(pop_M,1), pct = round(pct,1))])
