# 11_combined_main_figure.R
# Combined main-text figure for the NCC manuscript (Figure 1 candidate).
#   Panel a: global exposure regime map (code copied from 09_global_map_only.R)
#   Panel b: population by maximum Saffir-Simpson category (from 06_exposure_by_category_frequency.R)
#   Panel c: repeat exposure within the Cat 2-4 regime (from 06_exposure_by_category_frequency.R)
# Layout: map full width on top; b + c side by side below; bold lowercase panel
# tags (Nature style). Palette/theme identical to the existing standalone figures;
# text sizes scaled down for the 183 mm double-column width.
# Inputs:  data/tce_dat/processed/global_cells_aggregated.rds
# Outputs: results/figures/figure1_global_combined.png (183 mm x 150 mm, 300 dpi)
# Depends: data.table, sf, ggplot2, rnaturalearth, patchwork, scales

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2)
  library(rnaturalearth); library(patchwork); library(scales)
})

root <- "data/tce_dat"; ff <- "sans"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
mod_regimes <- c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)")

# shared palette (identical to 06/09)
cat_colors  <- c("Tropical Storm" = "#7BAF6B", "Cat 1" = "#E8A020",
                 "Cat 2-4" = "#DC267F", "Cat 5" = "#1A1A1A")
freq_colors <- c("Once" = "#F4A9C6", "Twice" = "#F08AB0", "3+ times" = "#DC267F")

# ---------------------------------------------------------------------------
# Panel a: global regime map  (copied from R/09_global_map_only.R)
# ---------------------------------------------------------------------------
map_dt <- copy(cells)
map_dt[, regime_group := fcase(
  regime == "Tropical Storm only", "Tropical Storm",
  regime == "Low (Cat 1)", "Cat 1",
  regime %in% mod_regimes, "Cat 2-4",
  regime == "Catastrophic (Cat 5)", "Cat 5")]
# draw weak first so intense cells sit on top
sev <- c("Tropical Storm" = 1, "Cat 1" = 2, "Cat 2-4" = 3, "Cat 5" = 4)
map_dt[, sev := sev[regime_group]]
setorder(map_dt, sev)
map_dt[, regime_group := factor(regime_group, levels = c("Cat 5","Cat 2-4","Cat 1","Tropical Storm"))]

world <- ne_countries(scale = 50, returnclass = "sf")

p_map <- ggplot() +
  geom_sf(data = world, fill = "#EDEDED", color = "#B0B0B0", linewidth = 0.15) +
  geom_tile(data = map_dt, aes(x = LON, y = LAT, fill = regime_group),
            width = 0.2, height = 0.2) +
  geom_sf(data = world, fill = NA, color = "#B0B0B0", linewidth = 0.15) +
  scale_fill_manual(
    values = cat_colors, name = NULL,
    breaks = c("Tropical Storm", "Cat 1", "Cat 2-4", "Cat 5"),
    labels = c("Tropical Storm (34-63 kn)", "Cat 1 (64-82 kn)",
               "Cat 2-4 (83-136 kn)", "Cat 5 (>=137 kn)"),
    drop = FALSE, guide = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-40, 47), expand = FALSE) +
  theme_void(base_size = 8, base_family = ff) +
  theme(panel.background = element_rect(fill = "#DEEAF0", color = NA),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(2, 4, 0, 4),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.32, "cm"),
        legend.margin = margin(t = 2, b = 0))

# ---------------------------------------------------------------------------
# Panels b + c  (copied from R/06_exposure_by_category_frequency.R;
# text sizes reduced from the 11-in standalone version to fit 183 mm)
# ---------------------------------------------------------------------------
lab_M_pct <- function(M, pct) sprintf("%s M\n(%.0f%%)", comma(round(M)), pct)

theme_bars <- theme_minimal(base_size = 8.5, base_family = ff) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 8.5),
        plot.subtitle = element_text(size = 6.5, color = "grey35"))

# Panel b: by maximum storm category
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

pB <- ggplot(A, aes(regime_group, pop_M, fill = regime_group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = lab_M_pct(pop_M, pct)), vjust = -0.25, size = 2.3,
            lineheight = 0.85, family = ff) +
  scale_fill_manual(values = cat_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = comma) +
  labs(title = "Who lives with tropical-cyclone winds",
       subtitle = "Population by maximum category experienced\n(1950-2015, 2015 baseline)",
       y = "Population (millions)") +
  theme_bars

# Panel c: repeat exposure within the Cat 2-4 regime
mod <- cells[regime %in% mod_regimes]
mod[, band := fcase(n_events_cat2to4 <= 1, "Once",
                    n_events_cat2to4 == 2, "Twice",
                    n_events_cat2to4 >= 3, "3+ times")]
B <- mod[, .(pop_M = sum(exposed_pop, na.rm = TRUE) / 1e6), by = band]
B[, pct := 100 * pop_M / sum(pop_M)]
B[, band := factor(band, levels = c("Once","Twice","3+ times"))]
setorder(B, band)

pC <- ggplot(B, aes(band, pop_M, fill = band)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = lab_M_pct(pop_M, pct)), vjust = -0.25, size = 2.3,
            lineheight = 0.85, family = ff) +
  scale_fill_manual(values = freq_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = comma) +
  labs(title = "...and how often",
       subtitle = "Cat 2-4 regime (758 M): times exposed\nto Cat 2-4 winds",
       y = NULL) +
  theme_bars

# ---------------------------------------------------------------------------
# Assemble: map (full width) over b | c, with bold lowercase Nature-style tags
# ---------------------------------------------------------------------------
fig <- p_map / (pB + pC + plot_layout(widths = c(4, 3))) +
  plot_layout(heights = c(0.78, 1.22)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 10, family = ff),
        plot.tag.position = c(0, 1))

# NCC double column: 183 mm wide. Height 130 mm with heights c(0.78, 1.22):
# at this width the world map's fixed coord_sf aspect needs ~50 mm incl. legend,
# so the top slot is sized to match and the letterbox white space disappears.
w_in <- 183 / 25.4
h_in <- 130 / 25.4
out_png <- file.path("results/figures/figure1_global_combined.png")
ggsave(out_png, fig, width = w_in, height = h_in, dpi = 300, bg = "white")

cat("Saved:", out_png, "\n")
cat("\nPanel b (by category):\n"); print(A[, .(regime_group, pop_M = round(pop_M,1), pct = round(pct,1))])
cat("\nPanel c (frequency):\n");   print(B[, .(band, pop_M = round(pop_M,1), pct = round(pct,1))])
