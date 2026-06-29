# 13_figure2_3_maintext.R
# NCC-styled rebuild of main-text Figure 2 (and later Figure 3) in ggplot2,
# matching the Nature theme of 11_combined_main_figure.R (sans font, IBM
# colorblind palette, bold lowercase panel tags, patchwork, 300 dpi).
# Plots Stata margins exported to CSV (analysis stays authoritative in Stata).
# Inputs:  results/intermediate/fig2_panelA_margins.csv, fig2_panelB_margins.csv
# Output:  results/figures/figure2_field_ushape_windspeed_R.png  (PoC)

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(patchwork)
})

# Paths are relative to the replication_package/ root (set working dir there,
# as run_R.R requires). No hardcoded user directories.
INT  <- "results/intermediate"
OUT  <- "results/figures"
ff   <- "sans"

# shared palette (same family as Fig 1)
line_col  <- "#235789"   # deep blue line
band_col  <- "#648FFF"   # IBM blue ribbon
tp_col    <- "#DC267F"   # magenta = moderate-exposure accent (ties to Fig 1)
cat_col   <- "#1A1A1A"   # categorical bin means (neutral, distinct from tp magenta)
ss_breaks <- c(83, 96, 113, 137)   # Saffir-Simpson Cat 2/3/4/5 lower bounds (kn)

theme_ncc <- function(base = 9) {
  theme_classic(base_size = base, base_family = ff) %+replace%
    theme(
      plot.title       = element_text(face = "plain", size = base + 1, hjust = 0,
                                      margin = margin(b = 4)),   # caption: regular weight
      plot.tag         = element_text(face = "bold", size = base + 3, family = ff),  # panel letter: bold
      plot.tag.position = c(0.01, 1.0),
      axis.title       = element_text(size = base),
      axis.text        = element_text(size = base - 1, colour = "grey20"),
      axis.line        = element_line(colour = "grey30", linewidth = 0.3),
      axis.ticks       = element_line(colour = "grey30", linewidth = 0.3),
      plot.margin      = margin(4, 8, 2, 4),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

make_panel <- function(csv, tag, title, ytitle, ylim, ybreaks,
                       tp, tp_lab, lab_y, cat_prefix) {
  d <- fread(csv)
  setnames(d, c("_at1", "_margin", "_ci_lb", "_ci_ub"),
           c("ws", "est", "lo", "hi"), skip_absent = TRUE)
  # Three wind-speed exposure categories used in the text (low/medium/high;
  # Stata margins), placed at each category's mean wind speed and labelled by
  # n observations. Overlaid on the continuous fit so the figure and text match.
  cm <- fread(file.path(INT, sprintf("fig2_cat%s_bin3.csv", cat_prefix)))  # b, ll, ul, bin
  bw <- fread(file.path(INT, "fig2_bins_bin3.csv"))                        # bin, ws, n_vill
  no <- fread(file.path(INT, sprintf("fig2_nobs%s_bin3.csv", cat_prefix))) # bin, n_obs
  cc <- merge(merge(cm, bw[, .(bin, ws)], by = "bin"), no, by = "bin")
  ggplot(d, aes(ws, est)) +
    geom_vline(xintercept = ss_breaks, linetype = "dotted",
               colour = "grey80", linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = tp, linetype = "dashed", colour = tp_col,
               linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = band_col, alpha = 0.16) +
    geom_line(colour = line_col, linewidth = 0.8) +
    geom_errorbar(data = cc, aes(x = ws, ymin = ll, ymax = ul), width = 2.5,
                  colour = cat_col, linewidth = 0.4, inherit.aes = FALSE) +
    geom_point(data = cc, aes(x = ws, y = b), colour = cat_col, size = 1.6,
               inherit.aes = FALSE) +
    geom_text(data = cc, aes(x = ws, y = ul, label = paste0("n=", n_obs)),
              colour = cat_col, size = 1.8, family = ff, vjust = -0.7,
              inherit.aes = FALSE) +
    annotate("text", x = tp + 1.5, y = lab_y,
             label = tp_lab, colour = tp_col, size = 3.0, hjust = 0,
             family = ff, lineheight = 0.95) +
    scale_x_continuous(breaks = seq(70, 150, 20)) +
    scale_y_continuous(breaks = ybreaks) +
    coord_cartesian(xlim = c(68, 147), ylim = ylim) +
    labs(title = title, x = "Sustained wind speed (knots)", y = ytitle) +
    theme_ncc()
}

# Panel C: solidarity response by economic vulnerability (continuous index).
# Predicted curves at low/average/high vulnerability (-1/0/+1 SD of the baseline
# coping-PCA index). The U-shape deepens with vulnerability; the continuous
# interaction is suggestive, not conclusive (F(2,29)=2.45, p=0.10; the coarser
# tertile split is in SI Table S20).
need_cols <- c("Less vulnerable" = "#7FB0E0", "Average" = "#FE6100", "More vulnerable" = "#DC267F")

make_panel_need <- function(csv, title, ytitle, ylim, ybreaks) {
  d <- fread(csv)
  setnames(d, c("_at1", "_margin", "_ci_lb", "_ci_ub", "_m1"),
           c("ws", "est", "lo", "hi", "grp"), skip_absent = TRUE)
  d <- d[!is.na(ws)]
  d[, grp := factor(grp, levels = c("Less vulnerable", "Average", "More vulnerable"))]
  # Predicted curves at -1/0/+1 SD of the continuous vulnerability index; the
  # more-vulnerable curve is emphasized (deepest U). Light bands reflect the
  # suggestive, underpowered interaction (p = 0.10).
  ggplot(d, aes(ws, colour = grp, fill = grp)) +
    geom_vline(xintercept = ss_breaks, linetype = "dotted",
               colour = "grey80", linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, colour = NA) +
    geom_line(aes(y = est, linewidth = grp)) +
    scale_colour_manual(values = need_cols, name = NULL) +
    scale_fill_manual(values = need_cols, guide = "none") +
    scale_linewidth_manual(
      values = c("Less vulnerable" = 0.45, "Average" = 0.45, "More vulnerable" = 1.0),
      guide = "none") +
    scale_x_continuous(breaks = seq(70, 150, 20)) +
    scale_y_continuous(breaks = ybreaks) +
    coord_cartesian(xlim = c(68, 147), ylim = ylim) +
    labs(title = title, x = "Sustained wind speed (knots)", y = ytitle) +
    theme_ncc() +
    theme(legend.position = "inside", legend.position.inside = c(0.5, 0.015),
          legend.justification = c(0.5, 0), legend.direction = "horizontal",
          legend.text = element_text(size = 6.3),
          legend.key.size = unit(7, "pt"), legend.key.spacing.x = unit(1, "pt"),
          legend.background = element_rect(fill = "white", colour = NA))
}

pA <- make_panel(file.path(INT, "fig2_panelA_margins.csv"), "a",
                 "Solidarity transfers (3 years)",
                 expression(Delta * " Transfers (PHP)"),
                 ylim = c(-15, 5), ybreaks = seq(-15, 5, 5),
                 tp = 113, tp_lab = "Turning point\n≈ 113 kn (Cat 4)", lab_y = -13,
                 cat_prefix = "A")

pB <- make_panel(file.path(INT, "fig2_panelB_margins.csv"), "b",
                 "Reciprocity (9 years)",
                 "Reciprocity (SD)",
                 ylim = c(-0.5, 0.5), ybreaks = seq(-0.5, 0.5, 0.25),
                 tp = 101, tp_lab = "Turning point\n≈ 101 kn (Cat 3)", lab_y = -0.42,
                 cat_prefix = "B")

pC <- make_panel_need(file.path(INT, "fig2_panelC_vuln.csv"),
                      "By economic vulnerability",
                      expression(Delta * " Transfers (PHP)"),
                      ylim = c(-16, 11), ybreaks = seq(-15, 10, 5))

# Main-text Figure 2: two panels (a, b). Heterogeneity (former panel c) moved to the SI.
fig2 <- pA + pB + plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12, family = ff))

ggsave(file.path(OUT, "figure2_field_ushape_windspeed_R.png"), fig2,
       width = 160, height = 76, units = "mm", dpi = 300, bg = "white")

# SI figure: heterogeneity by economic vulnerability (former main-text Fig. 2c)
ggsave(file.path(OUT, "figureS_het_vulnerability.png"), pC,
       width = 110, height = 82, units = "mm", dpi = 300, bg = "white")
cat("wrote figure2_field_ushape_windspeed_R.png\n")

# =====================================================================
# Figure 3 — mechanisms (4 panels): lab U-shape, diffusion, ambiguity
# gap, aid satisfaction. Same Nature theme as Figure 2.
# Inputs: fig3_panelAB_curve.csv, fig3_panelB_points.csv,
#         fig3_panelC_lines.csv, fig3_panelC_gaps.csv,
#         fig3_panelD_curves.csv, fig3_scalars.csv
# Output: results/figures/figure3_mechanisms_R.png
# =====================================================================

# extra colours (colourblind-safe, same family as Fig 1)
need_col  <- "#648FFF"   # blue  = stated need for aid
sev_col   <- "#DC267F"   # magenta = severe damage
band_purp <- "#785EF0"   # IBM purple = ambiguity-gap band
green_col <- "#117733"   # low diffusion (1 helper)
orange_col<- "#FE6100"   # high diffusion (2 helpers)
mean_col  <- "grey55"

sc  <- as.list(fread(file.path(INT, "fig3_scalars.csv")))
pct_lab <- function(x) paste0(x, "%")

# --- shared lab U-shape curve (panels a & b) ---
ab <- fread(file.path(INT, "fig3_panelAB_curve.csv"))
lab_curve <- function(faint = FALSE) {
  list(
    geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3),
    geom_ribbon(data = ab, aes(damages, ymin = lo, ymax = hi),
                fill = band_col, alpha = if (faint) 0.10 else 0.16,
                inherit.aes = FALSE),
    geom_line(data = ab, aes(damages, est), colour = line_col,
              linewidth = if (faint) 0.6 else 0.8,
              alpha = if (faint) 0.5 else 1, inherit.aes = FALSE)
  )
}
ab_scaffold <- list(
  scale_x_continuous(limits = c(-40, 440), breaks = seq(0, 400, 100)),
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), labels = pct_lab),
  labs(x = "Group damage severity", y = "Transfers (% of endowment)")
)

# Panel a: short-term transfers, turning point @220
pA3 <- ggplot() +
  annotate("segment", x = 220, xend = 220, y = 0, yend = 40,
           linetype = "dashed", colour = tp_col, linewidth = 0.4) +
  lab_curve() +
  annotate("text", x = 220, y = 42, hjust = 0.5, vjust = 0,
           label = "Turning point ≈ 220\n95% CI [180, 260]",
           colour = tp_col, size = 2.6, family = ff, lineheight = 0.95) +
  ab_scaffold +
  labs(title = "Lab: short-term transfers") +
  theme_ncc()

# Panel b: diffusion of responsibility (two annotated points + stats box)
pts <- fread(file.path(INT, "fig3_panelB_points.csv"))
pts[, grp := ifelse(damages == 200, "low", "high")]
diff_lab <- sprintf("Difference: %d pp [%d, %d], p = %.2f",
                    round(sc$diff), round(sc$diff_lo), round(sc$diff_hi), sc$diff_p)
pB3 <- ggplot() +
  lab_curve(faint = TRUE) +
  geom_point(data = pts, aes(damages, est, colour = grp), size = 3.2) +
  annotate("text", x = 214, y = 27, hjust = 0, vjust = 0.5, colour = green_col,
           size = 2.9, family = ff, lineheight = 0.95,
           label = "Low diffusion\n(1 helper): 20%") +
  annotate("text", x = 168, y = 9, hjust = 1, vjust = 0.5, colour = orange_col,
           size = 2.9, family = ff, lineheight = 0.95,
           label = "High diffusion\n(2 helpers): 2%") +
  annotate("text", x = 200, y = 57, hjust = 0.5, vjust = 1, colour = "grey20",
           size = 2.6, family = ff, label = diff_lab) +
  scale_colour_manual(values = c(low = green_col, high = orange_col), guide = "none") +
  ab_scaffold +
  labs(title = "Lab: diffusion of responsibility") +
  theme_ncc()

# Panel c: field ambiguity gap (two lpoly fits + shaded band + category gaps)
cl <- fread(file.path(INT, "fig3_panelC_lines.csv"))
cg <- fread(file.path(INT, "fig3_panelC_gaps.csv"))
pC3 <- ggplot(cl, aes(grid)) +
  geom_vline(xintercept = ss_breaks, linetype = "dotted", colour = "grey80", linewidth = 0.3) +
  geom_ribbon(aes(ymin = fit_sev, ymax = fit_need, fill = "Ambiguity gap"), alpha = 0.22) +
  geom_line(aes(y = fit_need, colour = "Need for aid"), linewidth = 0.7) +
  geom_line(aes(y = fit_sev, colour = "Severe damage"), linewidth = 0.7, linetype = "dashed") +
  geom_segment(data = cg, aes(x = xm, xend = xm, y = yb, yend = ya),
               colour = "grey35", linewidth = 0.3, inherit.aes = FALSE) +
  geom_text(data = cg, aes(x = xm + 1, y = ym, label = paste0(round(gap), " pp")),
            hjust = 0, size = 2.4, family = ff, colour = "grey15", inherit.aes = FALSE) +
  scale_colour_manual(name = NULL, values = c("Need for aid" = need_col, "Severe damage" = sev_col)) +
  scale_fill_manual(name = NULL, values = c("Ambiguity gap" = band_purp)) +
  scale_x_continuous(limits = c(68, 152), breaks = seq(70, 150, 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Field: ambiguity gap",
       x = "Sustained wind speed (knots)", y = "Share (%)") +
  theme_ncc() +
  theme(legend.text = element_text(size = 7), legend.key.size = unit(8, "pt"),
        legend.margin = margin(0, 0, 0, 0), legend.spacing.x = unit(2, "pt"),
        legend.position = "inside", legend.position.inside = c(0.99, 0.02),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", colour = NA))

# Aid-satisfaction panel (old panel d) dropped 2026-06-17: it fails the
# categorical robustness cut (severe-damage cells 12/12/139, severe means flat
# 53/57/55) and the quadratic dip at 107 kn is a functional-form artifact. The
# mechanism stands on the lab diffusion experiment (a, b) and the field
# ambiguity gap (c). See quality_reports/2026-06-16_mechanism-field-correlates.md
# and quality_reports/2026-06-17_toc-mediator-extension.md.

fig3 <- pA3 + pB3 + pC3 + plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12, family = ff),
        plot.title = element_text(size = 8))

ggsave(file.path(OUT, "figure3_mechanisms_R.png"), fig3,
       width = 180, height = 78, units = "mm", dpi = 300, bg = "white")
cat("wrote figure3_mechanisms_R.png\n")
