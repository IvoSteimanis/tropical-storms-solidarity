# 14_figure5_impacts.R
# NCC-styled rebuild of the 6-panel damage / impact figure (Methods "Figure 5"),
# replacing the Stata swift_red version so it matches the ggplot Nature theme of
# Figures 1-4. The smoother is Stata's OWN lpoly fit (epanechnikov kernel,
# bwidth 6), exported to CSV by scripts/_export_fig5_curves.do, so the line is
# identical to the published figure_damage_windspeed_manuscript.png.
#
# Panels (outcome vs calibrated sustained wind speed):
#   a Houses damaged (share)            d Major life event in 2022 (share)
#   b Costs of damages (1,000 PHP)      e Damage inequality, within-village IQR (1,000 PHP)
#   c Reported need for help (share)    f IQR / mean damage (ratio)
# Points are village (session) means. Inputs: results/intermediate/fig5_<tag>_curve.csv
# (lpoly grid) and fig5_<tag>_pts.csv (village scatter).
# Output: results/figures/figure5_impacts_R.png  (183 mm x 122 mm, 300 dpi)

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(patchwork)
})
INT <- "results/intermediate"; OUT <- "results/figures"; ff <- "sans"
line_col <- "#235789"; pt_col <- "#648FFF"
ss_breaks <- c(83, 96, 113, 137)         # Saffir-Simpson Cat 2/3/4/5 lower bounds

# Saffir-Simpson band labels, drawn on panel a only (the figure note tells the
# reader the categories are labelled there). Bands: Cat 1 <83, Cat 2 83-95,
# Cat 3 96-112, Cat 4 113-136, Cat 5 >=137 kn; each label sits at its band
# midpoint within the plotted x range.
ss_labels <- function(xlo, xhi) {
  edges <- c(xlo, ss_breaks, xhi)
  data.frame(x = head(edges, -1) + diff(edges) / 2,
             lab = paste0("Cat ", 1:5))
}

theme_ncc <- function(base = 9) {
  theme_classic(base_size = base, base_family = ff) %+replace%
    theme(
      plot.title       = element_text(face = "plain", size = base, hjust = 0,
                                      margin = margin(b = 3)),
      plot.tag         = element_text(face = "bold", size = base + 3, family = ff),
      plot.tag.position = c(0.01, 1.0),
      axis.title       = element_text(size = base - 1),
      axis.text        = element_text(size = base - 2, colour = "grey20"),
      axis.line        = element_line(colour = "grey30", linewidth = 0.3),
      axis.ticks       = element_line(colour = "grey30", linewidth = 0.3),
      plot.margin      = margin(4, 8, 2, 4),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

mk_panel <- function(tag, title, ytitle, pct = FALSE, ylim = NULL, ybreaks = waiver(),
                     cats = FALSE) {
  curve <- fread(file.path(INT, sprintf("fig5_%s_curve.csv", tag)))
  setnames(curve, c("gx", "gy"), c("ws", "fit"))
  pts <- fread(file.path(INT, sprintf("fig5_%s_pts.csv", tag)))
  ylab_fun <- if (pct) function(z) paste0(round(z * 100), "%") else waiver()
  ggplot() +
    geom_vline(xintercept = ss_breaks, linetype = "dotted", colour = "grey80",
               linewidth = 0.3) +
    (if (cats) geom_text(data = ss_labels(68, 150), aes(x = x, y = Inf, label = lab),
                         inherit.aes = FALSE, vjust = 1.3, size = 1.9,
                         colour = "grey45", family = ff)) +
    geom_point(data = pts, aes(ws, y), colour = pt_col, alpha = 0.6, size = 1.2) +
    geom_line(data = curve, aes(ws, fit), colour = line_col, linewidth = 0.9) +
    scale_x_continuous(breaks = seq(70, 150, 20)) +
    scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylab_fun) +
    coord_cartesian(xlim = c(68, 150), ylim = ylim) +
    labs(title = title, x = "Sustained wind speed (knots)", y = ytitle) +
    theme_ncc()
}

pa <- mk_panel("a", "Houses damaged", "Share", pct = TRUE, ylim = c(0, 1), ybreaks = seq(0, 1, .2),
               cats = TRUE)
pb <- mk_panel("b", "Costs of damages", "1,000 PHP", ylim = c(0, 35), ybreaks = seq(0, 35, 5))
pc <- mk_panel("c", "Reported need for help", "Share", pct = TRUE, ylim = c(0, 1), ybreaks = seq(0, 1, .2))
pd <- mk_panel("d", "Major life event in 2022", "Share", pct = TRUE, ylim = c(0, 1), ybreaks = seq(0, 1, .2))
pe <- mk_panel("e", "Damage inequality (IQR)", "1,000 PHP")
pf <- mk_panel("f", "IQR / mean damage", "Ratio")

fig5 <- (pa + pb + pc) / (pd + pe + pf) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12, family = ff))

ggsave(file.path(OUT, "figure5_impacts_R.png"), fig5,
       width = 183, height = 122, units = "mm", dpi = 300, bg = "white")
cat("wrote figure5_impacts_R.png\n")
