# 09_global_map_only.R
# Haiyan-free motivation visual: just the GLOBAL exposure map (panel b of
# figure_tce_dat_map), without the Panay study-area box or zoom panels.
# Coastal 0.1-degree cells colored by the MAXIMUM Saffir-Simpson category
# experienced 1950-2015. Palette identical to figure_tce_dat_map.png.
# Output: results/R_output/figure_global_map_only.png

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(rnaturalearth)
})

root <- "data/tce_dat"; ff <- "sans"

cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))
cells[, regime_group := fcase(
  regime == "Tropical Storm only", "Tropical Storm",
  regime == "Low (Cat 1)", "Cat 1",
  regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), "Cat 2-4",
  regime == "Catastrophic (Cat 5)", "Cat 5")]
# draw weak first so intense cells sit on top
sev <- c("Tropical Storm" = 1, "Cat 1" = 2, "Cat 2-4" = 3, "Cat 5" = 4)
cells[, sev := sev[regime_group]]
setorder(cells, sev)
cells[, regime_group := factor(regime_group, levels = c("Cat 5","Cat 2-4","Cat 1","Tropical Storm"))]

regime_group_colors <- c("Cat 2-4" = "#DC267F", "Cat 1" = "#E8A020",
                         "Tropical Storm" = "#7BAF6B", "Cat 5" = "#1A1A1A")

world <- ne_countries(scale = 50, returnclass = "sf")

p <- ggplot() +
  geom_sf(data = world, fill = "#EDEDED", color = "#B0B0B0", linewidth = 0.15) +
  geom_tile(data = cells, aes(x = LON, y = LAT, fill = regime_group),
            width = 0.2, height = 0.2) +
  geom_sf(data = world, fill = NA, color = "#B0B0B0", linewidth = 0.15) +
  scale_fill_manual(
    values = regime_group_colors, name = NULL,
    breaks = c("Tropical Storm", "Cat 1", "Cat 2-4", "Cat 5"),
    labels = c("Tropical Storm (34-63 kn)", "Cat 1 (64-82 kn)",
               "Cat 2-4 (83-136 kn)", "Cat 5 (>=137 kn)"),
    drop = FALSE, guide = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-40, 47), expand = FALSE) +
  theme_void(base_size = 12, base_family = ff) +
  theme(panel.background = element_rect(fill = "#DEEAF0", color = NA),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(2, 4, 2, 4),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 11), legend.key.size = unit(0.5, "cm"))

ggsave(file.path("results/R_output/figure_global_map_only.png"), p, width = 12, height = 4.2, dpi = 300, bg = "white")
cat("Saved figure_global_map_only.png\n")
