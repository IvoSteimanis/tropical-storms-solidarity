# 03_build_map.R — Global TCE-DAT figure for NCC

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(haven)
  library(patchwork)
  library(scales)
})

root <- "data/tce_dat"
ff <- "sans"

# ---- Load data ----
cells <- as.data.table(readRDS(file.path(root, "processed/global_cells_aggregated.rds")))

cells[, regime := factor(regime,
  levels = c("Tropical Storm only", "Low (Cat 1)",
             "Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)",
             "Catastrophic (Cat 5)"))]

cells[, regime_group := fcase(
  regime == "Tropical Storm only", "Tropical Storm",
  regime == "Low (Cat 1)", "Cat 1",
  regime %in% c("Moderate (Cat 2-4)", "Moderate, repeated (>=3 events)"), "Cat 2-4",
  regime == "Catastrophic (Cat 5)", "Cat 5"
)]
# geom_area stacks last level at bottom; severity order = TS -> Cat1 -> Cat2-4 -> Cat5
cells[, regime_group := factor(regime_group,
  levels = c("Cat 5", "Cat 2-4", "Cat 1", "Tropical Storm"))]

# ---- Base layers ----
world <- ne_countries(scale = 50, returnclass = "sf")

regime_colors <- c(
  "Tropical Storm only"               = "#7BAF6B",
  "Low (Cat 1)"                       = "#E8A020",
  "Moderate (Cat 2-4)"                = "#F08AB0",
  "Moderate, repeated (>=3 events)"   = "#DC267F",
  "Catastrophic (Cat 5)"              = "#1A1A1A"
)

regime_group_colors <- c(
  "Cat 2-4"        = "#DC267F",
  "Cat 1"          = "#E8A020",
  "Tropical Storm" = "#7BAF6B",
  "Cat 5"          = "#1A1A1A"
)

# ---- Load Haiyan track + study villages ----
ty <- read_dta(file.path("data/map/tycoord1.dta"))
names(ty) <- c("id", "X", "Y", "windspeed", "units", "gusts")
ty_sf <- st_as_sf(ty, coords = c("X", "Y"), crs = 4326)
ty_line <- ty %>%
  { st_linestring(as.matrix(.[, c("X", "Y")])) } %>%
  st_sfc(crs = 4326) %>% st_sf(geometry = .)

villages <- read_dta(file.path("data/map/StudySitePoints.dta"))
names(villages) <- c("id", "X", "Y", "pickone")
villages_sf <- st_as_sf(villages, coords = c("X", "Y"), crs = 4326)

ty_line_utm <- st_transform(ty_line, crs = 32651)
buf40 <- st_buffer(ty_line_utm, dist = 40000) %>% st_transform(4326)
buf80 <- st_buffer(ty_line_utm, dist = 80000) %>% st_transform(4326)
buf_moderate <- st_difference(buf80, buf40)

# ---- Panel (b): Panay study area with calibrated wind regime bands ----
panay_bbox <- c(xmin = 120.5, ymin = 9.5, xmax = 124.5, ymax = 12.5)

# Build concentric buffer rings from calibrated wind profile
# Distances (km) where each SS category boundary falls:
buf_cat1  <- st_buffer(ty_line_utm, dist = 126500) %>% st_transform(4326)  # 64 kn
buf_cat2  <- st_buffer(ty_line_utm, dist =  94500) %>% st_transform(4326)  # 83 kn
buf_cat3  <- st_buffer(ty_line_utm, dist =  73500) %>% st_transform(4326)  # 96 kn
buf_cat4  <- st_buffer(ty_line_utm, dist =  54500) %>% st_transform(4326)  # 113 kn
buf_cat5  <- st_buffer(ty_line_utm, dist =  23500) %>% st_transform(4326)  # 137 kn

# Create annulus rings (outer minus inner) for each regime
ring_ts   <- st_difference(buf_cat1, buf_cat2)    # TS: 94.5-126.5 km (64-83 kn)
ring_cat1 <- st_difference(buf_cat2, buf_cat3)    # Cat 1 not used — Cat 2-4 spans 23.5-94.5 km
ring_cat24 <- st_difference(buf_cat2, buf_cat5)   # Cat 2-4: 23.5-94.5 km (83-137 kn)
ring_cat5 <- buf_cat5                              # Cat 5: 0-23.5 km (>=137 kn)

p_a <- ggplot() +
  geom_sf(data = world, fill = "#EDEDED", color = "#B0B0B0", linewidth = 0.3) +
  # Wind regime bands (bottom to top: TS, Cat 2-4, Cat 5)
  geom_sf(data = ring_ts, fill = "#7BAF6B", alpha = 0.30, color = NA) +
  geom_sf(data = ring_cat24, fill = "#DC267F", alpha = 0.25, color = NA) +
  geom_sf(data = ring_cat5, fill = "#1A1A1A", alpha = 0.20, color = NA) +
  # Dashed boundaries at key thresholds
  geom_sf(data = st_boundary(buf_cat2), color = "#DC267F", linetype = "dashed", linewidth = 0.4) +
  geom_sf(data = st_boundary(buf_cat5), color = "#1A1A1A", linetype = "dashed", linewidth = 0.4) +
  geom_sf(data = world, fill = NA, color = "#B0B0B0", linewidth = 0.3) +
  # Haiyan track
  geom_sf(data = ty_line, color = "#B91C1C", linewidth = 1.2) +
  geom_sf(data = ty_sf, color = "#B91C1C", size = 2.0) +
  # Study villages on top
  geom_sf(data = villages_sf, color = "#1D4ED8", size = 2.5, shape = 16) +
  scale_fill_manual(values = regime_group_colors, guide = "none", drop = FALSE) +
  # Labels for wind regime bands
  annotate("text", x = 122.5, y = 11.6, label = "Panay Is.",
           size = 4, fontface = "italic", color = "grey30", family = ff) +
  annotate("text", x = 121.2, y = 10.2, label = "Cat 2-4\n(83-137 kn)",
           size = 3, color = "#DC267F", fontface = "bold", family = ff, lineheight = 0.85) +
  annotate("text", x = 123.7, y = 12.0, label = "Cat 5\n(>=137 kn)",
           size = 2.8, color = "#1A1A1A", fontface = "bold", family = ff, lineheight = 0.85) +
  coord_sf(xlim = panay_bbox[c("xmin", "xmax")],
           ylim = panay_bbox[c("ymin", "ymax")], expand = FALSE) +
  theme_void(base_family = ff) +
  theme(
    panel.background = element_rect(fill = "#DEEAF0", color = NA),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(3, 3, 3, 3)
  )

# ---- Panel (b): Global map ----

p_b <- ggplot() +
  geom_sf(data = world, fill = "#EDEDED", color = "#B0B0B0", linewidth = 0.15) +
  geom_tile(data = cells, aes(x = LON, y = LAT, fill = regime_group),
            width = 0.2, height = 0.2) +
  geom_sf(data = world, fill = NA, color = "#B0B0B0", linewidth = 0.15) +
  annotate("rect", xmin = panay_bbox["xmin"], xmax = panay_bbox["xmax"],
                   ymin = panay_bbox["ymin"], ymax = panay_bbox["ymax"],
           color = "black", fill = NA, linewidth = 0.6) +
  scale_fill_manual(
    values = regime_group_colors,
    name = NULL,
    breaks = c("Tropical Storm", "Cat 1", "Cat 2-4", "Cat 5"),
    labels = c("Tropical Storm (34-63 kn)",
               "Cat 1 (64-82 kn)",
               "Cat 2-4 (83-136 kn)",
               "Cat 5 (≥137 kn)"),
    drop = FALSE,
    guide = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-40, 47), expand = FALSE) +
  ggtitle("a") +
  theme_void(base_size = 12, base_family = ff) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 2)),
    panel.background = element_rect(fill = "#DEEAF0", color = NA),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 4, 2, 4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.45, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(t = 2, b = 0)
  )

# ---- Panel (c): Cumulative chart ----
annual_unique <- cells[, .(pop_M = sum(exposed_pop, na.rm = TRUE) / 1e6),
                        by = .(year = year_first_event, regime_group)]

all_years <- seq(min(cells$year_first_event), max(cells$year_first_event))
all_groups <- levels(cells$regime_group)
grid <- CJ(year = all_years, regime_group = factor(all_groups, levels = all_groups))
annual_full <- merge(grid, annual_unique, by = c("year", "regime_group"), all.x = TRUE)
annual_full[is.na(pop_M), pop_M := 0]
setorder(annual_full, regime_group, year)
annual_full[, cum_pop_M := cumsum(pop_M), by = regime_group]

cat24_total <- round(sum(cells[regime_group == "Cat 2-4", exposed_pop], na.rm = TRUE) / 1e6)

p_c <- ggplot(annual_full, aes(x = year, y = cum_pop_M, fill = regime_group)) +
  geom_area(alpha = 0.9, linewidth = 0.25, color = "white") +
  scale_fill_manual(
    values = regime_group_colors, name = NULL,
    breaks = c("Tropical Storm", "Cat 1", "Cat 2-4", "Cat 5"),
    guide = "none") +
  scale_x_continuous(breaks = seq(1950, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_comma(suffix = " M"),
                     breaks = seq(0, 2500, by = 500),
                     expand = expansion(mult = c(0, 0.05))) +
  # Cat 2-4 band sits above TS (~1245M) and Cat 1 (~589M) in severity stacking
  annotate("text", x = 2002, y = 2100,
           label = paste0("Cat 2-4\n", cat24_total, " M"),
           size = 5, fontface = "bold", color = "white", lineheight = 0.85,
           family = ff) +
  labs(x = NULL, y = "Cumulative population exposed (millions)") +
  ggtitle("c") +
  theme_minimal(base_size = 13, base_family = ff) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 2)),
    axis.title.y = element_text(size = 12, color = "grey20", margin = margin(r = 6)),
    axis.text = element_text(size = 11, color = "grey25"),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(6, 8, 2, 6)
  )

# ---- Panel (a) title ----
p_a <- p_a + ggtitle("b") +
  theme(plot.title = element_text(face = "bold", size = 14, margin = margin(b = 1)))

# ---- Compose: (a) small top-left, (b) full-width map, (c) full-width chart ----
# (b) global map on top full width, (a) study area + (c) chart side-by-side below
bottom_row <- p_a + p_c + plot_layout(widths = c(1, 2))
fig <- p_b / bottom_row + plot_layout(heights = c(1, 1.1))

# ---- Headline numbers ----
mod_all_M <- round(sum(cells[regime_group == "Cat 2-4", exposed_pop], na.rm = TRUE) / 1e6, 1)
mod_lmic_M <- round(sum(cells[regime_group == "Cat 2-4" &
  income_tier %in% c("Lower-middle", "Low"), exposed_pop], na.rm = TRUE) / 1e6, 1)
mod_phl_M <- round(sum(cells[regime_group == "Cat 2-4" & ISO == "PHL", exposed_pop], na.rm = TRUE) / 1e6, 1)
grand_total <- round(sum(cells$exposed_pop, na.rm = TRUE) / 1e6, 1)

cat("\n=== HEADLINE NUMBERS (GLOBAL) ===\n")
cat("Total population in any TC-exposed cell:               ", grand_total, "M\n", sep = "")
cat("Population in Cat 2-4 regime (globally):               ", mod_all_M, "M\n", sep = "")
cat("  of which in low/lower-middle-income countries:        ", mod_lmic_M, "M\n", sep = "")
cat("Philippines population in Cat 2-4 regime:              ", mod_phl_M, "M\n", sep = "")

# ---- Save ----
out_png <- file.path("results/R_output/figure_tce_dat_map.png")
ggsave(out_png, fig, width = 10, height = 8, dpi = 300, bg = "white")
cat("\nSaved:\n", out_png, "\n", sep = "")
