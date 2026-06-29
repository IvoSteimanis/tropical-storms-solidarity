# 12_study_site_figure.R
# Manuscript Figure 4 (study site), replacing the non-reproducible PowerPoint figure.
#   Panel a: Philippines overview; historical super-typhoon tracks 1950-2015
#            (max sustained wind >= 108 kn ~ >200 km/h somewhere along track,
#            passing near the Philippines), Typhoon Haiyan highlighted, study
#            area (Panay Island, Western Visayas) boxed.
#   Panel b: Panay zoom; municipality boundaries, 30 study villages, Haiyan
#            track, exposure as calibrated sustained-wind bands with
#            Saffir-Simpson breaks (Cat 2 83 kn, Cat 3 96, Cat 4 113, Cat 5 137).
#
# Wind-field reconstruction: the paper's calibrated village wind speed
# (data/windspeed_predicted.dta) is a strictly monotone (Spearman = -1)
# deterministic function of distance to the Haiyan track
# (data/distance_storm_km.dta). We therefore fit a monotone spline V(r) on the
# 30 (distance, windspeed) pairs and evaluate it on a land grid's distance to
# the IBTrACS best track. Village points carry the paper's exact calibrated
# values via a distance-rank join (validated below; computed vs paper distances
# agree to <6 km, rank-safe because near-tied distances have near-tied winds).
#
# Inputs:
#   data/tce_dat/raw/ibtracs.WP.list.v04r01.csv
#     IBTrACS v04r01 Western Pacific, NOAA NCEI; downloaded 2026-06-11 from
#     https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.WP.list.v04r01.csv
#     (113.9 MB; auto-downloaded by this script if missing)
#   data/map/muncoord.dta           (spmap polygon coords, 100 municipalities)
#   data/map/StudySitePoints.dta    (30 village coordinates)
#   data/distance_storm_km.dta      (session distance to Haiyan track, km)
#   data/windspeed_predicted.dta    (session calibrated wind, kn)
# Outputs:
#   results/figures/figure4_study_site.png  (183 mm x 105 mm, 300 dpi)
#   data/tce_dat/processed/study_site_figure_objects.rds
# Depends: data.table, sf, ggplot2, rnaturalearth, patchwork, haven
#   (all present in the project library; nothing installed by this script)

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2)
  library(rnaturalearth); library(patchwork); library(haven)
})

# --- paths: working directory is the replication_package root ---------------
root <- "data/tce_dat"; ff <- "sans"

ibtracs_csv <- file.path(root, "raw/ibtracs.WP.list.v04r01.csv")
ibtracs_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.WP.list.v04r01.csv"
if (!file.exists(ibtracs_csv)) {
  message("Downloading IBTrACS WP (ca. 114 MB) from NOAA NCEI ...")
  download.file(ibtracs_url, ibtracs_csv, mode = "wb")
}

# ---------------------------------------------------------------------------
# 1. IBTrACS: super-typhoon tracks 1950-2015 near the Philippines
# ---------------------------------------------------------------------------
ib <- fread(ibtracs_csv,
            select = c("SID","SEASON","NAME","ISO_TIME","LAT","LON","USA_WIND","TRACK_TYPE"))
ib <- ib[-1]                                   # drop units row
ib[, `:=`(LAT  = as.numeric(LAT), LON = as.numeric(LON),
          WIND = suppressWarnings(as.numeric(USA_WIND)),
          SEASON = as.integer(SEASON))]
ib <- ib[TRACK_TYPE == "main" & !is.na(LAT) & !is.na(LON)]

phl_box <- c(xmin = 115, xmax = 135, ymin = 4, ymax = 22)   # "near the Philippines"
# super-typhoon intensity (>=108 kn ~ >200 km/h) WHILE near the Philippines;
# requiring intensity anywhere along track floods the map with recurving storms
ib[, in_box := LON >= phl_box["xmin"] & LON <= phl_box["xmax"] &
               LAT >= phl_box["ymin"] & LAT <= phl_box["ymax"]]
sel <- ib[SEASON >= 1950 & SEASON <= 2015,
          .(maxw_box = suppressWarnings(max(WIND[in_box], na.rm = TRUE))),
          by = .(SID, SEASON)][is.finite(maxw_box) & maxw_box >= 108]

trk_pts <- ib[SID %in% sel$SID][order(SID, ISO_TIME)]
tracks <- st_sf(
  SID = sel$SID[match(unique(trk_pts$SID), sel$SID)],
  geometry = st_sfc(lapply(split(trk_pts[, .(LON, LAT)], trk_pts$SID),
                           function(d) st_linestring(as.matrix(d))), crs = 4326))

haiyan_sid <- "2013306N07162"
stopifnot(haiyan_sid %in% sel$SID)
hy_pts  <- trk_pts[SID == haiyan_sid]
trk_hy  <- tracks[tracks$SID == haiyan_sid, ]
trk_oth <- tracks[tracks$SID != haiyan_sid, ]

# Keep only storms whose track crosses Philippine land (landfalling typhoons).
# Without this filter the panel is unreadable spaghetti and obscures how rarely
# Panay itself is crossed (user review 2026-06-11).
phl_land <- rnaturalearth::ne_countries(scale = 50, country = "Philippines",
                                        returnclass = "sf")
suppressMessages(sf::sf_use_s2(FALSE))
trk_oth <- trk_oth[lengths(st_intersects(trk_oth, phl_land)) > 0, ]
cat("Landfalling intense typhoons plotted:", nrow(trk_oth), "\n")

# ---------------------------------------------------------------------------
# 2. Replication-package map data
# ---------------------------------------------------------------------------
# municipalities: spmap coordinate format (_ID polygons, parts separated by NA)
mc <- as.data.table(read_dta(file.path("data/map/muncoord.dta")))
setnames(mc, c("id", "x", "y"))
build_multipoly <- function(d) {
  d[, part := cumsum(is.na(x))]
  rings <- lapply(split(d[!is.na(x)], by = "part", keep.by = FALSE), function(r) {
    m <- as.matrix(r[, .(x, y)])
    if (nrow(m) < 4) return(NULL)
    if (any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
    list(m)
  })
  rings <- Filter(Negate(is.null), rings)
  st_multipolygon(rings)
}
mun <- st_sf(munID = sort(unique(mc$id)),
             geometry = st_sfc(lapply(split(mc, by = "id", keep.by = FALSE),
                                      build_multipoly), crs = 4326))
mun <- st_make_valid(mun)

# municipality damage shares (v3 study-site figure variable; spmap joined on id)
dmg <- as.data.table(read_dta(file.path("data/map/damages_municipalities.dta")))
# shares can exceed 1 (combined damages relative to 2010 household counts);
# cap at 100% for display, as in the published spmap breaks (0-1).
# JOIN KEY: muncoord _ID corresponds to damages_municipalities.munID
# (NOT the running id 1-100; verified 2026-06-11, intersection check)
mun <- merge(mun, dmg[, .(munID, share_dmg = pmin(share_houses_dmg_combined, 1))],
             by = "munID", all.x = TRUE)

# how many intense landfalling typhoons crossed Panay itself before Haiyan?
panay_poly <- st_union(st_geometry(mun))
crossed <- trk_oth[lengths(st_intersects(trk_oth, panay_poly)) > 0, ]
crossed_seasons <- sel$SEASON[match(crossed$SID, sel$SID)]
cat("Intense landfalling typhoons crossing Panay 1950-2012 (pre-Haiyan):",
    sum(crossed_seasons < 2013), "| 1950-2015 total:", nrow(crossed), "\n")
cat("  Panay-crossing seasons:", paste(sort(crossed_seasons), collapse = ", "), "\n")

# villages
ssp <- as.data.table(read_dta(file.path("data/map/StudySitePoints.dta")))
vil <- st_as_sf(ssp, coords = c("_X", "_Y"), crs = 4326)

# paper's calibrated wind / distance (session level, no coordinates)
dst <- as.data.table(read_dta(file.path("data/distance_storm_km.dta")))
wsp <- as.data.table(read_dta(file.path("data/windspeed_predicted.dta")))
cal <- merge(dst, wsp, by = c("session", "year"))[order(distance_storm_km)]

# ---------------------------------------------------------------------------
# 3. Wind field: V(distance to track), evaluated on the study-area land grid
#    (planar work in UTM 51N — exact enough at this scale)
# ---------------------------------------------------------------------------
utm <- 32651
hy_local <- hy_pts[LON > 119 & LON < 127]                       # segment near Panay
trk_hy_loc <- st_transform(
  st_sfc(st_linestring(as.matrix(hy_local[, .(LON, LAT)])), crs = 4326), utm)

vil_utm <- st_transform(vil, utm)
vil$dist_km <- as.numeric(st_distance(vil_utm, trk_hy_loc)) / 1000

# validation: computed village distances vs the paper's distance_storm_km
dist_dev <- max(abs(sort(vil$dist_km) - cal$distance_storm_km))

# rank join: i-th closest village gets i-th closest session's calibrated wind
vil$windspeed <- cal$windspeed_predicted[rank(vil$dist_km, ties.method = "first")]

# monotone decreasing spline V(r) from the 30 calibrated pairs; linear tail
Vfun <- splinefun(cal$distance_storm_km, cal$windspeed_predicted, method = "hyman")
r_max <- max(cal$distance_storm_km)
tail_slope <- (Vfun(r_max) - Vfun(r_max - 5)) / 5
V <- function(r) {
  out <- Vfun(pmin(pmax(r, min(cal$distance_storm_km)), r_max))
  far <- r > r_max
  out[far] <- Vfun(r_max) + tail_slope * (r[far] - r_max)
  pmax(out, 20)
}

bb <- c(xmin = 121.55, xmax = 123.45, ymin = 10.30, ymax = 12.05)  # panel b extent
grd <- as.data.table(expand.grid(LON = seq(bb["xmin"], bb["xmax"], by = 0.012),
                                 LAT = seq(bb["ymin"], bb["ymax"], by = 0.012)))
grd_sf  <- st_as_sf(grd, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)
mun_u   <- st_union(st_transform(mun, utm))
grd_utm <- st_transform(grd_sf, utm)
on_land <- lengths(st_intersects(grd_utm, mun_u)) > 0
grd     <- grd[on_land]
grd[, dist_km := as.numeric(st_distance(grd_utm[on_land, ], trk_hy_loc)) / 1000]
grd[, wind := V(dist_km)]

ss_breaks <- c(-Inf, 83, 96, 113, 137, Inf)
ss_labels <- c("≤ Cat 1 (<83)", "Cat 2 (83–96)", "Cat 3 (96–113)",
               "Cat 4 (113–137)", "Cat 5 (≥137)")
grd[, ss_cat := cut(wind, ss_breaks, ss_labels)]
vil$ss_cat <- cut(vil$windspeed, ss_breaks, ss_labels)

# ---------------------------------------------------------------------------
# 4. Panel a: Philippines overview with super-typhoon tracks
# ---------------------------------------------------------------------------
ne_scale <- if (requireNamespace("rnaturalearthhires", quietly = TRUE)) 10 else 50
world <- ne_countries(scale = ne_scale, returnclass = "sf")

study_box <- st_as_sfc(st_bbox(c(xmin = 121.6, xmax = 123.4,
                                 ymin = 10.30, ymax = 12.00), crs = st_crs(4326)))

# label anchor: Haiyan track point nearest 128E, nudged south
hy_lab <- hy_pts[which.min(abs(LON - 128.5))]

sea  <- "#DEEAF0"; land <- "#EDEDED"; land_line <- "#B0B0B0"
col_oth <- "#9AA0A6"; col_cross <- "#2B2B2B"; col_hy <- "#DC267F"  # faded / Panay-crossing / Haiyan

# Panel a: smooth track-density of intense typhoons near the Philippines,
# 1950-2015. A 2-D kernel density of the best-track points (Haiyan excluded and
# overlaid separately) gives a smooth passage-intensity surface; the wide
# bandwidth makes the 3-hourly point spacing immaterial at this scale. Replaces
# the spaghetti tracks with an aggregate heatmap.
dp <- as.matrix(trk_pts[SID != haiyan_sid, .(LON, LAT)])
kd <- MASS::kde2d(dp[, 1], dp[, 2], n = 220, h = c(2, 2),
                  lims = c(phl_box["xmin"], phl_box["xmax"],
                           phl_box["ymin"], phl_box["ymax"]))
kdf <- cbind(expand.grid(LON = kd$x, LAT = kd$y), z = as.vector(kd$z))
cat("Panel a density: best-track points (excl. Haiyan) =", nrow(dp), "\n")

# Nature-style conventions: the study box labeled with the zoom panel's letter
# ("b"), italic gray geography labels.
p_a <- ggplot() +
  geom_sf(data = world, fill = land, color = NA) +
  geom_raster(data = kdf, aes(LON, LAT, fill = z)) +
  geom_sf(data = world, fill = NA, color = land_line, linewidth = 0.15) +
  geom_sf(data = trk_hy, color = col_hy, linewidth = 0.9) +
  geom_sf(data = study_box, fill = NA, color = "#1A1A1A", linewidth = 0.6) +
  annotate("text", x = hy_lab$LON, y = hy_lab$LAT - 1.1, label = "Haiyan (2013)",
           color = col_hy, size = 2.6, fontface = "bold", family = ff, hjust = 0.4) +
  annotate("text", x = 123.7, y = 12.35, label = "b", family = ff,
           color = "#1A1A1A", size = 3.2, fontface = "bold", hjust = 0) +
  annotate("text", x = 121.2, y = 17.2, label = "PHILIPPINES", family = ff,
           color = "grey45", size = 2.6, fontface = "italic") +
  annotate("text", x = 119.3, y = 9.0, label = "Sulu Sea", family = ff,
           color = "grey55", size = 2.2, fontface = "italic") +
  annotate("text", x = 130.5, y = 14.5, label = "Philippine Sea", family = ff,
           color = "grey55", size = 2.2, fontface = "italic") +
  scale_fill_gradientn("Intense typhoon track density, 1950–2015",
    colours = c("#FFFFFF00", "#EFEDF5", "#BCBDDC", "#807DBA", "#54278F"),
    values = scales::rescale(c(0, 0.06, 0.3, 0.6, 1)),
    guide = guide_colorbar(title.position = "top", barwidth = unit(3, "cm"),
                           barheight = unit(0.22, "cm"), label = FALSE)) +
  scale_x_continuous(breaks = seq(115, 135, 5)) +
  scale_y_continuous(breaks = seq(5, 20, 5)) +
  coord_sf(xlim = c(phl_box["xmin"], phl_box["xmax"]),
           ylim = c(phl_box["ymin"], phl_box["ymax"]), expand = FALSE) +
  labs(title = "Regional typhoon exposure") +
  theme_minimal(base_size = 8, base_family = ff) +
  theme(panel.background = element_rect(fill = sea, color = NA),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
        panel.grid.major = element_line(color = "white", linewidth = 0.15),
        axis.text = element_text(size = 5.5, color = "grey40"),
        axis.title = element_blank(),
        plot.title = element_text(size = 7.5, face = "bold", hjust = 0,
                                  margin = margin(t = 0, b = 1.5)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(2, 4, 0, 4),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(t = 2, b = 0),
        legend.box.spacing = unit(0.08, "cm"))

# ---------------------------------------------------------------------------
# 5. Panel b: Panay zoom with wind bands, municipalities, villages, track
# ---------------------------------------------------------------------------
# Panel b: municipality damage choropleth (as in the v3 figure) with the
# Saffir-Simpson category boundaries of the calibrated wind field drawn as
# labeled isolines on top — keeps BOTH the damage shares and the wind framing.
cat_breaks <- c(83, 96, 113, 137)
cat_names  <- c("Cat 2", "Cat 3", "Cat 4", "Cat 5")

# Wind-field band labels: place each Saffir-Simpson label on every MAJOR segment of
# its boundary contour, not just once on the left edge. The calibrated field is
# non-monotone (an inner Cat 5 lobe near the track plus a second lobe to the north),
# so the top-left study communities sit in a Cat 4 zone the single-label version left
# unlabelled. Positions are computed from the land wind grid via contourLines().
ulon <- sort(unique(grd$LON)); ulat <- sort(unique(grd$LAT))
Wm <- matrix(NA_real_, length(ulon), length(ulat))
Wm[cbind(match(grd$LON, ulon), match(grd$LAT, ulat))] <- grd$wind
.lab <- list()
for (i in seq_along(cat_breaks)) {
  for (s in contourLines(ulon, ulat, Wm, levels = cat_breaks[i])) {
    if (length(s$x) >= 15) {                       # skip tiny stray segments
      k <- which.min(s$x)                          # western terminus of the segment
      .lab[[length(.lab) + 1]] <- data.frame(x = s$x[k], y = s$y[k],
                                             lab = cat_names[i])
    }
  }
}
cat_lab <- do.call(rbind, .lab)

# explicit "Cat 4" in the northern (top-left) zone, anchored on the cluster of Cat 4
# study communities above the inner Cat 5 lobe (data-driven, not hardcoded coords)
.vc <- cbind(as.data.frame(st_coordinates(vil)), cat = as.character(vil$ss_cat))
names(.vc)[1:2] <- c("LON", "LAT")
.n4 <- .vc[grepl("Cat 4", .vc$cat) & .vc$LAT > 11.5, ]
if (nrow(.n4) > 0)
  cat_lab <- rbind(cat_lab, data.frame(x = mean(.n4$LON),
                                       y = max(.n4$LAT) + 0.07, lab = "Cat 4"))

p_b <- ggplot() +
  geom_sf(data = world, fill = land, color = land_line, linewidth = 0.15) +
  geom_sf(data = mun, aes(fill = share_dmg), color = "grey55", linewidth = 0.14) +
  geom_contour(data = grd, aes(LON, LAT, z = wind), breaks = cat_breaks,
               color = "#1A1A1A", linewidth = 0.35, linetype = "22") +
  geom_label(data = cat_lab, aes(x, y, label = lab), family = ff, size = 1.95,
             color = "#1A1A1A", fontface = "bold", label.size = 0,
             label.padding = unit(0.4, "mm"), fill = alpha("white", 0.6)) +
  geom_path(data = hy_local, aes(LON, LAT), color = "#1A1A1A", linewidth = 0.9) +
  annotate("text", x = 122.05, y = 11.60, label = "Haiyan track", family = ff,
           color = "#FFFFFF", size = 2.4, fontface = "bold", hjust = 0, angle = -8) +
  geom_sf(data = vil, aes(shape = "Study communities (n = 30)"),
          fill = "#648FFF", color = "white", size = 1.7, stroke = 0.35) +
  scale_fill_distiller("Share of houses damaged", palette = "Reds", direction = 1,
                       limits = c(0, 1), labels = scales::percent,
                       na.value = "grey92",
                       guide = guide_colorbar(order = 1, title.position = "top",
                                              barwidth = unit(2.6, "cm"),
                                              barheight = unit(0.22, "cm"))) +
  scale_shape_manual(NULL, values = c("Study communities (n = 30)" = 21),
                     guide = guide_legend(order = 2, title.position = "top",
                                          override.aes = list(size = 2.2))) +
  annotate("text", x = 122.65, y = 11.05, label = "Panay Island", family = ff,
           color = "grey35", size = 2.4, fontface = "italic") +
  annotate("text", x = 122.40, y = 11.97, label = "Sibuyan Sea", family = ff,
           color = "grey55", size = 2.1, fontface = "italic") +
  annotate("text", x = 122.45, y = 10.38, label = "Panay Gulf", family = ff,
           color = "grey55", size = 2.1, fontface = "italic") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.22,
                              height = unit(0.12, "cm"), text_cex = 0.5,
                              bar_cols = c("grey20", "white")) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
      height = unit(0.55, "cm"), width = unit(0.45, "cm"),
      style = ggspatial::north_arrow_minimal(text_size = 5)) +
  scale_x_continuous(breaks = seq(121.5, 123.5, 0.5)) +
  scale_y_continuous(breaks = seq(10.5, 12, 0.5)) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE) +
  labs(title = "Study site: Panay Island") +
  theme_minimal(base_size = 8, base_family = ff) +
  theme(panel.background = element_rect(fill = sea, color = NA),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
        panel.grid.major = element_line(color = "white", linewidth = 0.15),
        axis.text = element_text(size = 5.5, color = "grey40"),
        axis.title = element_blank(),
        plot.title = element_text(size = 7.5, face = "bold", hjust = 0,
                                  margin = margin(t = 0, b = 1.5)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(2, 4, 0, 4),
        legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(size = 6.5, face = "bold"),
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(t = 2, b = 0),
        legend.box.spacing = unit(0.08, "cm"))

# ---------------------------------------------------------------------------
# 6. Assemble and save (NCC double column: 183 mm wide)
# ---------------------------------------------------------------------------
fig <- p_a + p_b + plot_layout(widths = c(1.05, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 10, family = ff),
        plot.tag.position = c(0, 1))

w_in <- 183 / 25.4
h_in <- 92 / 25.4
out_png <- file.path("results/figures/figure4_study_site.png")
ggsave(out_png, fig, width = w_in, height = h_in, dpi = 300, bg = "white")

saveRDS(list(tracks_sel = sel, villages = vil, wind_grid = grd,
             dist_dev_km = dist_dev),
        file.path(root, "processed/study_site_figure_objects.rds"))

# ---------------------------------------------------------------------------
# 7. Diagnostics
# ---------------------------------------------------------------------------
panay_bb <- c(xmin = 121.5, xmax = 123.2, ymin = 10.4, ymax = 11.9)
vxy <- st_coordinates(vil)
vil_ok <- all(vxy[, 1] >= panay_bb["xmin"] & vxy[, 1] <= panay_bb["xmax"] &
              vxy[, 2] >= panay_bb["ymin"] & vxy[, 2] <= panay_bb["ymax"])
cat("Saved:", out_png, "\n\n")
cat("=== DIAGNOSTICS ===\n")
cat("Panel a tracks plotted:", nrow(tracks), "(super typhoons >=108 kn near PHL)\n")
cat("  seasons:", min(sel$SEASON), "-", max(sel$SEASON),
    "| Haiyan max wind (kn):", max(hy_pts$WIND, na.rm = TRUE), "\n")
cat("Villages plotted:", nrow(vil), "| inside Panay bbox (121.5-123.2E, 10.4-11.9N):",
    vil_ok, "\n")
vil_land_m <- as.numeric(st_distance(vil_utm, mun_u))
cat("Villages on municipality land: max offshore distance =",
    round(max(vil_land_m), 1), "m\n")
cat("Computed vs paper village-track distances: max |dev| =",
    round(dist_dev, 2), "km\n")
cat("Village calibrated wind (kn): range",
    paste(round(range(vil$windspeed), 1), collapse = " - "), "\n")
print(table(vil$ss_cat))
cat("Wind-field land cells:", nrow(grd), "| band counts:\n")
print(table(grd$ss_cat))
cat("Wind field range (kn):", paste(round(range(grd$wind), 1), collapse = " - "), "\n")
bbg <- c(range(grd$LON), range(grd$LAT))
cat("Grid bbox:", round(bbg, 2), "| Panel b limits:", bb, "\n")
cat("Haiyan local track lon range:", paste(round(range(hy_local$LON), 2), collapse = " - "),
    "lat:", paste(round(range(hy_local$LAT), 2), collapse = " - "), "\n")
