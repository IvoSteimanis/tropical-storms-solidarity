# 01_extract_global.R
# Loop over all TCE-DAT single-event CSV files (global, no bbox filter),
# consolidate into one data.table. Use fix2015 version (consistent population baseline).
#
# Output: data/tce_dat/processed/global_events_long.rds
#   columns: storm_id, ISO, LAT, LON, exposed_assets, exposed_pop, windspeed_kn
#   (one row per affected cell per event worldwide)

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
})

root <- "data/tce_dat"
events_dir <- file.path(root, "raw/extracted/events_2015/TCE-DAT_single_events_2015/TC_data")
out_path <- file.path(root, "processed/global_events_long.rds")

files <- list.files(events_dir, pattern = "_fix2015\\.csv$", full.names = TRUE)
cat("Found", length(files), "event files. Extracting globally (no bbox filter)...\n")

read_event <- function(f) {
  storm_id <- sub("_fix2015\\.csv$", "", basename(f))
  dt <- fread(f, showProgress = FALSE)
  if (nrow(dt) == 0) return(NULL)
  dt[, storm_id := storm_id]
  dt[, .(storm_id, ISO, LAT, LON, exposed_assets, exposed_pop, windspeed_kn = windspeed)]
}

t0 <- Sys.time()
event_list <- lapply(files, read_event)
event_list <- event_list[!sapply(event_list, is.null)]
events_long <- rbindlist(event_list, use.names = TRUE)
t1 <- Sys.time()

cat("Read", length(event_list), "events globally.\n")
cat("Total rows:", format(nrow(events_long), big.mark = ","), "\n")
cat("Unique ISOs:", length(unique(events_long$ISO)), "\n")
cat("ISO codes:", paste(sort(unique(events_long$ISO)), collapse = ", "), "\n")
cat("Time:", format(t1 - t0), "\n")

# Sanity check: find Haiyan
haiyan <- events_long[storm_id == "2013306N07162"]
cat("Haiyan (2013306N07162):\n",
    "  rows:", nrow(haiyan), "\n",
    "  max wind (knots):", round(max(haiyan$windspeed_kn, na.rm = TRUE), 1), "\n",
    "  ISO countries:", paste(sort(unique(haiyan$ISO)), collapse = ", "), "\n")

saveRDS(events_long, out_path)
cat("Saved:", out_path, "(",
    format(file.size(out_path) / 1e6, digits = 3), "MB)\n")
