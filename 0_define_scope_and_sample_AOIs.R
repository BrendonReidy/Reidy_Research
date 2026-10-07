# Step 0: Spatial domain + reproducible random sample of Areas of Interest
# -----------------------------------------------------------------------------
# Order of operations:
#   1. Pull EPA Level III ecoregions (ECOREGION_CODES) from GEE.
#   2. Tile them with a fixed-origin hexagon grid (EPSG:5070); keep hexes that
#      sit >= MIN_HEX_IN_ECO inside ONE ecoregion. Each hex is a candidate AOI.
#   3. Screen every candidate hex server-side in ONE batch export:
#        land_ha        Hansen datamask == 1
#        forest_ha      NLCD 2001 deciduous/mixed (pre-disturbance forest type)
#        qual_loss_ha   Hansen loss (gain == 0, same rule as Step 1) inside that
#                       forest, loss year LOSS_YEAR_MIN..LOSS_YEAR_MAX
#        large_loss_ha  the part of qual_loss sitting in clumps >= MIN_PATCH_HA
#   4. Eligibility rules -> sampling frame.
#   5. Seeded, stratified sample (equal allocation per ecoregion) via GRTS
#      (spatially balanced) or simple random sampling, plus ordered
#      replacement AOIs.
#   6. Save frame + sample, upload the sample as a GEE asset for Steps 1-4c,
#      write a manifest.
#   7. Map centroids + check the sample against the frame it came from.
# -----------------------------------------------------------------------------
# AOI = one hex (~541 km^2). Steps 1-4c then process ALL qualifying patches
# inside each sampled hex (two-stage cluster design; aoi_id becomes a random
# effect / cluster in Step 5).
#
# Requires: rgee, sf, dplyr, tidyr, ggplot2; spsurvey if SAMPLING_METHOD = "grts";
#           maps (optional, state outlines on the map)
# NOTE: written without GEE access -- on the first run, read the printed
#       ecoregion table and screening summary before trusting the sample.
# =============================================================================

source(file.path("~/Google Drive/My Drive/Reidy_research/fall color code", "config_scaleup.R"))

library(rgee)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

ee_Initialize(drive = TRUE)

# -----------------------------------------------------------------------------
# 1. Ecoregions
# -----------------------------------------------------------------------------
ecoFC <- ee$FeatureCollection(ECOREGION_ID)$
  filter(ee$Filter$inList("us_l3code", as.list(ECOREGION_CODES)))$
  map(ee_utils_pyfunc(function(f) f$simplify(maxError = 100)))   # 100 m is plenty for 25 km hexes

if (ecoFC$size()$getInfo() == 0) {
  stop("No ecoregion polygons matched ECOREGION_CODES. us_l3code is a STRING in ",
       ECOREGION_ID, " -- codes must be quoted, e.g. c('66', '67').")
}

eco_raw <- ee_as_sf(ecoFC, via = "drive", maxFeatures = 10000)
stopifnot(all(c("us_l3code", "us_l3name") %in% names(eco_raw)))

eco <- eco_raw %>%
  st_transform(AOI_CRS) %>%
  st_make_valid() %>%
  group_by(us_l3code, us_l3name) %>%
  summarise(.groups = "drop") %>%                  # dissolve the multipart pieces
  mutate(eco_area_km2 = as.numeric(st_area(.)) / 1e6)

cat("\nEcoregions in scope:\n")
print(st_drop_geometry(eco))

# -----------------------------------------------------------------------------
# 2. Hex grid (candidate AOIs)
# -----------------------------------------------------------------------------
hex_geom <- st_make_grid(eco, cellsize = HEX_CELLSIZE_M, square = FALSE,
                         offset = HEX_GRID_ORIGIN, what = "polygons")

hex <- st_sf(geometry = hex_geom)
hex <- hex[lengths(st_intersects(hex, eco)) > 0, ]

# Deterministic id from the hex centre (km, EPSG:5070): the same hex gets the
# same id every run, so samples, assets and joins are revisit-able.
xy <- st_coordinates(st_centroid(st_geometry(hex)))
hex$hex_id       <- sprintf("hx_%d_%d", as.integer(round(xy[, 1] / 1000)),
                            as.integer(round(xy[, 2] / 1000)))
hex$hex_area_km2 <- as.numeric(st_area(hex)) / 1e6
stopifnot(!anyDuplicated(hex$hex_id))

# Assign each hex to the ecoregion holding most of its area; keep "pure" hexes.
dominant_eco <- st_intersection(select(hex, hex_id), select(eco, us_l3code, us_l3name)) %>%
  mutate(ov_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  slice_max(ov_km2, n = 1, with_ties = FALSE) %>%
  ungroup()

hex <- hex %>%
  left_join(dominant_eco, by = "hex_id") %>%
  mutate(eco_share = ov_km2 / hex_area_km2) %>%
  filter(eco_share >= MIN_HEX_IN_ECO) %>%
  arrange(hex_id)                                  # deterministic row order for sampling

ll <- st_coordinates(st_transform(st_centroid(st_geometry(hex)), 4326))
hex$centroid_lon <- ll[, 1]
hex$centroid_lat <- ll[, 2]

cat(sprintf("\nCandidate hexes (>= %.0f%% in one ecoregion): %d\n",
            100 * MIN_HEX_IN_ECO, nrow(hex)))
print(table(hex$us_l3name))

# Pilot study area, drawn on the maps for orientation only.
pilot_roi <- st_sfc(st_point(c(-79.862539, 37.829550)), crs = 4326) %>%
  st_transform(AOI_CRS) %>%
  st_buffer(100000)

# Quick look BEFORE any heavy GEE work: does the grid sit where you expect?
p_candidates <- ggplot() +
  geom_sf(data = eco, aes(fill = us_l3name), color = "grey40", linewidth = 0.2, alpha = 0.35) +
  geom_sf(data = hex, fill = NA, color = "grey20", linewidth = 0.15) +
  geom_sf(data = pilot_roi, fill = NA, color = "black", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Candidate AOI hexes (before screening)",
       subtitle = sprintf("%d hexes, %.0f km across, >= %.0f%% in one ecoregion. Dashed = pilot ROI.",
                          nrow(hex), HEX_CELLSIZE_M / 1000, 100 * MIN_HEX_IN_ECO),
       fill = "EPA Level III ecoregion") +
  theme_minimal()
ggsave(file.path(outputDir, "map_aoi_candidates.png"), p_candidates, width = 10, height = 8, dpi = 200)

# -----------------------------------------------------------------------------
# 3. Server-side screening metrics (one batch export for all candidate hexes)
# -----------------------------------------------------------------------------
TEST_RUN <- !is.na(SCREEN_TEST_N)
hex_screen <- hex
if (TEST_RUN) {
  set.seed(SEED)
  hex_screen <- hex[sort(sample(nrow(hex), min(SCREEN_TEST_N, nrow(hex)))), ]
  cat(sprintf("\nTEST RUN: screening %d of %d candidate hexes.\n", nrow(hex_screen), nrow(hex)))
}

hex_ee <- sf_as_ee(st_transform(select(hex_screen, hex_id), 4326), via = "getInfo")

hansen   <- ee$Image(HANSEN_ID)
lossyear <- hansen$select("lossyear")
pixelHa  <- ee$Image$pixelArea()$divide(1e4)

land <- hansen$select("datamask")$eq(1)

# Select by system:index, the way Step 2b does, and fail loudly if it's absent.
nlcdCol   <- ee$ImageCollection(NLCD_COLL_ID)
available <- unlist(nlcdCol$aggregate_array("system:index")$getInfo())
if (!as.character(NLCD_PRE_YEAR) %in% available) {
  stop("NLCD epoch ", NLCD_PRE_YEAR, " not in ", NLCD_COLL_ID,
       ". Available: ", paste(available, collapse = ", "))
}
nlcdPre <- nlcdCol$
  filter(ee$Filter$eq("system:index", as.character(NLCD_PRE_YEAR)))$
  first()$
  select("landcover")

forestPre <- nlcdPre$remap(
  from         = as.list(FOREST_CLASSES),
  to           = as.list(rep(1L, length(FOREST_CLASSES))),
  defaultValue = 0
)$unmask(0)

# Same persistence rule as Step 1 (NB: Hansen 'gain' only covers 2000-2012).
qualLoss <- hansen$select("loss")$eq(1)$
  And(hansen$select("gain")$eq(0))$
  And(lossyear$gte(LOSS_YEAR_MIN - 2000L))$
  And(lossyear$lte(LOSS_YEAR_MAX - 2000L))$
  And(forestPre$eq(1))

# Clump size: connectedPixelCount groups pixels sharing the same value, so
# clumps are split by loss year the way Step 1's labelProperty = 'lossyear'
# splits patches. Counts saturate at maxSize (1024 px ~ 60+ ha), which is far
# above the 25 ha threshold, so saturation doesn't matter here.
clumpPx   <- lossyear$updateMask(qualLoss)$connectedPixelCount(maxSize = 1024L, eightConnected = FALSE)
clumpHa   <- clumpPx$multiply(ee$Image$pixelArea())$divide(1e4)
largeLoss <- clumpHa$gte(MIN_PATCH_HA)$unmask(0)$And(qualLoss)

# Multi-band image + single-output reducer -> output properties are named
# after the BANDS (land_ha, ...), so no setOutputs() gymnastics needed.
statsImg <- ee$Image$cat(
  land$multiply(pixelHa)$rename("land_ha"),
  forestPre$And(land)$multiply(pixelHa)$rename("forest_ha"),
  qualLoss$multiply(pixelHa)$rename("qual_loss_ha"),
  largeLoss$multiply(pixelHa)$rename("large_loss_ha")
)

# Native Hansen grid: crs given, scale deliberately omitted.
hexStats <- statsImg$reduceRegions(
  collection = hex_ee,
  reducer    = ee$Reducer$sum(),
  crs        = hansen$select("loss")$projection(),
  tileScale  = 8          # raise to 16 if the task hits a memory limit
)

screening_csv <- export_table_csv(
  hexStats,
  name      = if (TEST_RUN) "aoi_hex_screening_TEST" else "aoi_hex_screening",
  selectors = c("hex_id", "land_ha", "forest_ha", "qual_loss_ha", "large_loss_ha")
)

# -----------------------------------------------------------------------------
# 4. Eligibility -> sampling frame
# -----------------------------------------------------------------------------
screening <- read.csv(screening_csv, stringsAsFactors = FALSE)
stopifnot(nrow(screening) == nrow(hex_screen))

frame <- hex_screen %>%
  inner_join(screening, by = "hex_id") %>%
  mutate(
    forest_frac = ifelse(land_ha > 0, forest_ha / land_ha, NA_real_),
    eligible    = !is.na(forest_frac) &
                  forest_frac   >= MIN_FOREST_FRAC &
                  large_loss_ha >= MIN_LARGE_LOSS_HA
  ) %>%
  arrange(hex_id)

frame_summary <- frame %>%
  st_drop_geometry() %>%
  group_by(us_l3code, us_l3name) %>%
  summarise(
    n_candidates        = n(),
    n_eligible          = sum(eligible),
    median_forest_frac  = median(forest_frac, na.rm = TRUE),
    total_large_loss_ha = sum(large_loss_ha[eligible]),
    .groups = "drop"
  )
cat("\nScreening summary by ecoregion:\n")
print(frame_summary)

if (TEST_RUN) {
  cat("\nPer-hex screening results (test subset):\n")
  print(frame %>% st_drop_geometry() %>%
          select(hex_id, us_l3name, land_ha, forest_frac, qual_loss_ha, large_loss_ha, eligible))
  stop("TEST RUN finished (this is not a failure). If the numbers above look sensible, ",
       "set SCREEN_TEST_N <- NA in config_scaleup.R and re-run for the full screening + sample.",
       call. = FALSE)
}

short <- frame_summary %>% filter(n_eligible < N_PER_ECOREGION + N_OVER_PER_ECOREGION)
if (nrow(short) > 0) {
  stop("Too few eligible hexes in: ", paste(short$us_l3name, collapse = ", "),
       ". Relax MIN_FOREST_FRAC / MIN_LARGE_LOSS_HA / MIN_HEX_IN_ECO, or lower ",
       "N_PER_ECOREGION, in config_scaleup.R.")
}

# -----------------------------------------------------------------------------
# 5. Seeded stratified sample (equal allocation per ecoregion)
# -----------------------------------------------------------------------------
eligible_pts <- frame %>%
  filter(eligible) %>%
  arrange(hex_id) %>%
  st_centroid() %>%                                # points, EPSG:5070
  suppressWarnings()                               # "attributes assumed constant" -- expected

strata <- sort(unique(eligible_pts$us_l3code))

set.seed(SEED)
if (SAMPLING_METHOD == "grts") {
  if (!requireNamespace("spsurvey", quietly = TRUE)) {
    stop("install.packages('spsurvey'), or set SAMPLING_METHOD <- 'srs' in config_scaleup.R")
  }
  design <- spsurvey::grts(
    sframe      = eligible_pts,
    n_base      = setNames(rep(N_PER_ECOREGION, length(strata)), strata),
    stratum_var = "us_l3code",
    n_over      = as.list(setNames(rep(N_OVER_PER_ECOREGION, length(strata)), strata)),
    DesignID    = "AOI"
  )
  # Replacement ("Over") sites must be used in siteID order within a stratum.
  sample_tbl <- rbind(design$sites_base, design$sites_over) %>%
    st_drop_geometry() %>%
    select(hex_id, siteID, siteuse, design_wgt = wgt)
} else {
  sample_tbl <- eligible_pts %>%
    st_drop_geometry() %>%
    group_by(us_l3code) %>%
    slice_sample(n = N_PER_ECOREGION + N_OVER_PER_ECOREGION) %>%
    mutate(draw = row_number(),
           siteuse = if_else(draw <= N_PER_ECOREGION, "Base", "Over")) %>%
    ungroup() %>%
    arrange(us_l3code, draw) %>%
    mutate(siteID = sprintf("AOI-%03d", row_number()), design_wgt = NA_real_) %>%
    select(hex_id, siteID, siteuse, design_wgt)
}

aoi <- frame %>%
  inner_join(sample_tbl, by = "hex_id") %>%
  mutate(aoi_id = hex_id, run_id = RUN_ID, sample_seed = SEED,
         sample_method = SAMPLING_METHOD) %>%
  arrange(siteID)

cat("\nSampled AOIs:\n")
print(table(aoi$us_l3name, aoi$siteuse))

# -----------------------------------------------------------------------------
# 6. Save + upload + manifest
# -----------------------------------------------------------------------------
st_write(frame, file.path(outputDir, "aoi_sampling_frame.gpkg"), delete_dsn = TRUE, quiet = TRUE)
st_write(aoi,   file.path(outputDir, "aoi_sample.gpkg"),         delete_dsn = TRUE, quiet = TRUE)
write.csv(st_drop_geometry(aoi), file.path(outputDir, "aoi_sample_centroids.csv"), row.names = FALSE)

# Base AND replacement AOIs go to the asset; Steps 1-4c filter siteuse == 'Base'
# (and swap in replacements in siteID order if a base AOI is dropped).
aoi_asset <- asset_id("aoi_sample")
invisible(sf_as_ee(
  st_transform(select(aoi, aoi_id, siteID, siteuse, us_l3code), 4326),
  via = "getInfo_to_asset", assetId = aoi_asset, overwrite = TRUE
))

write_manifest("0_aoi_sample", c(
  paste("sampling_method:", SAMPLING_METHOD),
  paste("ecoregion_codes:", paste(ECOREGION_CODES, collapse = ",")),
  paste("hex_cellsize_m:", HEX_CELLSIZE_M, "| min_hex_in_eco:", MIN_HEX_IN_ECO),
  paste("loss_years:", LOSS_YEAR_MIN, "-", LOSS_YEAR_MAX),
  paste("min_forest_frac:", MIN_FOREST_FRAC, "| min_patch_ha:", MIN_PATCH_HA,
        "| min_large_loss_ha:", MIN_LARGE_LOSS_HA),
  paste("n_candidates:", nrow(frame), "| n_eligible:", sum(frame$eligible),
        "| n_base:", sum(aoi$siteuse == "Base"), "| n_over:", sum(aoi$siteuse == "Over")),
  paste("gee_asset:", aoi_asset)
))

# -----------------------------------------------------------------------------
# 7. Maps + representativeness check
# -----------------------------------------------------------------------------
frame_pts <- suppressWarnings(st_centroid(frame))
aoi_pts   <- suppressWarnings(st_centroid(aoi))
bb        <- st_bbox(eco)

p_map <- ggplot() +
  geom_sf(data = eco, aes(fill = us_l3name), color = "grey40", linewidth = 0.2, alpha = 0.35)

if (requireNamespace("maps", quietly = TRUE)) {
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>% st_transform(AOI_CRS)
  p_map <- p_map + geom_sf(data = states, fill = NA, color = "grey25", linewidth = 0.25)
}

p_map <- p_map +
  geom_sf(data = filter(frame_pts, !eligible), color = "grey75", size = 0.5) +
  geom_sf(data = filter(frame_pts,  eligible), color = "grey30", size = 0.7) +
  geom_sf(data = pilot_roi, fill = NA, color = "black", linetype = "dashed", linewidth = 0.5) +
  geom_sf(data = aoi_pts, aes(shape = siteuse), color = "black", fill = "red", size = 2.3, stroke = 0.6) +
  scale_shape_manual(values = c(Base = 21, Over = 4), name = "Sampled AOI") +
  coord_sf(crs = st_crs(AOI_CRS),
           xlim = as.numeric(bb[c("xmin", "xmax")]),
           ylim = as.numeric(bb[c("ymin", "ymax")])) +
  labs(
    title    = "Sampled Areas of Interest (hex centroids)",
    subtitle = sprintf(paste0("%d candidate hexes (light grey = ineligible, dark grey = eligible); ",
                              "%d base + %d replacement AOIs\n%s sample, seed = %d, run = %s. ",
                              "Dashed circle = pilot ROI."),
                       nrow(frame), sum(aoi$siteuse == "Base"), sum(aoi$siteuse == "Over"),
                       toupper(SAMPLING_METHOD), SEED, RUN_ID),
    fill = "EPA Level III ecoregion"
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave(file.path(outputDir, "map_aoi_sample_centroids.png"), p_map, width = 10, height = 8, dpi = 300)

# Does the base sample look like the eligible frame it was drawn from?
balance <- bind_rows(
  frame %>% st_drop_geometry() %>% filter(eligible) %>% mutate(set = "Eligible frame"),
  aoi   %>% st_drop_geometry() %>% filter(siteuse == "Base") %>% mutate(set = "Base sample")
) %>%
  transmute(set, us_l3name, centroid_lat, forest_frac,
            log10_large_loss_ha = log10(large_loss_ha)) %>%
  pivot_longer(c(centroid_lat, forest_frac, log10_large_loss_ha),
               names_to = "variable", values_to = "value")

p_balance <- ggplot(balance, aes(x = us_l3name, y = value, fill = set)) +
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.8)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c(`Eligible frame` = "grey70", `Base sample` = "firebrick"), name = NULL) +
  labs(title = "Sample vs. sampling frame, by ecoregion",
       subtitle = "Boxes should overlap; a big offset means the draw is unrepresentative on that variable.",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(outputDir, "aoi_sample_vs_frame.png"), p_balance, width = 9, height = 9, dpi = 300)

cat(sprintf("\nStep 0 outputs in %s:\n", outputDir),
    "  aoi_sampling_frame.gpkg, aoi_sample.gpkg, aoi_sample_centroids.csv\n",
    "  map_aoi_sample_centroids.png, aoi_sample_vs_frame.png\n",
    "  manifest_0_aoi_sample.txt\n",
    sprintf(" GEE asset for Steps 1-4c: %s\n", aoi_asset))
