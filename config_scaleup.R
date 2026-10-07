# config_scaleup.R -- single source of truth for the SCALED-UP pipeline
# -----------------------------------------------------------------------------
# Every scale-up script starts with:
#   source("~/Google Drive/My Drive/Reidy_research/fall color code/config_scaleup.R")
#
# Deliberately NOT named config.R: Steps 1, 2, 4 and 4b of the pilot already
# source a file by that name, and dropping this one in would silently send
# their outputs to runs/<RUN_ID>/ instead of the pilot folder. Keep the pilot
# untouched until Steps 1-4c are refactored to read the AOI asset.
#
# Change scope, years, thresholds or the random seed HERE, never inside a step
# script. Bump RUN_ID whenever the scope or the sample changes, so a new run
# never overwrites (or gets silently mixed up with) an older run's outputs.
# =============================================================================

# ---- Run identity -------------------------------------------------------------
RUN_ID <- "appalachia_v1"
SEED   <- 42L

# ---- Paths --------------------------------------------------------------------
PROJECT_DIR <- path.expand("~/Google Drive/My Drive/Reidy_research")
CODE_DIR    <- file.path(PROJECT_DIR, "fall color code")
outputDir   <- file.path(PROJECT_DIR, "runs", RUN_ID)   # every local output for this run
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

DRIVE_FOLDER   <- paste0("fallcolor_", RUN_ID)          # GEE export folder (flat name)
GEE_ASSET_ROOT <- "projects/breidyee/assets"
asset_id <- function(name) paste0(GEE_ASSET_ROOT, "/", RUN_ID, "__", name)

# ---- Datasets -----------------------------------------------------------------
HANSEN_ID     <- "UMD/hansen/global_forest_change_2024_v1_12"
MOD13_ID      <- "MODIS/061/MOD13A1"
MCD12Q2_ID    <- "MODIS/061/MCD12Q2"            # in GEE through the 2023 product year (Oct 2026)
NLCD_COLL_ID  <- "USGS/NLCD_RELEASES/2019_REL/NLCD"   # one release for every epoch (Step 2b)
NLCD_EPOCHS   <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)  # system:index values
NLCD_PRE_YEAR <- 2001                            # AOI screening only; patches use before_epoch()
ECOREGION_ID  <- "EPA/Ecoregions/2013/L3"        # us_l3code is a STRING
DEM_ID        <- "USGS/SRTMGL1_003"

# True MODIS 500 m grid spacing (tile width 1111950.5197665 m / 2400 pixels).
# Use this, or better crs = modis_projection() with NO scale, never "500".
MODIS_PIXEL_M  <- 463.312716528
MODIS_PIXEL_HA <- MODIS_PIXEL_M^2 / 1e4          # ~21.47 ha, not 25

# ---- Spatial scope ------------------------------------------------------------
# EPA Level III, Level II 8.4 (Ozark/Ouachita-Appalachian Forests):
#   66 Blue Ridge | 67 Ridge and Valley | 69 Central Appalachians |
#   70 Western Allegheny Plateau
# The pilot ROI (-79.86, 37.83) already straddles 66/67/69.
ECOREGION_CODES <- c("66", "67", "69", "70")

AOI_CRS         <- 5070      # CONUS Albers equal-area, for hex geometry
HEX_CELLSIZE_M  <- 25000     # flat-to-flat; area = sqrt(3)/2 * d^2 ~= 541 km^2
HEX_GRID_ORIGIN <- c(0, 0)   # fixed anchor so the grid doesn't move if the
                             # ecoregion list changes (hex_ids stay stable)
MIN_HEX_IN_ECO  <- 0.75      # hex must be >= 75% inside ONE ecoregion

# ---- Eligibility (AOI screening) ----------------------------------------------
FOREST_CLASSES    <- c(41L, 43L)  # NLCD deciduous + mixed (42 evergreen excluded)
DECID_THRESHOLD   <- 0.75         # Step 2b PRIMARY filter: patch >= 75% 41/43 BEFORE loss
BEFORE_LAG_YEARS  <- 2L           # "before" epoch must be <= loss_year - 2 (NLCD epoch
                                  # imagery can come from the year either side of its label)
LOSS_YEAR_MIN     <- 2004L        # >= 3 pre-loss MCD12Q2 years (product starts 2001)
LOSS_YEAR_MAX     <- 2020L        # >= 3 post-loss years if MCD12Q2 ends 2023 -- re-check
MIN_FOREST_FRAC   <- 0.50         # share of land in deciduous/mixed forest in 2001
MIN_PATCH_HA      <- 25           # screening size for MODIS-scale clumps
MIN_LARGE_LOSS_HA <- 50           # hex needs >= this much loss in clumps >= MIN_PATCH_HA

# ---- Sampling -----------------------------------------------------------------
SAMPLING_METHOD      <- "grts"   # "grts" (spatially balanced, needs spsurvey) or "srs"
N_PER_ECOREGION      <- 25L      # 4 ecoregions x 25 = 100 base AOIs. Check Step 0's
                                 # total_large_loss_ha: after the deciduous-before
                                 # filter, yield per hex may be low enough to need more
N_OVER_PER_ECOREGION <- 5L       # ordered replacements if a base AOI fails QA later

# Test run: screen only this many randomly chosen candidate hexes, print the
# results, and stop before sampling. Use a small number (e.g. 20) for the
# first run to confirm the GEE export works; set to NA for the real run.
SCREEN_TEST_N <- 20L

# ---- Analysis windows (used by Step 5 once refactored) -----------------------
PRE_WINDOW  <- -3:-1
POST_WINDOW <-  1:3

# =============================================================================
# Helpers
# =============================================================================

# MODIS sinusoidal projection at its NATIVE scale. Pass this as `crs =` to
# reduceRegions()/reproject()/Export WITHOUT a `scale =` argument; adding
# scale = 500 silently builds a different (500 m) grid.
modis_projection <- function() {
  rgee::ee$ImageCollection(MOD13_ID)$first()$select("NDVI")$projection()
}

# Export a FeatureCollection to Drive as CSV, wait for it, and download EXACTLY
# that task's file. Replaces the drive_ls(pattern = ...)[1, ] pattern, which
# picks an arbitrary file when Drive holds duplicates (GEE never overwrites).
export_table_csv <- function(fc, name, selectors = NULL, local_dir = outputDir) {
  task <- rgee::ee_table_to_drive(
    collection     = fc,
    description    = name,
    folder         = DRIVE_FOLDER,
    fileNamePrefix = name,
    fileFormat     = "CSV",
    selectors      = selectors,
    timePrefix     = FALSE
  )
  task$start()
  rgee::ee_monitoring(task)
  dsn <- file.path(local_dir, paste0(name, ".csv"))
  # consider = "last": if Drive has several files with this name, take the
  # newest rather than prompting interactively.
  rgee::ee_drive_to_local(task = task, dsn = dsn, overwrite = TRUE, consider = "last")
  dsn
}

# Most recent NLCD epoch that is genuinely BEFORE a patch's loss
# (epoch <= loss_year - BEFORE_LAG_YEARS). Vectorised over loss_year.
# Replaces Step 2b's fixed decid_2001, which is ~13 years stale for a 2015 cut.
before_epoch <- function(loss_year) {
  vapply(loss_year, function(ly) {
    cand <- NLCD_EPOCHS[NLCD_EPOCHS <= ly - BEFORE_LAG_YEARS]
    if (length(cand) == 0) NA_real_ else max(cand)
  }, numeric(1))
}

# Append a small provenance record for each step.
write_manifest <- function(step, lines = character()) {
  f <- file.path(outputDir, paste0("manifest_", step, ".txt"))
  writeLines(c(
    paste("step:", step),
    paste("run_id:", RUN_ID),
    paste("created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("seed:", SEED),
    lines,
    "",
    "---- sessionInfo ----",
    utils::capture.output(utils::sessionInfo())
  ), f)
  invisible(f)
}
