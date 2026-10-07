# Scale-up Step 2: check each hex for hardwood forest + cutting, swap out the ones that fail
# -----------------------------------------------------------------------------
# Self-contained, like Step 1. Open in RStudio and click "Source".
# Needs: rgee, googledrive, sf, dplyr, ggplot2
# Reads:  scaleup/aoi_candidates_ordered.gpkg   (made by Step 1)
#
# What it does:
#   1. Loads Step 1's hexes, in their saved random order
#   2. Sends them to Earth Engine and measures, for each hex:
#        land_ha       land area
#        decid_ha      hardwood (deciduous + mixed) forest in 2001, NLCD 41/43
#        qual_loss_ha  forest loss 2004-2020 that was hardwood in 2001
#                      (same "persistent loss" rule as your Step 1: loss and no gain)
#        big_loss_ha   the part of qual_loss in clumps of >= 25 ha
#   3. A hex PASSES if  >= 50% of its land was hardwood forest  AND
#                       >= 50 ha of loss sits in clumps of >= 25 ha
#   4. Walks down each ecoregion's random order and keeps the first 25 that
#      pass. Any original pick that fails is replaced by the next hex on the
#      list, so the sample stays random. (No new randomness here: same
#      Step 1 file + same settings = same result every time.)
#   5. Saves the results, maps them, and uploads the final 100 hexes to
#      Earth Engine for the later steps.
#
# FIRST RUN: set MAX_SCREEN_PER_ECOREGION <- 3 to check everything works in a
# few minutes (it will say "not enough hexes" -- that's expected for a test).
# Then set it back to Inf for the real run, which can take a while.
# =============================================================================

library(rgee)
library(googledrive)
library(sf)
library(dplyr)
library(ggplot2)

ee_Initialize(drive = TRUE)

# ---- Settings ---------------------------------------------------------------
N_PER_ECOREGION          <- 25     # hexes to keep per ecoregion
MAX_SCREEN_PER_ECOREGION <- Inf    # Inf = check every candidate; 3 = quick test
MIN_FOREST_FRAC          <- 0.50   # share of land that was hardwood forest in 2001
MIN_CLUMP_HA             <- 25     # a clump of loss must be at least this big to count
MIN_BIG_LOSS_HA          <- 50     # total loss in big clumps needed per hex
LOSS_YEAR_MIN            <- 2004   # >= 3 years of before data (phenology starts 2001)
LOSS_YEAR_MAX            <- 2020   # >= 3 years of after data

outDir      <- file.path("~/Google Drive/My Drive/Reidy_research", "scaleup")
driveFolder <- "Reidy_research"
assetId     <- "projects/breidyee/assets/scaleup_aoi_hexes"

# ---- 1. Load Step 1's hexes -------------------------------------------------
step1_file <- file.path(outDir, "aoi_candidates_ordered.gpkg")
if (!file.exists(step1_file)) {
  stop("Can't find Step 1's output at:\n  ", path.expand(step1_file),
       "\nRun scaleup_step1_sample_AOIs.R first.", call. = FALSE)
}

cand <- st_read(step1_file, quiet = TRUE) %>%
  mutate(us_l3code = as.character(us_l3code),
         sampled   = as.logical(sampled)) %>%
  arrange(us_l3code, draw_order)

to_screen <- cand %>% filter(draw_order <= MAX_SCREEN_PER_ECOREGION)
cat(sprintf("\nScreening %d of %d candidate hexes.\n", nrow(to_screen), nrow(cand)))

# ---- 2. Measure forest and loss in each hex (Earth Engine) ------------------
hex_ee <- sf_as_ee(st_transform(to_screen["hex_id"], 4326), via = "getInfo")

hansen   <- ee$Image("UMD/hansen/global_forest_change_2024_v1_12")
lossyear <- hansen$select("lossyear")
pixelHa  <- ee$Image$pixelArea()$divide(10000)
land     <- hansen$select("datamask")$eq(1)

nlcd2001 <- ee$ImageCollection("USGS/NLCD_RELEASES/2019_REL/NLCD")$
  filter(ee$Filter$eq("system:index", "2001"))$
  first()$
  select("landcover")
decid2001 <- nlcd2001$remap(list(41L, 43L), list(1L, 1L), 0)$unmask(0)

qualLoss <- hansen$select("loss")$eq(1)$
  And(hansen$select("gain")$eq(0))$
  And(lossyear$gte(LOSS_YEAR_MIN - 2000))$
  And(lossyear$lte(LOSS_YEAR_MAX - 2000))$
  And(decid2001$eq(1))

# Size of the connected clump each loss pixel belongs to (pixels from the
# same loss year that touch side-to-side, like your Step 1 patches).
clumpPx <- lossyear$updateMask(qualLoss)$connectedPixelCount(maxSize = 1024L, eightConnected = FALSE)
clumpHa <- clumpPx$multiply(ee$Image$pixelArea())$divide(10000)
bigLoss <- clumpHa$gte(MIN_CLUMP_HA)$unmask(0)$And(qualLoss)

# Several bands + one reducer -> each output column is named after its band.
statsImg <- land$multiply(pixelHa)$rename("land_ha")$
  addBands(decid2001$multiply(pixelHa)$rename("decid_ha"))$
  addBands(qualLoss$multiply(pixelHa)$rename("qual_loss_ha"))$
  addBands(bigLoss$multiply(pixelHa)$rename("big_loss_ha"))

hexStats <- statsImg$reduceRegions(
  collection = hex_ee,
  reducer    = ee$Reducer$sum(),
  crs        = hansen$select("loss")$projection(),   # Hansen's own 30 m grid
  tileScale  = 16
)

# Unique name every run, so the download can never pick up an old file.
task_name <- paste0("aoi_screening_", format(Sys.time(), "%Y%m%d_%H%M%S"))

task <- ee_table_to_drive(
  collection  = hexStats,
  description = task_name,
  folder      = driveFolder,
  fileFormat  = "CSV",
  selectors   = c("hex_id", "land_ha", "decid_ha", "qual_loss_ha", "big_loss_ha"),
  timePrefix  = FALSE
)
task$start()
cat("Earth Engine task started:", task_name,
    "\n(You can watch it in the Tasks tab of the GEE Code Editor.)\n")
ee_monitoring(task)

state <- task$status()$state
if (state != "COMPLETED") stop("Earth Engine task ended as ", state, call. = FALSE)

Sys.sleep(10)
f <- drive_ls(path = driveFolder, pattern = task_name)
if (nrow(f) == 0) stop("Export finished but the file isn't in Drive yet. Wait a minute and re-run from here.")
screen_csv <- file.path(outDir, paste0(task_name, ".csv"))
drive_download(as_id(f$id[1]), path = screen_csv, overwrite = TRUE)

scr <- read.csv(screen_csv)
stopifnot(nrow(scr) == nrow(to_screen))

# ---- 3 & 4. Pass/fail, then keep the first 25 passing hexes per ecoregion ---
res <- to_screen %>%
  inner_join(scr, by = "hex_id") %>%
  mutate(
    forest_frac = ifelse(land_ha > 0, decid_ha / land_ha, NA_real_),
    passes      = !is.na(forest_frac) &
                  forest_frac >= MIN_FOREST_FRAC &
                  big_loss_ha >= MIN_BIG_LOSS_HA
  ) %>%
  arrange(us_l3code, draw_order) %>%
  group_by(us_l3code) %>%
  mutate(selected = passes & cumsum(passes) <= N_PER_ECOREGION) %>%
  ungroup() %>%
  mutate(status = case_when(
    selected & sampled  ~ "Kept (original pick)",
    selected & !sampled ~ "Replacement",
    sampled & !passes   ~ "Dropped (failed check)",
    TRUE                ~ "Not used"
  ))

summary_tbl <- res %>%
  st_drop_geometry() %>%
  group_by(us_l3name) %>%
  summarise(
    screened         = n(),
    pass             = sum(passes),
    selected         = sum(selected),
    original_kept    = sum(selected & sampled),
    replacements     = sum(selected & !sampled),
    deepest_pick     = if (any(selected)) max(draw_order[selected]) else NA_integer_,
    big_loss_ha      = round(sum(big_loss_ha[selected])),
    max_patches      = floor(sum(big_loss_ha[selected]) / MIN_CLUMP_HA),
    .groups = "drop"
  )

cat("\nResults by ecoregion:\n")
as.data.frame(summary_tbl)
cat("\nmax_patches = most 25 ha+ patches the selected hexes could hold (an upper bound).\n")

complete <- all(summary_tbl$selected == N_PER_ECOREGION) &&
            nrow(summary_tbl) == length(unique(cand$us_l3code))
if (!complete) {
  warning("Some ecoregions have fewer than ", N_PER_ECOREGION, " passing hexes. ",
          if (is.finite(MAX_SCREEN_PER_ECOREGION))
            "This was a test run (MAX_SCREEN_PER_ECOREGION is limited) -- set it to Inf."
          else
            "Lower MIN_FOREST_FRAC or MIN_BIG_LOSS_HA, or keep fewer per ecoregion.",
          call. = FALSE)
}

# ---- 5. Save, map, upload ---------------------------------------------------
write.csv(st_drop_geometry(res), file.path(outDir, "aoi_screening_results.csv"), row.names = FALSE)

final <- res %>%
  filter(selected) %>%
  select(hex_id, us_l3code, us_l3name, draw_order, status, forest_frac,
         qual_loss_ha, big_loss_ha, lon, lat)
st_write(final, file.path(outDir, "aoi_final.gpkg"), delete_dsn = TRUE, quiet = TRUE)

p <- ggplot() +
  geom_sf(data = cand, fill = NA, color = "grey85", linewidth = 0.1) +
  geom_sf(data = res, aes(fill = status), color = "grey50", linewidth = 0.1) +
  scale_fill_manual(values = c("Kept (original pick)"   = "red",
                               "Replacement"            = "orange",
                               "Dropped (failed check)" = "black",
                               "Not used"               = "grey90"),
                    name = NULL) +
  labs(title = "Final AOI hexes after the forest + loss check",
       subtitle = sprintf("Pass = >= %.0f%% hardwood forest in 2001 and >= %.0f ha of %d-%d loss in clumps >= %.0f ha",
                          100 * MIN_FOREST_FRAC, MIN_BIG_LOSS_HA, LOSS_YEAR_MIN, LOSS_YEAR_MAX, MIN_CLUMP_HA)) +
  theme_minimal()
print(p)
ggsave(file.path(outDir, "map_aoi_final.png"), p, width = 10, height = 8, dpi = 200)

if (complete) {
  sf_as_ee(st_transform(final, 4326), via = "getInfo_to_asset",
           assetId = assetId, overwrite = TRUE)
  cat("\nFinal", nrow(final), "hexes uploaded to Earth Engine as:\n ", assetId, "\n")
} else {
  cat("\nNot uploading to Earth Engine until every ecoregion has", N_PER_ECOREGION, "hexes.\n")
}

cat("\nDone. Files in", outDir, ":\n",
    " aoi_screening_results.csv  (every screened hex, with its numbers)\n",
    " aoi_final.gpkg             (the hexes to study)\n",
    " map_aoi_final.png\n")
