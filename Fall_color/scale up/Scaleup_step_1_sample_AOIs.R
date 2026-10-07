# Scale-up Step 1: choose the ecoregions and draw a repeatable random sample of AOIs
# -----------------------------------------------------------------------------
# Self-contained: no config file, no Google Drive downloads.
# Open in RStudio and click "Source" (runs top to bottom).
# Needs: rgee (already working for you), sf, dplyr, ggplot2
#
# What it does:
#   1. Gets the 4 EPA Level III ecoregions from Earth Engine
#   2. Covers them with ~25 km hexagons (each hex = one candidate AOI)
#   3. Keeps hexes that are >= 75% inside a single ecoregion
#   4. Puts each ecoregion's hexes in a random order (set by SEED);
#      the first 25 in each ecoregion are the sample
#   5. Saves the list and maps the centroids
#
# The full random ORDER is saved, not just the 25. In step 2 we'll check
# each sampled hex for deciduous forest + loss; any hex that fails is replaced
# by the next one down the list, which keeps the sample random.
# =============================================================================

library(rgee)
library(sf)
library(dplyr)
library(ggplot2)

ee_Initialize()

# ---- Settings ---------------------------------------------------------------
SEED            <- 42
ECOREGION_CODES <- c("66", "67", "69", "70")
#   66 Blue Ridge | 67 Ridge and Valley | 69 Central Appalachians |
#   70 Western Allegheny Plateau
HEX_WIDTH_KM    <- 25     # each hex ~541 km2
MIN_SHARE       <- 0.75   # hex must be >= 75% inside one ecoregion
N_PER_ECOREGION <- 25     # 4 ecoregions x 25 = 100 AOIs

outDir <- file.path("~/Google Drive/My Drive/Reidy_research", "scaleup")
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Ecoregions ----------------------------------------------------------
eco_ee <- ee$FeatureCollection("EPA/Ecoregions/2013/L3")$
  filter(ee$Filter$inList("us_l3code", as.list(ECOREGION_CODES)))$
  map(ee_utils_pyfunc(function(f) f$simplify(maxError = 250)))

eco <- ee_as_sf(eco_ee, maxFeatures = 10000) %>%   # if this errors on size, add: via = "drive"
  st_transform(5070) %>%                           # equal-area, metres
  st_make_valid() %>%
  group_by(us_l3code, us_l3name) %>%
  summarise(.groups = "drop")

cat("\nEcoregions loaded:\n")
print(st_drop_geometry(eco))
stopifnot(nrow(eco) == length(ECOREGION_CODES))

# ---- 2. Hexagon grid --------------------------------------------------------
grid <- st_make_grid(eco, cellsize = HEX_WIDTH_KM * 1000, square = FALSE)
hex  <- st_sf(geometry = grid)

xy <- st_coordinates(st_centroid(grid))
hex$hex_id  <- sprintf("hx_%d_%d", round(xy[, 1] / 1000), round(xy[, 2] / 1000))
hex$area_m2 <- as.numeric(st_area(hex))

# ---- 3. Keep hexes mostly inside ONE ecoregion ------------------------------
ov <- st_intersection(hex[, c("hex_id", "area_m2")], eco[, c("us_l3code", "us_l3name")])
ov$share <- as.numeric(st_area(ov)) / ov$area_m2

best <- ov %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  slice_max(share, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(share >= MIN_SHARE) %>%
  select(hex_id, us_l3code, us_l3name, share)

cand <- hex %>% inner_join(best, by = "hex_id")

cat("\nCandidate hexes per ecoregion:\n")
table(cand$us_l3name)

# ---- 4. Random order within each ecoregion (repeatable via SEED) -----------
set.seed(SEED)
cand <- cand %>%
  arrange(us_l3code, hex_id) %>%          # fixed starting order -> same draw every run
  group_by(us_l3code) %>%
  mutate(draw_order = sample(n())) %>%
  ungroup() %>%
  mutate(sampled = draw_order <= N_PER_ECOREGION) %>%
  arrange(us_l3code, draw_order)

ll <- st_coordinates(st_transform(st_centroid(st_geometry(cand)), 4326))
cand$lon <- ll[, 1]
cand$lat <- ll[, 2]

cat("\nSampled hexes per ecoregion:\n")
table(cand$us_l3name, cand$sampled)

# ---- 5. Save ----------------------------------------------------------------
st_write(cand, file.path(outDir, "aoi_candidates_ordered.gpkg"), delete_dsn = TRUE, quiet = TRUE)
write.csv(st_drop_geometry(cand), file.path(outDir, "aoi_candidates_ordered.csv"), row.names = FALSE)

# ---- 6. Map the centroids ---------------------------------------------------
cent  <- suppressWarnings(st_centroid(cand))
pilot <- st_sfc(st_point(c(-79.862539, 37.829550)), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(100000)

ggplot() +
  geom_sf(data = eco, aes(fill = us_l3name), alpha = 0.3, color = "grey40") +
  geom_sf(data = filter(cent, !sampled), color = "grey60", size = 0.6) +
  geom_sf(data = filter(cent,  sampled), color = "red", size = 1.8) +
  geom_sf(data = pilot, fill = NA, color = "black", linetype = "dashed") +
  labs(title    = "Sampled AOIs (red) among candidate hexes (grey)",
       subtitle = sprintf("%d per ecoregion, seed = %d. Dashed circle = pilot study area.",
                          N_PER_ECOREGION, SEED),
       fill     = "Ecoregion") +
  theme_minimal()


ggsave(file.path(outDir, "map_aoi_sample.png"), p, width = 10, height = 8, dpi = 200)


