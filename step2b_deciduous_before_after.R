# Step 2b: Deciduous/Mixed Fraction Per Patch, ALL NLCD Epochs
# -----------------------------------------------------------------------------
# PURPOSE: build the before-and-after deciduous criterion that the pipeline's
# study design calls for but never actually implemented.
#
# WHAT WAS MISSING. Step 2 builds a 41/43 fractional-cover RASTER, and Step 3
# computes forest_cover_mean + meets_forest_threshold per patch. Both are
# built from NLCD 2021 ONLY, and both are descriptive outputs -- Step 3's own
# header calls the threshold "a FLAG, not a filter, so you can decide later."
# Step 5 was the "later" and never applied it: meets_forest_threshold appears
# there exactly once, as a fill colour on map_patches_meeting_criteria.png.
#
# So two things were absent, not one:
#   1. The criterion was never applied as a filter.
#   2. There was no "before" layer at all -- a single 2021 snapshot means
#      "recovered to deciduous" for a patch cut in 2005, and "was deciduous"
#      for one cut in 2023. Filtering on it would apply opposite criteria to
#      different patches.
#
# WHY BEFORE-AND-AFTER MATTERS HERE, not just as bookkeeping: Part K of Step 5
# found that patches regrowing to EVERGREEN show the MidGreendown effect
# REVERSED relative to those regrowing deciduous. Species composition is not
# a nuisance variable in this analysis -- it changes the sign of the result.
# Requiring deciduous/mixed at both ends removes conversion as a confound, so
# the remaining signal is disturbance rather than species change.
#
# RELEASE CHOICE: everything comes from 2019_REL, which carries the full
# back-catalogue (2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019). The
# 2021_REL collection contains only 2021. Mixing releases would mean the
# "before" and "after" classifications come from different processing
# generations, and NLCD's own guidance is to use a single release's
# back-cast series for change analysis -- otherwise reclassification
# differences masquerade as land-cover change.
#
# Output: patch_decid_all_epochs.csv -- one row per patch >= 25 ha:
#   patch_uuid, loss_year, area_ha, decid_2001 ... decid_2019
#   (each = fraction of the patch classified 41 or 43 in that epoch)
#
# This script deliberately exports ALL epochs rather than a pre-computed
# before/after pair. The before/after matching rule and the threshold are
# decisions best made against real numbers, locally, without another GEE
# round-trip -- see section 4.
# -----------------------------------------------------------------------------
# Requires: rgee, googledrive; Step 1's patch asset must exist.
# =============================================================================

library(rgee)
library(googledrive)

ee_Initialize(drive = TRUE)

outputDir <- file.path("~/Google Drive/My Drive", "Reidy_research")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Patch catalog, restricted to MODIS-resolvable patches
#    Same >= 25 ha cutoff used by Step 4 and everywhere downstream, so this
#    table joins cleanly onto the 615-patch analysis population.
# -----------------------------------------------------------------------------
MODIS_PIXEL_AREA_HA <- 25

pv <- ee$FeatureCollection('projects/breidyee/assets/hansen_persistent_loss_vectors_2')$
  filter(ee$Filter$gte('area_ha', MODIS_PIXEL_AREA_HA))$
  select(list('patch_uuid', 'loss_year', 'area_ha'))

# -----------------------------------------------------------------------------
# 2. Deciduous/mixed fraction at every epoch
#    Images are selected by filtering on system:index rather than by building
#    an asset path string. The path form was guessed wrong once already
#    ("USGS/NLCD_RELEASES/2021_REL/NLCD/2001" does not exist), so this asks
#    the collection what it has instead of assuming.
# -----------------------------------------------------------------------------
nlcdCol <- ee$ImageCollection("USGS/NLCD_RELEASES/2019_REL/NLCD")
epochs  <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)

# Sanity check against the live collection before doing any work.
available <- nlcdCol$aggregate_array('system:index')$getInfo()
missing   <- setdiff(as.character(epochs), available)
if (length(missing) > 0) {
  stop('These epochs are not in 2019_REL: ', paste(missing, collapse = ', '),
       '. Available: ', paste(available, collapse = ', '))
}
cat('NLCD 2019_REL epochs confirmed:', paste(available, collapse = ', '), '\n')

# setOutputs() on every reducer, for the reason documented at length in
# Step 3 and Step 4: reduceRegions() on a SINGLE-BAND image with a
# SINGLE-OUTPUT reducer names the property after the REDUCER ('mean'), not
# the band. Without setOutputs() every epoch would overwrite the previous
# one's 'mean' column and the export would silently carry one epoch, not
# eight.
fc <- pv
for (yr in epochs) {
  img <- nlcdCol$
    filter(ee$Filter$eq('system:index', as.character(yr)))$
    first()$
    select('landcover')$
    remap(list(41L, 43L), list(1L, 1L), 0)
  
  fc <- img$reduceRegions(
    collection = fc,
    reducer    = ee$Reducer$mean()$setOutputs(list(paste0('decid_', yr))),
    scale      = 30,
    tileScale  = 4
  )
}

# -----------------------------------------------------------------------------
# 3. Export
# -----------------------------------------------------------------------------
decidColumns <- c('patch_uuid', 'loss_year', 'area_ha', paste0('decid_', epochs))

task <- ee_table_to_drive(
  collection  = fc,
  description = 'patch_decid_all_epochs',
  folder      = 'Reidy_research',
  fileFormat  = 'CSV',
  selectors   = decidColumns,
  timePrefix  = FALSE
)
task$start()
cat('Multi-epoch deciduous fraction export started.\n')
ee_monitoring(task)

# Newest-by-id download. GEE does not overwrite files on Drive, so repeated
# runs leave same-named copies behind and drive_ls()[1, ] has no guarantee
# of returning the current one -- this exact trap cost a debugging cycle in
# Step 3, where a stale 2-byte export was silently picked up.
Sys.sleep(15)
f <- drive_ls(path = 'Reidy_research', pattern = 'patch_decid_all_epochs')
stopifnot(nrow(f) >= 1)
mtimes <- sapply(f$drive_resource, function(x) x$modifiedTime)
cat(sprintf('Using export modified %s (of %d matching files).\n',
            max(mtimes), nrow(f)))

localPath <- file.path(outputDir, 'patch_decid_all_epochs.csv')
drive_download(as_id(f$id[order(mtimes, decreasing = TRUE)][1]),
               path = localPath, overwrite = TRUE)

decid <- read.csv(localPath)
cat(sprintf('Downloaded: %d patches x %d epochs.\n', nrow(decid), length(epochs)))
stopifnot(nrow(decid) > 0)

# -----------------------------------------------------------------------------
# 4. DECIDE the before/after rule and threshold -- against real numbers
#
#    Two choices to make, and neither should be made blind:
#
#    (a) WHICH EPOCH IS "AFTER". Matched lag (nearest epoch >= loss_year + 5)
#        rather than a fixed calendar year. A fixed year would give a patch
#        cut in 2002 seventeen years to return to deciduous but one cut in
#        2014 only five, biasing the filter against recent disturbances --
#        they'd be excluded for still being shrub even if on a deciduous
#        trajectory. Matched lag assesses every patch at the same
#        successional stage.
#
#    (b) THE THRESHOLD. 0.75 is inherited from Step 3, but it was never
#        stress-tested. Requiring >= 0.75 deciduous at BOTH ends, at 30 m,
#        inside patches mostly 25-50 ha, is demanding. The table below shows
#        what each threshold costs before you commit to one.
#
#    ELIGIBILITY: loss_year >= 2002 (so 2001 is genuinely pre-disturbance)
#    and an "after" epoch must exist (loss_year + 5 <= 2019, i.e.
#    loss_year <= 2014).
# -----------------------------------------------------------------------------
AFTER_LAG_YEARS <- 5

decid$after_epoch <- sapply(decid$loss_year, function(ly) {
  cand <- epochs[epochs >= ly + AFTER_LAG_YEARS]
  if (length(cand) == 0) NA_integer_ else min(cand)
})

decid$decid_after <- mapply(function(ep, i) {
  if (is.na(ep)) NA_real_ else decid[[paste0('decid_', ep)]][i]
}, decid$after_epoch, seq_len(nrow(decid)))

decid$decid_before <- decid$decid_2001

eligible <- subset(decid, loss_year >= 2002 & !is.na(after_epoch) & !is.na(decid_after))

cat(sprintf('\nEligible patches (loss_year 2002-%d, "after" epoch available): %d of %d\n',
            max(epochs) - AFTER_LAG_YEARS, nrow(eligible), nrow(decid)))
cat('Excluded: ', sum(decid$loss_year < 2002), ' with no clean "before" (cut in 2001); ',
    sum(is.na(decid$after_epoch)), ' with no "after" epoch (cut after ',
    max(epochs) - AFTER_LAG_YEARS, ').\n', sep = '')

cat('\nSensitivity of n to the threshold choice:\n')
thresholds <- c(0.50, 0.60, 0.70, 0.75, 0.80)
sens <- data.frame(
  threshold   = thresholds,
  before_only = sapply(thresholds, function(t) sum(eligible$decid_before >= t)),
  after_only  = sapply(thresholds, function(t) sum(eligible$decid_after  >= t)),
  both        = sapply(thresholds, function(t) sum(eligible$decid_before >= t &
                                                     eligible$decid_after  >= t))
)
print(sens, row.names = FALSE)

cat('\nHOW TO READ THIS. `both` is the analysis population under Path A.\n')
cat('  >150  : full re-run viable, moderators included.\n')
cat('  60-150: main period effects hold; Part H size interaction and Part K\n')
cat('          regrowth split should be dropped -- too thin to subgroup.\n')
cat('  <60   : 0.75 may be too strict at MODIS scale. Consider a lower\n')
cat('          threshold, but state it as a deliberate, justified choice --\n')
cat('          not one reverse-engineered to reach a comfortable n.\n')
cat('\nIf `before_only` is large but `both` is small, the loss is on the\n')
cat('RECOVERY side -- patches were deciduous and did not come back deciduous.\n')
cat('That is a substantive finding about this landscape, not a filtering\n')
cat('technicality, and belongs in the results either way.\n')

# -----------------------------------------------------------------------------
# 5. Write the filter table for Step 5 to join on
#
#    OUTCOME OF SECTION 4, and why the design changed:
#
#    The "after" criterion is NOT USABLE. At a +5yr lag only 11 patches are
#    >=0.75 deciduous at both ends, and relaxing the threshold to 0.50 lifts
#    that only to 17 -- so this is structural, not a calibration problem. The
#    reason is visible in the recovery-time cross-tab: patches 0-2 years post
#    disturbance are overwhelmingly grassland, 3-6 years are shrub, and only
#    at 7+ years does forest dominate. NLCD needs re-established canopy to
#    call something 41/43, so "clearcut, then >=75% deciduous five years
#    later" is close to self-contradictory -- it asks a patch to be both
#    meaningfully disturbed and to look undisturbed shortly after. The 11 that
#    pass are likely cases where Hansen flagged loss but little happened.
#
#    Separately, and independent of n: filtering on POST-disturbance
#    composition means conditioning on a post-treatment variable. Composition
#    after disturbance is an outcome, and Step 5's Part K showed it is related
#    to the response -- evergreen regrowth flips the sign of the MidGreendown
#    effect. Selecting on a variable that sits between treatment and outcome
#    biases the treatment effect, the same way estimating a drug's effect only
#    among patients who recovered would.
#
#    So: filter on BEFORE only. Pre-disturbance composition is fixed prior to
#    treatment, making it a legitimate eligibility criterion, and it enforces
#    the phenologically necessary precondition -- MidGreendown has to mean
#    something at baseline for a shift in it to be interpretable. Post-
#    disturbance composition is then carried as a MODERATOR (Part K), which is
#    where the interesting result lives anyway.
#
#    decid_before_after is still written, for transparency and in case a
#    reviewer asks -- but with n=11 it cannot support an analysis.
# -----------------------------------------------------------------------------
DECID_THRESHOLD <- 0.75

# PRIMARY criterion.
decid$decid_before_ok <- !is.na(decid$decid_before) &
  decid$decid_before >= DECID_THRESHOLD

# Retained for reference only -- n is far too small to analyse (see above).
decid$decid_before_after <- with(decid,
                                 !is.na(decid_before) & !is.na(decid_after) &
                                   loss_year >= 2002 &
                                   decid_before >= DECID_THRESHOLD & decid_after >= DECID_THRESHOLD)

filterTable <- decid[, c('patch_uuid', 'loss_year', 'area_ha',
                         'decid_before', 'after_epoch', 'decid_after',
                         'decid_before_ok', 'decid_before_after')]

write.csv(filterTable,
          file.path(outputDir, 'patch_deciduous_filter.csv'), row.names = FALSE)

cat(sprintf('\nAt threshold %.2f:\n', DECID_THRESHOLD))
cat(sprintf('  PRIMARY  -- deciduous BEFORE:        %d patches\n',
            sum(filterTable$decid_before_ok)))
cat(sprintf('  reference -- deciduous before AND after: %d patches (too few to use)\n',
            sum(filterTable$decid_before_after)))
cat('Written: patch_deciduous_filter.csv\n')

# Sanity check worth keeping: patches cut in 2001 should mostly FAIL the
# before-filter, because NLCD 2001's imagery already reflects a 2001 cut --
# 2001 is not genuinely "before" for them. If a lot of 2001 patches pass,
# that would suggest the before-layer isn't capturing pre-disturbance state.
n_2001_pass <- sum(decid$loss_year == 2001 & decid$decid_before_ok, na.rm = TRUE)
n_2001_all  <- sum(decid$loss_year == 2001, na.rm = TRUE)
cat(sprintf('\nCheck: %d of %d patches cut in 2001 pass the before-filter ',
            n_2001_pass, n_2001_all))
cat('(expected to be low -- NLCD 2001 already reflects a 2001 disturbance).\n')
