# Patch Step 5: does forest loss change greenness and fall timing?
# -----------------------------------------------------------------------------
# Runs entirely in R. Open in RStudio and click "Source". Takes a minute or two.
# Needs: dplyr, tidyr, ggplot2, lme4, lmerTest
# Reads:  scaleup_patches/timeseries/patch_year_series.csv   (Patch Step 4)
#
# THE COMPARISON
#   For every patch and year, Step 4 already subtracted the patch's own nearby
#   uncut forest ("controls"):   difference = patch value - control value.
#   That removes weather and other year-to-year swings shared by the patch and
#   its surroundings. Each patch is compared with its OWN never-cut controls,
#   not with other patches cut in other years -- so the different loss years
#   don't bias the result.
#
# What it does (for the 100 patches in the analysis set, years -5 to +10):
#   1. Describes the analysis set (regions, loss years, sizes).
#   2. EVENT STUDY, one model per variable:
#        difference ~ year-since-loss + (1 | ecoregion) + (1 | patch)
#      Each year is compared with the year before the loss (-1). The pre-loss
#      years (-5 to -2) should sit near 0: that's the check that patches and
#      controls were behaving alike before the loss ("parallel trends").
#   3. SUMMARY EFFECTS: early (years 1-3) and late (4-10) vs before the loss,
#      for each variable, with p-values adjusted for testing 5 variables x 2
#      periods (Holm). NDVI and MidGreendown are the main outcomes.
#   4. BY REGION (exploratory): early and late effects within each Level II
#      region that has at least 5 patches.
#   5. Figures: event-study plots, raw patch vs control trajectories, region
#      effects.
#
# Reading the numbers: NDVI differences are in NDVI units. Phenology
# differences are in days; positive = LATER than the controls, negative = earlier.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# ---- Settings ---------------------------------------------------------------
OUTCOMES <- c(ndvi         = "July NDVI",
              greenup      = "Greenup (day of year)",
              senescence   = "Senescence (day of year)",
              midgreendown = "MidGreendown (day of year)",
              dormancy     = "Dormancy (day of year)")
PRIMARY  <- c("ndvi", "midgreendown")
REL_MIN  <- -5          # earliest year relative to the loss
REL_MAX  <- 10          # latest year relative to the loss
EARLY    <- 1:3         # "early" years after the loss
LATE     <- 4:10        # "late" years after the loss
MIN_PATCHES_REGION <- 5 # region results only where at least this many patches

outDir <- file.path("~/Google Drive/My Drive/Reidy_research", "scaleup_patches")
tsDir  <- file.path(outDir, "timeseries")
resDir <- file.path(outDir, "results")
dir.create(resDir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. The analysis set ----------------------------------------------------
ser <- read.csv(file.path(tsDir, "patch_year_series.csv"))
d <- ser %>%
  filter(as.logical(in_analysis), years_since_loss >= REL_MIN, years_since_loss <= REL_MAX) %>%
  mutate(patch_id  = factor(patch_id),
         us_l3code = factor(us_l3code),
         na_l2code = factor(na_l2code))

patches <- d %>% distinct(patch_id, na_l2code, us_l3code, loss_year, area_ha)
cat(sprintf("\nAnalysis set: %d patches, %d patch-years\n", nrow(patches), nrow(d)))
cat("\nPatches by Level II region:\n");  print(table(patches$na_l2code))
cat("\nPatches by year of loss:\n");     print(table(patches$loss_year))
cat("\nPatch size (ha):\n");             print(round(summary(patches$area_ha), 1))
write.csv(patches, file.path(resDir, "analysis_set_patches.csv"), row.names = FALSE)

prep <- function(var) {
  d %>%
    mutate(y = .data[[paste0(var, "_diff")]]) %>%
    filter(!is.na(y))
}

# ---- 2. Event study ---------------------------------------------------------
event_rows <- list()
pretrend   <- list()
for (var in names(OUTCOMES)) {
  dd <- prep(var) %>% mutate(rel = relevel(factor(years_since_loss), ref = "-1"))
  m  <- lmer(y ~ rel + (1 | us_l3code) + (1 | patch_id), data = dd)
  co <- summary(m)$coefficients
  k  <- grep("^rel", rownames(co))
  event_rows[[var]] <- bind_rows(
    tibble(outcome = var, rel_year = as.integer(sub("^rel", "", rownames(co)[k])),
           estimate = co[k, "Estimate"], se = co[k, "Std. Error"], p = co[k, "Pr(>|t|)"]),
    tibble(outcome = var, rel_year = -1L, estimate = 0, se = 0, p = NA_real_))
  
  # Joint test: are the pre-loss years (-5 to -2) all ~0?
  pre_terms <- intersect(paste0("rel", -5:-2), names(fixef(m)))
  L <- matrix(0, nrow = length(pre_terms), ncol = length(fixef(m)),
              dimnames = list(pre_terms, names(fixef(m))))
  for (j in seq_along(pre_terms)) L[j, pre_terms[j]] <- 1
  ct <- lmerTest::contest(m, L, joint = TRUE)
  pretrend[[var]] <- tibble(outcome = var, pre_years_tested = length(pre_terms),
                            F_value = ct$`F value`, p = ct$`Pr(>F)`,
                            patches = n_distinct(dd$patch_id), patch_years = nrow(dd))
}
event    <- bind_rows(event_rows) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         label = OUTCOMES[outcome]) %>%
  arrange(outcome, rel_year)
pretrend <- bind_rows(pretrend)
write.csv(event,    file.path(resDir, "event_study_coefficients.csv"), row.names = FALSE)
write.csv(pretrend, file.path(resDir, "pretrend_tests.csv"),           row.names = FALSE)

cat("\nPre-loss check (p > 0.05 = no sign the patches were already drifting before the loss):\n")
print(as.data.frame(pretrend %>% mutate(across(c(F_value, p), ~ round(.x, 3)))), row.names = FALSE)

# ---- 3. Summary effects: early and late vs before ---------------------------
period_of <- function(r) case_when(r < 0 ~ "pre", r %in% EARLY ~ "early", r %in% LATE ~ "late")

fit_period <- function(dd, re_ecoregion = TRUE) {
  dd <- dd %>%
    mutate(period = factor(period_of(years_since_loss), levels = c("pre", "early", "late"))) %>%
    filter(!is.na(period))
  f <- if (re_ecoregion) y ~ period + (1 | us_l3code) + (1 | patch_id) else y ~ period + (1 | patch_id)
  m  <- lmer(f, data = dd)
  co <- summary(m)$coefficients
  tibble(period = c("early", "late"),
         estimate = co[c("periodearly", "periodlate"), "Estimate"],
         se       = co[c("periodearly", "periodlate"), "Std. Error"],
         p        = co[c("periodearly", "periodlate"), "Pr(>|t|)"],
         patches  = n_distinct(dd$patch_id))
}

period_eff <- bind_rows(lapply(names(OUTCOMES), function(var) {
  fit_period(prep(var)) %>% mutate(outcome = var, .before = 1)
})) %>%
  mutate(ci_lo   = estimate - 1.96 * se,
         ci_hi   = estimate + 1.96 * se,
         p_holm  = p.adjust(p, method = "holm"),
         primary = outcome %in% PRIMARY,
         label   = OUTCOMES[outcome])
write.csv(period_eff, file.path(resDir, "period_effects.csv"), row.names = FALSE)

cat("\nEffect of forest loss (patch minus controls, compared with before the loss):\n")
print(as.data.frame(period_eff %>%
                      transmute(outcome, period, estimate = signif(estimate, 3),
                                ci = sprintf("%.3g to %.3g", ci_lo, ci_hi),
                                p = signif(p, 3), p_holm = signif(p_holm, 3), primary)), row.names = FALSE)

# ---- 4. By region (exploratory) ---------------------------------------------
regions <- names(which(table(patches$na_l2code) >= MIN_PATCHES_REGION))
region_eff <- bind_rows(lapply(PRIMARY, function(var) {
  bind_rows(lapply(regions, function(r) {
    dd <- prep(var) %>% filter(na_l2code == r)
    out <- tryCatch(fit_period(dd, re_ecoregion = FALSE), error = function(e) NULL)
    if (is.null(out)) return(NULL)
    out %>% mutate(outcome = var, na_l2code = r, .before = 1)
  }))
}))
if (nrow(region_eff) > 0) {
  region_eff <- region_eff %>%
    mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se, label = OUTCOMES[outcome])
}
write.csv(region_eff, file.path(resDir, "region_effects_exploratory.csv"), row.names = FALSE)

cat(sprintf("\nBy Level II region (exploratory; regions with >= %d patches):\n", MIN_PATCHES_REGION))
if (nrow(region_eff) > 0) {
  print(as.data.frame(region_eff %>%
                        transmute(outcome, region = na_l2code, period, patches, estimate = signif(estimate, 3),
                                  ci = sprintf("%.3g to %.3g", ci_lo, ci_hi))), row.names = FALSE)
}

# ---- 5. Figures -------------------------------------------------------------
plot_event <- function(df, title) {
  ggplot(df, aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = REL_MIN - 0.5, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey92") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "darkgreen", alpha = 0.2) +
    geom_line(color = "darkgreen") +
    geom_point(color = "darkgreen", size = 1.3) +
    facet_wrap(~ label, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = REL_MIN:REL_MAX) +
    labs(title = title,
         subtitle = paste0(),
         x = "Years since loss", y = "Change relative to year -1") +
    theme_minimal()
}
p1 <- plot_event(filter(event, outcome %in% PRIMARY),
                 sprintf("Ndvi/ Midgreendown vs year (n = %d patches)", nrow(patches)))
print(p1)
ggsave(file.path(resDir, "event_study_main.png"), p1, width = 8, height = 7, dpi = 200)

p2 <- plot_event(event, sprintf("Ndvi/ Midgreendown vs year (n = %d patches)", nrow(patches)))
ggsave(file.path(resDir, "event_study_all.png"), p2, width = 8, height = 12, dpi = 200)

traj <- bind_rows(lapply(PRIMARY, function(var) {
  d %>%
    select(patch_id, years_since_loss, Patch = all_of(var), Controls = all_of(paste0("ctrl_", var))) %>%
    pivot_longer(c(Patch, Controls), names_to = "series", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(series, years_since_loss) %>%
    summarise(mean = mean(value), se = sd(value) / sqrt(n()), .groups = "drop") %>%
    mutate(label = OUTCOMES[var])
}))
p3 <- ggplot(traj, aes(x = years_since_loss, y = mean, color = series, fill = series)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line() +
  facet_wrap(~ label, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(Patch = "darkorange", Controls = "darkgreen"), name = NULL) +
  scale_fill_manual(values = c(Patch = "darkorange", Controls = "darkgreen"), name = NULL) +
  scale_x_continuous(breaks = REL_MIN:REL_MAX) +
  labs(title = "Raw values: disturbed patches vs their nearby uncut forest",
       x = "Years since loss", y = NULL) +
  theme_minimal()
print(p3)
ggsave(file.path(resDir, "trajectories_patch_vs_control.png"), p3, width = 8, height = 7, dpi = 200)

if (nrow(region_eff) > 0) {
  p4 <- ggplot(region_eff, aes(x = na_l2code, y = estimate, color = period)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5)) +
    facet_wrap(~ label, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c(early = "darkorange", late = "darkred"), name = "After loss") +
    labs(title = "Effect by Level II region (exploratory)",
         subtitle = "Early = years 1-3, late = years 4-10, each vs before the loss. Bars = 95% CI.",
         x = "Level II ecoregion", y = "Patch minus controls, vs before") +
    theme_minimal()
  ggsave(file.path(resDir, "region_effects_exploratory.png"), p4, width = 8, height = 7, dpi = 200)
}

cat("\nDone. Files in", resDir, ":\n",
    " event_study_main.png, event_study_all.png, trajectories_patch_vs_control.png,\n",
    " region_effects_exploratory.png\n",
    " period_effects.csv, event_study_coefficients.csv, pretrend_tests.csv,\n",
    " region_effects_exploratory.csv, analysis_set_patches.csv\n")
