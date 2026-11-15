# ============================================================================
#  GAMERAD_CLEAN.R - main analysis script for GameRAD ("Relationship Detective")
#  "Children's recognition of relational models from vignettes"
#
#  This script:
#    - loads the CHS-merged trial-level export
#      ("Analysis/GameRAD_data_with_age.csv"; falls back to the Sheet export)
#    - applies the PRE-REGISTERED inclusion cascade (OSF pre-registration, S5)
#    - computes per-child signal-detection scores (d-prime, criterion c) with
#      the log-linear correction (Hautus, 1995)
#    - runs the continuous age-in-days analysis
#    - produces the d-prime x age figures, the question-model d' figure, and
#      the circles (model-ID) accuracy figure
#
#  All paths use here::here() so the script runs from the project root
#  (the GameRAD repo folder).
#
#  DV CODING (everywhere): InitialYesNo is the child's binary response.
#  A trial is SIGNAL if IsCongruent == TRUE (QuestionModel matches the
#  VignetteModel; KeyedAnswer = YES) and NOISE if IsCongruent == FALSE.
#    hit         = YES on a congruent (signal) trial
#    false alarm = YES on an incongruent (noise) trial
#
#  DESIGN
#    12 vignettes per child, each narrated under one relational model
#    (EM = equality matching / AR = authority ranking / CS = communal sharing);
#    1 circles (3AFC diagram) trial + 3 yes/no test questions per vignette
#    -> 36 y/n trials (12 congruent, 24 incongruent) + 12 circles trials.
#    Type == "model-id" rows are the circles task (chance = 1/3).
#
#  SIGNAL DETECTION
#    Hit rate  H = (hits + 0.5) / (N_congruent   + 1)   [log-linear, all Ss]
#    FA  rate  F = (FAs  + 0.5) / (N_incongruent + 1)
#    d-prime     = qnorm(H) - qnorm(F)
#    criterion c = -0.5 * (qnorm(H) + qnorm(F))
#
#  PRE-REGISTERED INCLUSION CASCADE (applied in order; OSF prereg S5)
#    1. Session-level: >= 6 of 12 vignettes completed.
#    2. One session per child: CHS child_hashed_id identifies children across
#       ParticipantIDs; the most complete session is kept (ties -> earliest).
#       Sessions without a ParticipantID (developer/test rows) are excluded,
#       as are sessions whose ChildName marks a test/pilot run.
#    3. Age window: 4y9m to 9y0m. Continuous age = age_in_days / 365.25
#       (CHS birthdate to session start) where available; caregiver-reported
#       year + 0.5 otherwise.
#    4. YES-bias filter (Y/N analyses only): children with a yes-rate of 0%
#       or 100% are excluded from yes/no analyses, retained for circles.
#
#  Companions: GameRAD_dprime.xlsx (live-spreadsheet version of the d'
#  computation); OSF pre-registration (H1-H5 brms models are NOT yet
#  implemented here -- per-child summaries below are descriptive).
# ============================================================================

library(here)   # for project-relative paths


# SETUP ----

## Packages ----

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c(
  "ggplot2", "tidyr", "readr", "stringr", "scales",
  "cowplot", "tidyverse", "dplyr"
)
ipak(packages)

library(dplyr)  # load last to avoid masking


## Paths ----
# GameRAD repo layout: this script and the CSVs sit in the root (the CHS-merged
# CSV in Analysis/); outputs go to tables/ and figures/.

raw_dir     <- here::here()
results_dir <- here::here("tables")
fig_dir     <- here::here("figures")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir,     recursive = TRUE, showWarnings = FALSE)


## Shared palette ----

okabe_ito <- c(
  "#000000", "#E69F00", "#56B4E9",
  "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7"
)

# single accent for the d-prime points / summary
dprime_col <- "#0072B2"

# relational-model colours (questions figure)
model_cols <- c(EM = "#0072B2", AR = "#D55E00", CS = "#009E73")
pretty_model <- c(EM = "Equality matching", AR = "Authority ranking",
                  CS = "Communal sharing")

# age colours (one Okabe-Ito colour per completed year)
age_cols <- c("5" = "#E69F00", "6" = "#56B4E9", "7" = "#009E73",
              "8" = "#D55E00")


## Shared helper functions ----

# log-linear (Hautus, 1995) signal-detection scores from raw counts.
# Applied to ALL participants (not only those with 0/1 rates) so the
# correction is uniform. Returns a data.frame the same length as the inputs.
sdt_loglinear <- function(hits, n_signal, fas, n_noise) {
  H <- (hits + 0.5) / (n_signal + 1)
  F <- (fas  + 0.5) / (n_noise  + 1)
  data.frame(
    hit_rate_adj = H,
    fa_rate_adj  = F,
    dprime       = qnorm(H) - qnorm(F),
    criterion_c  = -0.5 * (qnorm(H) + qnorm(F))
  )
}

# significance stars from a two-sided one-sample t-test p-value
p_star <- function(p) dplyr::case_when(p < .001 ~ "***", p < .01 ~ "**",
                                       p < .05  ~ "*",   TRUE    ~ "")

# shared ggplot theme
theme_study <- function() {
  theme_minimal(base_size = 18, base_family = "Avenir Next") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing      = unit(1.6, "lines"),
      axis.line          = element_line(colour = "black", linewidth = 0.6),
      axis.ticks         = element_line(colour = "black"),
      axis.text          = element_text(colour = "black", face = "bold"),
      legend.position    = "bottom",
      plot.title         = element_text(size = 18, face = "bold"),
      strip.text         = element_text(size = 14, face = "bold")
    )
}


# DATA ----

## Load raw source ----
# Preferred input: the CHS-merged export with birthdates (age_in_days computed
# from the consent framedata zip). Falls back to the plain Sheet export
# (age in completed years only).

data_file <- if (file.exists(file.path(raw_dir, "Analysis",
                                       "GameRAD_data_with_age.csv"))) {
  file.path(raw_dir, "Analysis", "GameRAD_data_with_age.csv")
} else if (file.exists(file.path(raw_dir, "GameRAD Data - Sheet1 (4).csv"))) {
  file.path(raw_dir, "GameRAD Data - Sheet1 (4).csv")
} else {
  stop("Data file not found; put the GameRAD Data CSV in ", raw_dir)
}
raw <- readr::read_csv(data_file, show_col_types = FALSE)
message("Loaded ", nrow(raw), " rows from ", basename(data_file))

# Ensure CHS-derived columns exist even if we loaded the plain Sheet export
if (!"age_in_days"     %in% names(raw)) raw$age_in_days     <- NA_real_
if (!"child_hashed_id" %in% names(raw)) raw$child_hashed_id <- NA_character_


## Clean and code ----

TEST_NAMES <- c("test", "teset", "lookit reviewer", "lr", "min")

dat <- raw %>%
  dplyr::filter(Type == "test") %>%
  dplyr::mutate(
    ID          = ParticipantID,
    age_field   = suppressWarnings(as.numeric(Age)),
    age_days    = suppressWarnings(as.numeric(age_in_days)),
    is_signal   = as.logical(IsCongruent),
    said_yes    = InitialYesNo == "YES",
    ts          = suppressWarnings(as.POSIXct(Timestamp,
                                              format = "%Y-%m-%dT%H:%M:%OS",
                                              tz = "UTC")),
    is_test_run = stringr::str_to_lower(stringr::str_trim(ChildName)) %in%
                    TEST_NAMES
  )


## Pre-registered inclusion cascade ----

sessions <- dat %>%
  # step 2 (part): developer/test sessions out
  dplyr::filter(!is.na(ID), !is_test_run) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    child_hashed_id = dplyr::first(child_hashed_id),
    ChildName  = dplyr::first(ChildName),
    age_field  = dplyr::first(age_field[!is.na(age_field)]),
    age_days   = dplyr::first(age_days[!is.na(age_days)]),
    n_vignettes = dplyr::n_distinct(VignetteID),
    n_trials   = dplyr::n(),
    ts_start   = min(ts, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  # step 1: >= 6 of 12 vignettes
  dplyr::filter(n_vignettes >= 6) %>%
  # step 2: one session per child (CHS hashed id; PID fallback when unmatched)
  dplyr::mutate(child_key = dplyr::coalesce(child_hashed_id,
                                            paste0("PID:", ID))) %>%
  dplyr::arrange(child_key, dplyr::desc(n_trials), ts_start) %>%
  dplyr::distinct(child_key, .keep_all = TRUE) %>%
  # step 3: age window 4y9m - 9y0m; continuous age in years
  dplyr::mutate(
    age_years = dplyr::coalesce(age_days / 365.25, age_field + 0.5),
    age_bin   = floor(age_years)
  ) %>%
  dplyr::filter(age_years >= 4.75, age_years < 9)


## Per-child SDT scores + YES-bias filter (step 4) ----

counts <- dat %>%
  dplyr::filter(ID %in% sessions$ID) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n_signal = sum(is_signal),
    n_noise  = sum(!is_signal),
    hits     = sum(is_signal  & said_yes),
    fas      = sum(!is_signal & said_yes),
    yes_rate = mean(said_yes),
    .groups  = "drop"
  )

dprime_by_child <- sessions %>%
  dplyr::left_join(counts, by = "ID") %>%
  dplyr::bind_cols(sdt_loglinear(.$hits, .$n_signal, .$fas, .$n_noise)) %>%
  dplyr::mutate(yes_bias_excl = yes_rate %in% c(0, 1)) %>%
  dplyr::arrange(age_years)

readr::write_csv(dprime_by_child,
                 file.path(results_dir, "dprime_by_child.csv"))

# Y/N analytic sample (yes-bias filtered); circles sample keeps everyone
gamerad_reg <- dplyr::filter(dprime_by_child, !yes_bias_excl)

message("Circles-eligible sample: ", nrow(dprime_by_child), " children ; ",
        "Y/N sample after YES-bias filter: ", nrow(gamerad_reg),
        " (", sum(dprime_by_child$yes_bias_excl), " excluded)")
message("Y/N sample: mean d' = ", round(mean(gamerad_reg$dprime), 3),
        " ; mean criterion c = ", round(mean(gamerad_reg$criterion_c), 3))


## Summary by age bin (+ t-test vs chance, d' = 0) ----

age_summary <- gamerad_reg %>%
  dplyr::group_by(age_bin) %>%
  dplyr::summarise(
    n       = dplyr::n(),
    mean_d  = mean(dprime),
    sd_d    = sd(dprime),
    min_d   = min(dprime),
    max_d   = max(dprime),
    p_vs_0  = if (dplyr::n() > 1) t.test(dprime, mu = 0)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ci_half = ifelse(n > 1, qt(0.975, n - 1) * sd_d / sqrt(n), 0),
    star    = ifelse(is.na(p_vs_0), "", p_star(p_vs_0))
  )

readr::write_csv(age_summary,
                 file.path(results_dir, "dprime_summary_by_age.csv"))
print(as.data.frame(age_summary), row.names = FALSE)


# AGE-IN-DAYS ANALYSIS ----
# Continuous age from CHS birthdates (age_in_days / 365.25). Children whose
# exit survey lacked a birthdate fall back to caregiver year + 0.5 in the
# cascade above but are EXCLUDED here, so the slope uses exact ages only.

age_cont <- gamerad_reg %>% dplyr::filter(!is.na(age_days))
message("Continuous-age sample (exact birthdates): ", nrow(age_cont),
        " of ", nrow(gamerad_reg), " Y/N children")

age_days_lm <- lm(dprime ~ I(age_days / 365.25), data = age_cont)
age_days_tbl <- as.data.frame(summary(age_days_lm)$coefficients)
age_days_tbl <- cbind(term = c("Intercept", "slope (d' per year)"),
                      age_days_tbl)
readr::write_csv(age_days_tbl, file.path(results_dir, "dprime_age_days_lm.csv"))
print(age_days_tbl, row.names = FALSE)

# criterion c over age (H5 context: is the yes-bias stable across age?)
crit_lm <- lm(criterion_c ~ I(age_days / 365.25), data = age_cont)
message("criterion c ~ age slope: ",
        round(coef(crit_lm)[2], 3), " per year (p = ",
        signif(summary(crit_lm)$coefficients[2, 4], 3), ")")


## Figure: d' by continuous age (days) ----

p_age_days <- ggplot(age_cont, aes(age_years, dprime)) +
  geom_hline(aes(yintercept = 0, linetype = "Chance (d' = 0)"),
             colour = "black", linewidth = 0.6) +
  geom_point(alpha = 0.5, size = 2.4, colour = dprime_col) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black",
              fill = dprime_col, alpha = 0.15, linewidth = 1) +
  scale_linetype_manual(name = NULL, values = c("Chance (d' = 0)" = "dashed")) +
  scale_x_continuous(breaks = 5:9) +
  coord_cartesian(clip = "off") +
  labs(x = "Age (years, from age in days)",
       y = "d' (congruent vs incongruent)") +
  theme_study()

ggsave(file.path(fig_dir, "gamerad_dprime_age_days.png"), p_age_days,
       width = 8.5, height = 5.5, dpi = 300, bg = "white")


## Figure: d' by age bin ----

p_age <- ggplot(gamerad_reg, aes(age_bin, dprime)) +
  geom_hline(aes(yintercept = 0, linetype = "Chance (d' = 0)"),
             colour = "black", linewidth = 0.6) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.45, size = 2.2,
              colour = dprime_col) +
  geom_pointrange(data = age_summary,
                  aes(age_bin, mean_d, ymin = mean_d - ci_half,
                      ymax = mean_d + ci_half),
                  inherit.aes = FALSE, colour = "black", linewidth = 1,
                  size = 0.7, fatten = 3) +
  geom_text(data = age_summary,
            aes(age_bin, mean_d + ci_half + 0.25, label = star),
            inherit.aes = FALSE, vjust = 0, size = 6, fontface = "bold") +
  scale_linetype_manual(name = NULL, values = c("Chance (d' = 0)" = "dashed")) +
  scale_x_continuous(breaks = sort(unique(gamerad_reg$age_bin))) +
  coord_cartesian(clip = "off") +
  labs(x = "Age (completed years)", y = "d' (congruent vs incongruent)") +
  theme_study()

ggsave(file.path(fig_dir, "gamerad_dprime_age.png"), p_age,
       width = 8.5, height = 5.5, dpi = 300, bg = "white")


## Figure: hit rate vs false-alarm rate ----

p_rates <- ggplot(gamerad_reg,
                  aes(fa_rate_adj, hit_rate_adj,
                      colour = factor(age_bin))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "black", linewidth = 0.6) +
  geom_point(alpha = 0.75, size = 2.6) +
  scale_colour_manual(values = age_cols, name = "Age") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_equal() +
  labs(x = "False-alarm rate (adj.)", y = "Hit rate (adj.)") +
  theme_study()

ggsave(file.path(fig_dir, "gamerad_hit_fa.png"), p_rates,
       width = 6.5, height = 6.5, dpi = 300, bg = "white")


## Combined main figure: A = continuous age, B = hit vs FA ----

combined <- cowplot::plot_grid(p_age_days, p_rates, ncol = 2,
                               labels = c("A", "B"),
                               rel_widths = c(1.2, 1))
ggsave(file.path(fig_dir, "gamerad_combined.png"), combined,
       width = 13, height = 5.8, dpi = 300, bg = "white")


# QUESTION-MODEL d' ----
# Separate d' per child for EM / AR / CS QUESTIONS. For question model M:
# signal = questions of model M asked in an M vignette (congruent, keyed YES);
# noise  = questions of model M asked in another model's vignette (keyed NO).
# Full design: 4 signal + 8 noise trials per model per child.
# Analytic sample = Y/N sample (yes-bias filtered).

qmodel_dprime <- dat %>%
  dplyr::filter(ID %in% gamerad_reg$ID) %>%
  dplyr::group_by(ID, QuestionModel) %>%
  dplyr::summarise(
    n_signal = sum(is_signal),
    n_noise  = sum(!is_signal),
    hits     = sum(is_signal  & said_yes),
    fas      = sum(!is_signal & said_yes),
    .groups  = "drop"
  ) %>%
  dplyr::left_join(dplyr::select(gamerad_reg, ID, age_years, age_bin),
                   by = "ID")
qmodel_dprime <- dplyr::bind_cols(
  qmodel_dprime,
  sdt_loglinear(qmodel_dprime$hits, qmodel_dprime$n_signal,
                qmodel_dprime$fas,  qmodel_dprime$n_noise))

readr::write_csv(qmodel_dprime,
                 file.path(results_dir, "dprime_by_question_model.csv"))

## Per-model summary + t-test vs chance (d' = 0) ----

qmodel_summary <- qmodel_dprime %>%
  dplyr::group_by(QuestionModel) %>%
  dplyr::summarise(
    n      = dplyr::n(),
    mean_d = mean(dprime),
    sd_d   = sd(dprime),
    p_vs_0 = t.test(dprime, mu = 0)$p.value,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ci_half = qt(0.975, n - 1) * sd_d / sqrt(n),
    star    = p_star(p_vs_0)
  )

readr::write_csv(qmodel_summary,
                 file.path(results_dir, "dprime_summary_by_question_model.csv"))
print(as.data.frame(qmodel_summary), row.names = FALSE)

## Figure: d' by question model ----

qm_plot_dat <- qmodel_dprime %>%
  dplyr::mutate(QuestionModel = factor(QuestionModel,
                                       levels = c("EM", "AR", "CS")))
qm_summ_dat <- qmodel_summary %>%
  dplyr::mutate(QuestionModel = factor(QuestionModel,
                                       levels = c("EM", "AR", "CS")))

p_qmodel <- ggplot(qm_plot_dat, aes(QuestionModel, dprime,
                                    colour = QuestionModel)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black",
             linewidth = 0.6) +
  geom_jitter(width = 0.14, height = 0, alpha = 0.4, size = 2) +
  geom_pointrange(data = qm_summ_dat,
                  aes(QuestionModel, mean_d, ymin = mean_d - ci_half,
                      ymax = mean_d + ci_half),
                  inherit.aes = FALSE, colour = "black", linewidth = 1,
                  size = 0.7, fatten = 3) +
  geom_text(data = qm_summ_dat,
            aes(QuestionModel, mean_d + ci_half + 0.2, label = star),
            inherit.aes = FALSE, vjust = 0, size = 6, fontface = "bold") +
  scale_colour_manual(values = model_cols, guide = "none") +
  scale_x_discrete(labels = pretty_model) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "d' (congruent vs incongruent)") +
  theme_study()

ggsave(file.path(fig_dir, "gamerad_dprime_qmodel.png"), p_qmodel,
       width = 8.5, height = 5.5, dpi = 300, bg = "white")


# CIRCLES (MODEL-ID) MEASURE ----
# After each vignette, children pick which of three circle diagrams (EM / AR /
# CS pictures) "shows how X and Y act". Chance = 1/3. Per the pre-registration,
# the YES-bias filter does NOT apply here, so the circles sample is the full
# post-cascade sample.
#
# NOTE on the export: in newer rows the TRUE/FALSE correctness flag sits in
# the ModelIDCertainty column and DEFINITELY/MAYBE sits in ModelIDCorrect
# (columns shifted relative to the header). Correctness is therefore computed
# directly as ModelIDChosen == VignetteModel, which matches the recorded flag
# in 100% of rows under both formats.

circles <- raw %>%
  dplyr::filter(Type == "model-id", ParticipantID %in% dprime_by_child$ID) %>%
  dplyr::mutate(ID = ParticipantID,
                correct = ModelIDChosen == VignetteModel) %>%
  # one choice per vignette: drop replays/duplicates
  dplyr::distinct(ID, VignetteID, .keep_all = TRUE)

circles_by_child <- circles %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n_trials  = dplyr::n(),
    k_correct = sum(correct),
    prop      = mean(correct),
    .groups   = "drop"
  ) %>%
  dplyr::left_join(dplyr::select(dprime_by_child, ID, ChildName,
                                 age_years, age_bin, age_days),
                   by = "ID") %>%
  dplyr::filter(n_trials >= 6)

readr::write_csv(circles_by_child,
                 file.path(results_dir, "circles_by_child.csv"))

circles_t <- t.test(circles_by_child$prop, mu = 1/3)
message("Circles: ", nrow(circles_by_child), " children ; mean prop correct = ",
        round(mean(circles_by_child$prop), 3), " vs chance .333 ; t(",
        circles_t$parameter, ") = ", round(circles_t$statistic, 2),
        ", p = ", signif(circles_t$p.value, 3))

## Summary by age bin ----

circles_age_summary <- circles_by_child %>%
  dplyr::group_by(age_bin) %>%
  dplyr::summarise(
    n      = dplyr::n(),
    mean_p = mean(prop),
    sd_p   = sd(prop),
    p_vs_chance = if (dplyr::n() > 1) t.test(prop, mu = 1/3)$p.value
                  else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ci_half = ifelse(n > 1, qt(0.975, n - 1) * sd_p / sqrt(n), 0),
    star    = ifelse(is.na(p_vs_chance), "", p_star(p_vs_chance))
  )

readr::write_csv(circles_age_summary,
                 file.path(results_dir, "circles_summary_by_age.csv"))
print(as.data.frame(circles_age_summary), row.names = FALSE)

## Figure: circles accuracy by age ----

p_circles <- ggplot(circles_by_child, aes(age_bin, prop)) +
  geom_hline(aes(yintercept = 1/3, linetype = "Chance (1/3)"),
             colour = "black", linewidth = 0.6) +
  geom_jitter(width = 0.12, height = 0.01, alpha = 0.45, size = 2.2,
              colour = "#CC79A7") +
  geom_pointrange(data = circles_age_summary,
                  aes(age_bin, mean_p, ymin = mean_p - ci_half,
                      ymax = mean_p + ci_half),
                  inherit.aes = FALSE, colour = "black", linewidth = 1,
                  size = 0.7, fatten = 3) +
  geom_text(data = circles_age_summary,
            aes(age_bin, mean_p + ci_half + 0.05, label = star),
            inherit.aes = FALSE, vjust = 0, size = 6, fontface = "bold") +
  scale_linetype_manual(name = NULL, values = c("Chance (1/3)" = "dashed")) +
  scale_x_continuous(breaks = sort(unique(circles_by_child$age_bin))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Age (completed years)", y = "Circles (model-ID) accuracy") +
  theme_study()

ggsave(file.path(fig_dir, "gamerad_circles_age.png"), p_circles,
       width = 8.5, height = 5.5, dpi = 300, bg = "white")


## Combined measures figure: A = question-model d', B = circles by age ----

combined_measures <- cowplot::plot_grid(p_qmodel, p_circles, ncol = 2,
                                        labels = c("A", "B"))
ggsave(file.path(fig_dir, "gamerad_combined_measures.png"), combined_measures,
       width = 13, height = 5.8, dpi = 300, bg = "white")


# EXPLORATORY: YES-RATE PROFILES ----
# NOT pre-registered. Question: do children endorse some relational behaviours
# (equal division, communal sharing) as likely in ANY close relationship,
# while treating others (authority) as conditional on the relationship?
# All analyses below use the Y/N sample (gamerad_reg) and the child's raw
# YES/NO responses, split by QUESTION model x VIGNETTE model / congruency.

## Item classification (resource distribution vs other) ----
# Hand-coded from the item texts; saved to tables/item_classification.csv.
# resource = the item concerns allocating/consuming/exchanging things;
# borderline = EM1 (equal time using an object), AR8 (biggest chair).

resource_items <- c("EM1", "EM2", "EM3", "EM4", "EM6", "EM7",
                    "CS1", "CS3", "CS5", "AR7", "AR8")

yn_trials <- dat %>%
  dplyr::filter(ID %in% gamerad_reg$ID) %>%
  dplyr::left_join(dplyr::select(gamerad_reg, ID, age_years, age_bin),
                   by = "ID") %>%
  dplyr::mutate(resource = ItemID %in% resource_items)

## Question-model x vignette-model yes-rate matrix ----

cell_yes <- yn_trials %>%
  dplyr::group_by(ID, QuestionModel, VignetteModel) %>%
  dplyr::summarise(yes = mean(said_yes), .groups = "drop")
readr::write_csv(cell_yes,
                 file.path(results_dir, "yesrate_by_qmodel_x_vignette.csv"))

message("Mean P(YES), question model x vignette model:")
cell_yes %>%
  dplyr::group_by(QuestionModel, VignetteModel) %>%
  dplyr::summarise(yes = mean(yes), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = VignetteModel, values_from = yes) %>%
  as.data.frame() %>% print(row.names = FALSE)

## False-alarm (incongruent) rates by question model ----
# High FA = the behaviour is endorsed regardless of relational model.

fa_child <- yn_trials %>%
  dplyr::filter(!is_signal) %>%
  dplyr::group_by(ID, QuestionModel) %>%
  dplyr::summarise(fa = mean(said_yes), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = QuestionModel, values_from = fa) %>%
  dplyr::left_join(dplyr::select(gamerad_reg, ID, age_years, age_bin),
                   by = "ID")

for (qm in c("EM", "CS", "AR")) {
  tt <- t.test(fa_child[[qm]], mu = 0.5)
  message(qm, " incongruent yes-rate = ", round(mean(fa_child[[qm]]), 3),
          " vs 50%: t(", tt$parameter, ") = ", round(tt$statistic, 2),
          ", p = ", signif(tt$p.value, 3))
}
message("Paired EM - AR: p = ",
        signif(t.test(fa_child$EM, fa_child$AR, paired = TRUE)$p.value, 3),
        " ; EM - CS: p = ",
        signif(t.test(fa_child$EM, fa_child$CS, paired = TRUE)$p.value, 3),
        " ; CS - AR: p = ",
        signif(t.test(fa_child$CS, fa_child$AR, paired = TRUE)$p.value, 3))

## Age effects on false alarms ----

for (qm in c("EM", "CS", "AR")) {
  m <- lm(stats::reformulate("age_years", qm), data = fa_child)
  message(qm, " FA age slope: ", round(coef(m)[2], 3), "/yr, p = ",
          signif(summary(m)$coefficients[2, 4], 3))
}
m_gap <- lm(I(EM - AR) ~ age_years, data = fa_child)
message("EM-AR selectivity gap x age: ", round(coef(m_gap)[2], 3),
        "/yr, p = ", signif(summary(m_gap)$coefficients[2, 4], 3))

## Resource vs non-resource items ----

fa_res <- yn_trials %>%
  dplyr::filter(!is_signal) %>%
  dplyr::group_by(ID, QuestionModel, resource) %>%
  dplyr::summarise(fa = mean(said_yes), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = resource, values_from = fa,
                     names_prefix = "res_")
for (qm in c("EM", "CS", "AR")) {
  g <- dplyr::filter(fa_res, QuestionModel == qm)
  tt <- t.test(g$res_TRUE, g$res_FALSE, paired = TRUE)
  message(qm, " FA resource = ", round(mean(g$res_TRUE, na.rm = TRUE), 3),
          " vs non-resource = ", round(mean(g$res_FALSE, na.rm = TRUE), 3),
          " ; paired p = ", signif(tt$p.value, 3))
}
readr::write_csv(fa_res, file.path(results_dir, "fa_by_qmodel_x_resource.csv"))

## Figure: yes-rate matrix ----
# Lines = question model, x = vignette model; the congruent cell of each
# line is ringed. Chance for a binary yes/no shown at 50%.

mat_dat <- cell_yes %>%
  dplyr::group_by(QuestionModel, VignetteModel) %>%
  dplyr::summarise(m = mean(yes), se = sd(yes) / sqrt(dplyr::n()),
                   .groups = "drop") %>%
  dplyr::mutate(
    VignetteModel = factor(VignetteModel, levels = c("EM", "CS", "AR")),
    congruent     = QuestionModel == as.character(VignetteModel)
  )

p_matrix <- ggplot(mat_dat,
                   aes(VignetteModel, m, colour = QuestionModel,
                       group = QuestionModel)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "black",
             linewidth = 0.6) +
  geom_line(linewidth = 1, position = position_dodge(0.25)) +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se),
                  size = 0.7, linewidth = 1,
                  position = position_dodge(0.25)) +
  geom_point(data = dplyr::filter(mat_dat, congruent), shape = 21,
             size = 6, stroke = 1.4, colour = "black", fill = NA,
             position = position_dodge(0.25)) +
  scale_colour_manual(values = model_cols, labels = pretty_model,
                      name = "Question model") +
  scale_x_discrete(labels = pretty_model) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "True vignette model", y = "P(YES)") +
  theme_study()
ggsave(file.path(fig_dir, "gamerad_yesrate_matrix.png"), p_matrix,
       width = 8.5, height = 5.8, dpi = 300, bg = "white")

## Figure: yes-rates by age (FAs and hits) ----

rate_age <- yn_trials %>%
  dplyr::group_by(ID, age_bin, QuestionModel, is_signal) %>%
  dplyr::summarise(yes = mean(said_yes), .groups = "drop") %>%
  dplyr::group_by(age_bin, QuestionModel, is_signal) %>%
  dplyr::summarise(m = mean(yes), se = sd(yes) / sqrt(dplyr::n()),
                   .groups = "drop") %>%
  dplyr::mutate(panel = ifelse(is_signal, "Congruent trials (hits)",
                               "Incongruent trials (false alarms)"))

p_rate_age <- ggplot(rate_age,
                     aes(age_bin, m, colour = QuestionModel)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "black",
             linewidth = 0.6) +
  geom_line(linewidth = 1, position = position_dodge(0.15)) +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se),
                  size = 0.7, linewidth = 1,
                  position = position_dodge(0.15)) +
  facet_wrap(~ panel) +
  scale_colour_manual(values = model_cols, labels = pretty_model,
                      name = "Question model") +
  scale_x_continuous(breaks = sort(unique(rate_age$age_bin))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age (completed years)", y = "P(YES)") +
  theme_study()
ggsave(file.path(fig_dir, "gamerad_yesrate_age.png"), p_rate_age,
       width = 13, height = 5.8, dpi = 300, bg = "white")


message("GAMERAD_CLEAN.R complete -- tables in ", results_dir,
        " ; figures in ", fig_dir)
