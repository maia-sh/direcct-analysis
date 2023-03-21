# TODO: Add interventions
# could include prospective reg, study type

# Create table of trial characteristics for included trials using priority ictrp registration

# Prepare cross-registrations ---------------------------------------------

cross_registrations <-
  registrations |>

  # Get all trns for included trials
  semi_join(trials, by = "id") |>


  # Limit to distinct registries per trial
  # NOTE: some trials, e.g. tri00985, have >1 registration but in single registry, so count for registry and not cross-registration
  distinct(id, registry) |>
  arrange(registry)

tbl_crossreg <-
  cross_registrations |>

  # Collapse registries per trial
  group_by(id) |>
  summarise(cross_registry = stringr::str_c(registry, collapse = ","))

# Prepare ICTRP -----------------------------------------------------------

# Get ictrp data for priority trns
trial_characteristics <-
  ictrp |>

  semi_join(trials, by = c("trn" = "trn_ictrp_priority")) |>
  left_join(select(trials, id, trn_ictrp_priority), by = c("trn" = "trn_ictrp_priority")) |>
  relocate(id, .before = 1) |>

  # Some EUCTR trials (n = 3) duplicated in ictrp, so select earliest registered (since EUCTR no last updated date)
  group_by(trn) |>
  arrange(date_registration, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>

  mutate(

    # Tidy phases
    phase = if_else(phase == "Phase 1-2", "Phase 1/Phase 2", phase),

    # Tidy enrollment
    target_enrollment = na_if(target_enrollment, "Not Available"),
    target_enrollment = as.numeric(target_enrollment),

    # Add results flag
    has_result = if_else(id %in% unique(results$id), "With Results", "Without Results")
  ) |>

  # Add registries
 left_join(tbl_crossreg, by = "id")


# Prepare table 1 ---------------------------------------------------------

library(forcats)
library(gtsummary)

tbl_trials <-
  trial_characteristics |>

  select(has_result, cross_registry, countries, target_enrollment, phase
         #,study_type,recruitment_status, retrospective_registration, date_registration #or date_completion used in main analysis? or pandemic "semester"?
  ) |>


  # Prepare characteristics for summary table
  mutate(

    # Recode cross-registrations
    cross_registry = if_else(stringr::str_detect(cross_registry, ","), "Cross-registered", cross_registry),

    # Recode multinational
    countries =
      if_else(stringr::str_detect(countries, ","), "Multinational", countries),

    # Lump infrequent registries
    # cross_registry =
    #   fct_lump_min(cross_registry, 3, other_level = "Registries with <3 trials"),

    # Order by frequency, with cross-registrations first, and lumped last
    cross_registry =
      fct_infreq(cross_registry),
    cross_registry =
      fct_relevel(cross_registry, "Cross-registered", after = 0),
    # cross_registry =
    #   fct_relevel(cross_registry, "Registries with <3 trials", after = Inf),

    # Lump infrequent countries
    countries =
      fct_lump_min(countries, 15, other_level = "Countries with <15 trials"),

    # Order by frequency, with multinational, lumped, and no country at end
    countries = fct_infreq(countries),
    countries = fct_relevel(countries, "Multinational", after = Inf),
    countries = fct_relevel(countries, "Countries with <15 trials", after = Inf),
    countries = fct_relevel(countries, "No Country Given", after = Inf),
  ) |>


  gtsummary::tbl_summary(
    by = has_result,
    # type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{N_nonmiss}",
    #                                  "{median} ({p25}, {p75})",
    #                                  "{min}, {max}"),
    missing = "no",
    label = list(
      target_enrollment ~ "Target enrollment",
      # study_type ~ "Study type",
      phase ~ "Phase",
      cross_registry ~ "Registry",
      countries ~ "Countries"
    ),
    # sort = list(
    # cross_registry ~ "frequency",
    # countries ~ "frequency"
    # ),
  ) %>%

  add_overall() %>%

  # add_difference(include = target_enrollment) %>%

  # modify_header(label = "**Publication Type**") %>%

  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Results Disseminated**") %>%
  modify_caption("**Trial Characteristics** (N = {N})")

# gt_tbl_trials <- as_gt(tbl_trials)
# gt::gtsave(gt_tbl_trials, here::here("docs", "figures", "tbl-trials.png"))
# writeLines(gt::as_rtf(gt_tbl_trials), here::here("docs", "figures", "tbl-trials.rtf"))

