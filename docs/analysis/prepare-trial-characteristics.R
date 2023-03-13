# Prepare cross-registrations ---------------------------------------------

cross_registrations <-
  registrations |>
  select(id, trn, registry) |>

  # Get all trns for included trials
  semi_join(trials, by = "id")

tbl_crossreg <-
  cross_registrations |>

  # Limit to distinct registries per trial
  # NOTE: some trials, e.g. tri00985, have >1 registration but in single registry, so count for registry and not cross-registration
  distinct(id, registry) |>
  arrange(registry) |>

  # Collapse registries per trial
  group_by(id) |>
  summarise(cross_registry = stringr::str_c(registry, collapse = ", ")) |>
  # Recode cross-registrations
  mutate(cross_registry = if_else(stringr::str_detect(cross_registry, ", "), "Cross-registered", cross_registry))

tbl_prep <-
  ictrp |>

  select(trn, phase, countries, target_enrollment) |>

  # Get all ictrp for all cross-registrations
  semi_join(cross_registrations, by = "trn") |>

  # Add id
  left_join(cross_registrations, by = "trn")


# Prepare countries -------------------------------------------------------

tbl_countries <-

  tbl_prep |>

  select(id, countries) |>

  # Remove duplicate countries
  tidyr::separate_rows(countries, sep = ", ") |>
  distinct() |>

  # Collapse countries per trial
  group_by(id) |>
  summarise(countries = stringr::str_c(countries, collapse = ", ")) |>

  # TODO: could check for "No country given" and remove
  # Collapse multinational (i.e., with comma)
  mutate(
    countries =
      if_else(stringr::str_detect(countries, ","), "Multinational", countries)
  )


# Prepare phases ----------------------------------------------------------

tbl_phases <-
  tbl_prep |>
  select(id, phase) |>

  # tri03199 has issue with phase --> recode as "Not Applicable"
  # TODO: check with ND
  mutate(phase = if_else(stringr::str_detect(phase, "^Human pharmacology"), "Not Applicable", phase)) |>

  # Tidy phases
  mutate(
    phase = stringr::str_remove_all(phase, "Phase "),
    phase = stringr::str_replace(phase, "[-/]", ","),
    phase = na_if(phase, "Not Applicable")
  ) |>
  tidyr::separate_rows(phase, sep = ",") |>
  distinct() |>
  arrange(phase) |>

  # Collapse phases per trial
  group_by(id) |>
  summarise(phase = stringr::str_c(phase, collapse = ",")) |>
  mutate(phase = tidyr::replace_na(phase, "Not Applicable"))


# Prepare enrollment ------------------------------------------------------

# Note: 8 trials with 0 enrollment: tri09969, tri09971, tri09972, tri09973, tri02771, tri04436, tri04441, tri04442
# Note: 6 trials with NA enrollment: tri00917, tri02448, tri03029, tri03309, tri06336, tri06856

tbl_enrollment <-
  tbl_prep |>
  distinct(id, target_enrollment) |>

  # Tidy enrollment
  mutate(
    target_enrollment = na_if(target_enrollment, "Not Available"),
    target_enrollment = as.numeric(target_enrollment)
  )|>

  # Get average enrollment across registrations
  # TODO: discuss with ND, could also us max, min, etc.
  group_by(id) |>
  summarise(target_enrollment = mean(target_enrollment)) |>
  mutate(target_enrollment = round(target_enrollment))


# Lump infrequent (<5 trials) countries
# countries =
#   fct_lump_min(countries, 5, other_level = "Countries with <5 trials")
# Lump infrequent (<3 trials) registries
# cross_registry =
#   fct_lump_min(cross_registry, 3, other_level = "Registries with <3 trials")

library(forcats)
library(gtsummary)

tbl_trials <-
  trials |>
  select(id) |>

  mutate(
    has_result = if_else(id %in% unique(results$id), "With Results", "Without Results")
  ) |>
  left_join(tbl_enrollment, by = "id") |>
  left_join(tbl_phases, by = "id") |>
  left_join(tbl_crossreg, by = "id") |>
  left_join(tbl_countries, by = "id") |>
  mutate(

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

  select(-id) |>

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

gt_tbl_trials <- as_gt(tbl_trials)
gt::gtsave(gt_tbl_trials, here::here("docs", "figures", "tbl-trials.png"))
writeLines(gt::as_rtf(gt_tbl_trials), here("docs", "figures", "tbl-trials.rtf"))
