# TODO: add randomized and perhaps more from nick/james project (e.g., world bank region)
# could include prospective reg, study type

# Create table of trial characteristics for included trials using priority ictrp registration

# Prepare cross-registrations ---------------------------------------------

trials_all_registries <-
  registrations |>

  # Get all trns for included trials
  semi_join(trials, by = "id") |>


  # Limit to distinct registries per trial
  # NOTE: some trials, e.g. tri00985, have >1 registration but in single registry, so count for registry and not cross-registration
  distinct(id, registry) |>
  arrange(registry)

# Create boolean for cross-registrations
cross_registrations <-
  trials_all_registries |>
  group_by(id) |>
  summarize(crossreg = if_else(n() > 1, TRUE, FALSE))

# Prepare pandemic "semester" ---------------------------------------------
pandemic_semester <-
  trials |>
  mutate(
    month_completion =
      lubridate::floor_date(date_completion, "month"),

    semester = case_when(
      month_completion < as.Date("2020-07-01") ~ 1,
      month_completion < as.Date("2021-01-01") ~ 2,
      month_completion < as.Date("2021-07-01") ~ 3,
    ),

    semester_fct = ordered(semester,
                           labels = c("Jan 2020-Jun 2020", "July 2020-Dec 2020", "Jan 2021-Jun 2021")
    )
  ) |>
  select(id, ends_with("completion"), starts_with("semester"))

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
    has_result = if_else(id %in% unique(results$id), "With Results", "Without Results"),

    # Add multinational flag
    multinational = if_else(stringr::str_detect(countries, ","), TRUE, FALSE)
  ) |>

  # Add registries
  left_join(cross_registrations, by = "id") |>

  # Add pandemic semester
  left_join(pandemic_semester, by = "id")


# Prepare top interventions -----------------------------------------------

# Get trials with row per unique intervention
# NOTE: There are trials in multiple rows but not same intervention in multiple rows (even if used in multiple arms)
trials_interventions <-

  # Get arms of included trials
  arms |>
  semi_join(trials, by = "id") |>

  # Limit to experimental
  filter(type == "experimental") |>

  # Get row per intervention (i.e., trials may be in multiple rows)
  tidyr::separate_rows(intervention, sep = ";") |>

  # Limit to 1 row per intervention in a trial (since some interventions in more that one arm)
  distinct(id, intervention, .keep_all = TRUE)

# How many trials per intervention?
intervention_counts <-
  trials_interventions |>
  count(intervention, name = "n_trials") |>
  arrange(desc(n_trials))

# How many trials investigating how many interventions?
trials_intervention_counts <-
  intervention_counts |>
  count(n_trials, name = "n_intervention")

N_TOP_INTERVENTIONS <- 5

# Top interventions
# NOTE: "traditional medicine" collapses many interventions and hence not a true intervention
top_interventions <-
  intervention_counts |>
  filter(intervention != "Traditional Medicine") |>
  slice_head(n = N_TOP_INTERVENTIONS)

# Get trials with common interventions (with 1 row per common intervention)
trials_common_interventions <-

  trials_interventions |>

  # Limit to common interventions
  filter(intervention %in% top_interventions$intervention)

# Prepare interventions characteristics table
tbl_interventions_top <-
  trials_common_interventions |>

  # Add results flag
  mutate(
    has_result = if_else(id %in% unique(results$id), "With Results", "Without Results")
  ) |>

  select(has_result, intervention) |>

  gtsummary::tbl_summary(
    by = has_result,
    label = list(intervention ~ glue::glue("Top {N_TOP_INTERVENTIONS} Interventions")),
    sort = all_categorical() ~ "frequency"
  ) |>
  add_overall()|>
  bold_labels()

# Prepare table 1 ---------------------------------------------------------

# library(forcats)
# library(gtsummary)

tbl_trials <-
  trial_characteristics |>

  select(has_result,target_enrollment, crossreg, multinational, phase, semester_fct
         #,study_type,recruitment_status, retrospective_registration, date_registration #or date_completion used in main analysis? or pandemic "semester"?
  ) |>

  # Recode booleans to categorical
  # NOTE: Could instead have single line for "cross-registered" and "multinational"
  mutate(
    crossreg = if_else(crossreg, "Multi", "Single"),
    multinational = if_else(multinational, "Multi", "Single")
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
      crossreg ~ "Registries",
      multinational ~ "Countries",
      phase ~ "Trial Phase",
      semester_fct ~ "Pandemic phase"
    ),
    # value = list(crossreg ~ c("TRUE", "FALSE")),
    # sort = list(
    # cross_registry ~ "frequency",
    # countries ~ "frequency"
    # ),
  )|>

  add_overall()|>

  # add_difference(include = target_enrollment)|>

  # modify_header(label = "**Publication Type**")|>

  bold_labels() #|>
  #modify_spanning_header(all_stat_cols() ~ "**Results Disseminated**") |>
  # modify_caption("**Trial Characteristics** (N = {N})")



# Combine characteristics table -------------------------------------------

tbl_characteristics <-
  tbl_stack(list(tbl_trials, tbl_interventions_top))

# gt_tbl_trials <- as_gt(tbl_characteristics)
# gt::gtsave(gt_tbl_trials, here::here("docs", "figures", "tbl-trials.png"))
# writeLines(gt::as_rtf(gt_tbl_trials), here::here("docs", "figures", "tbl-trials.rtf"))
