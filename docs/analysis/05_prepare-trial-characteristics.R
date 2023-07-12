# Create table of trial characteristics for included trials using priority ictrp registration
# Additional characteristics (but would need more processing): retrospective_registration, study_type, primary_purpose, blinding, control_arm

# Prepare cross-registrations ---------------------------------------------

trials_all_registries <-
  registrations |>

  # Get all trns for included trials
  semi_join(trials, by = "id") |>


  # Limit to distinct registries per trial
  # Note: some trials, e.g. tri00985, have >1 registration but in single registry, so count for registry and not cross-registration
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

# Prepare completion status -----------------------------------------------
# Encode as complete, terminated, other
# Note: completion status categories determined by MSH and ND consensus

completion_status <-
  completion_dates |>
  select(
    id,
    trial_status = trial_status_last_updated_prefer_euctr,
    status_complete = status_complete_last_updated_prefer_euctr
  ) |>
  mutate(completion_status = case_when(
    stringr::str_detect(trial_status, "Terminated|Prematurely Ended|Stopped early") ~ "Terminated",
    status_complete ~ "Completed",
    !status_complete | is.na(status_complete) ~ "Other"
  ))

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
  left_join(pandemic_semester, by = "id") |>

  # Confirm randomization available
  assertr::assert(assertr::not_na, c(study_design, is_randomized)) |>

  # Add completion_status
  left_join(select(completion_status, id, completion_status), by = "id")


# Prepare top interventions -----------------------------------------------

# Get trials with row per unique intervention
# Note: There are trials in multiple rows but not same intervention in multiple rows (even if used in multiple arms)
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

# Number of trials investigating each intervention
trials_per_intervention <-
  trials_interventions |>
  count(intervention, name = "n_trials") |>
  arrange(desc(n_trials))

# Number of interventions investigated in a trial
interventions_per_trial <-
  trials_interventions |>
  count(id, name = "n_interventions") |>
  count(n_interventions, name = "n_trials")

# How many interventions were investigated in how many trials?
# E.g., 2 interventions (azythromyacin and ivermectin) were investigated in 46 trials
# trials_per_intervention |>
#   count(n_trials, name = "n_intervention")

N_TOP_INTERVENTIONS <- 5

# Top interventions
# Note: "traditional medicine" collapses many interventions and hence not a true intervention
top_interventions <-
  trials_per_intervention |>
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

  select(has_result,target_enrollment, crossreg, multinational, is_randomized, completion_status, phase, semester_fct) |>

  gtsummary::tbl_summary(
    by = has_result,
    missing = "no",
    label = list(
      target_enrollment ~ "Target enrollment",
      crossreg ~ "Cross-registered",
      multinational ~ "Multinational",
      is_randomized ~ "Randomized",
      completion_status ~ "Trial Status",
      phase ~ "Trial Phase",
      semester_fct ~ "Pandemic Phase"
    )
  )|>

  add_overall()|>
  bold_labels()


# Combine characteristics table -------------------------------------------

tbl_characteristics <-
  tbl_stack(list(tbl_trials, tbl_interventions_top))
