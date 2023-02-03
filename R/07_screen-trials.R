# Screen trials using ICTRP, registries, and manual extractions
# Save intermediary screening dataframes for trial inclusion flow chart


library(dplyr)


# Set global variables ----------------------------------------------------

PHASE_3_CUTOFF <- as.Date("2021-07-01")


# Get data ----------------------------------------------------------------

trials_1 <- readr::read_csv(here::here("data", "cleaned", "trials_1.csv"))

ictrp <- readr::read_csv(here::here("data", "cleaned", "2021-07-01_ictrp.csv"))

registries <-
  readr::read_csv(here::here("data", "cleaned", "2021-07_registries.csv")) |>
  mutate(has_registry_data = TRUE)

registries_2204 <- readr::read_csv(here::here("data", "cleaned", "2022-04_registries.csv"))

registrations <- readr::read_csv(here::here("data", "processed", "deduped-registrations.csv"))

trials <- readr::read_csv(here::here("data", "processed", "deduped-trials.csv"))

# Check data assumptions --------------------------------------------------

# Check that all trials in ICTRP in registrations
ictrp |>
  anti_join(registrations, by = "trn") |>
  nrow() %>%
  {if(. > 0) rlang::abort("There are trials in ICTRP missing from registrations!")}


# Prepare screening: registries -------------------------------------------

screening_registries <-
  registries |>
  # Add ids
  left_join(select(registrations, id, trn, registry), by = c("trn", "registry")) |>
  relocate(id, .before = trn) |>

  # Check that all trials have ids and no rows added
  assertr::assert(assertr::not_na, id)  %>%
  assertr::verify(nrow(.) == nrow(registries)) |>

  # Create screening variables
  mutate(

    # For ClinicalTrials.gov, withdrawn status in registry data not ictrp
    is_not_withdrawn_registry =  !trial_status %in% "Withdrawn",

    # Completion date prior to cutoff
    is_rcd_cutoff_3 =
      if_else(
        rcd < PHASE_3_CUTOFF,
        TRUE, FALSE#,missing = FALSE # NOTE: Exclude rcd=NA so could code as FALSE
      )
  )

# Summarize registry screening across cross-registrations to row per unique trial
# Exclude if ANY withdrawn
# Include if ANY completion date passes
screening_deduped_registries <-
  screening_registries |>
  group_by(id) |>
  summarize(
    trn_registries = paste(trn, collapse = ";"),
    n_registries = n(),
    is_not_withdrawn_registry = all(is_not_withdrawn_registry),
    is_rcd_cutoff_3 = any(is_rcd_cutoff_3)
  ) |>
  mutate(in_registries = TRUE, .after = id)

# For cross-registered trials, we want to include trials completed per ANY registry. Therefore, if excluded based on missing or post-cutoff completion date, we want to be sure we have checked all registrations, as they stood on phase 3 cutoff.
# To do this, we get all trials excluded based on completion date. Then we get all cross-registrations, which resolve. Then we check later registry scrapes visually.

excluded_trials_missing_crossreg <-
  screening_deduped_registries |>

  # Limit to trials excluded based on completion date
  filter(!is_rcd_cutoff_3 | is.na(is_rcd_cutoff_3)) |>

  # Get all trns associated with excluded trials
  # NOTE: limit to registrations that resolve
  left_join(
    filter(registrations, is.na(resolved)) |> select(id, trn),
    by = "id") |>

  # Limit to trials with cross-registrations (>1 trn)
  janitor::get_dupes(id) |>

  select(id, trn) |>

  # Join in 2021-07 registry data
  left_join(select(screening_registries, id, trn, has_registry_data, rcd, is_rcd_cutoff_3), by = c("id", "trn")) |>

  # Limit to registrations missing 2021-07 registry data
  filter(!has_registry_data | is.na(has_registry_data)) |>
  arrange(trn) |>
  select(id, trn) |>

  # Join in later registry data
  left_join(
    select(registries_2204, trn, rcd, last_updated, registry_scrape_date),
    by = "trn"
  )

# Note: Manually checked these n=3 cross-registrations and all would be excluded based on completion date (n=1) or registration date (n=2)

# Prepare screening: ICTRP + phase 1 --------------------------------------

# Per ICTRP/registry data from 2021-07:
# Intervention
# Completion date < 2021-07-01  TODO:REMOVE
# Registration date > 2020-01-01
# Not withdrawn (from both ICTRP & registry)
# Additionally, for trials including in Phase 1, not manually manually excluded

trials_1_manually_excluded <-
  trials_1 |>
  filter(is_searched_cutoff_1) |>
  filter(!is_clinical_trial_manual | !is_covid_manual | !is_not_withdrawn_manual)

screening_ictrp <-
  ictrp |>

  # Add ids
  left_join(select(registrations, id, trn, registry), by = c("trn", "registry")) |>
  relocate(id, .before = trn) |>

  # Check that all trials have ids and no rows added
  assertr::assert(assertr::not_na, id) %>%
  assertr::verify(nrow(.) == nrow(ictrp)) |>


  # Create screening variables
  mutate(

    is_phase_1_manually_excluded =
      if_else(trn %in% trials_1_manually_excluded$trn | id %in% trials_1_manually_excluded$id, TRUE, FALSE),

    is_intervention =
      if_else(study_type %in% c("Interventional", "Prevention"), TRUE, FALSE),

    is_reg_2020 =
      if_else(
        date_registration >= "2020-01-01",
        TRUE, FALSE, missing = NA
        # missing = FALSE # Exclude REPEC "PER-002-21" with no rcd/ registry record, and ReBeC with no completion dates in registry
      ),

    is_not_withdrawn_ictrp =
      if_else(

        # For ChiCTR use variable from ICTRP. Data from other registries doesn't reliably make it to the ICTRP. Withdrawn trials from ClinicalTrials.gov removed from using registry data.
        !stringr::str_detect(public_title, "Cancelled|(Retracted due to)") &

          # # For ClinicalTrials.gov, use registries dataset
          # !trial_status %in% "Withdrawn" &

          # In ICTRP, withdrawn trials may be indicated as recruitment_status
          !recruitment_status %in% "Withdrawn",
        TRUE, FALSE
      ),

    # Eligible studies based on auto criteria and phase 1 screening
    # is_eligible_study =
    #   if_else(
    #     is_intervention & is_reg_2020 & is_not_withdrawn & is.na(is_phase_1_manually_excluded),
    #     TRUE, FALSE
    #   ),
    #
    # is_rcd_cutoff_3 =
    #   if_else(
    #     rcd < PHASE_3_CUTOFF,
    #     TRUE, FALSE#,missing = FALSE # NOTE: Exclude rcd=NA so could code as FALSE
    #   )
  )


# Summarize ictrp screening across cross-registrations to row per unique trial
# Include if ANY intervention, registered
# Exclude if ANY withdrawn
# Exclude if ANY manually excluded in phase 1 (though NO cross-registered were excluded in phase 1)
screening_deduped_ictrp <-
  screening_ictrp |>
  group_by(id) |>
  summarize(
    trn_ictrp = paste(trn, collapse = ";"),
    n_ictrp = n(),
    is_intervention = any(is_intervention),
    is_reg_2020 = any(is_reg_2020),
    is_not_withdrawn_ictrp = all(is_not_withdrawn_ictrp),
    is_phase_1_manually_excluded = any(is_phase_1_manually_excluded)
  ) |>
  mutate(in_ictrp = TRUE, .after = id)


# Prepare screening: registries + ICTRP + phase 1 + manual ----------------
# Create dataframe with one row per unique trial with data from all sources

# from this, should be able to make flow chart...
#
# id
# trn_all
# in_ictrp_21
# trn_ictrp
# in_registries_21
# trn_registries
# is_intervention (ANY crossreg in ictrp)
# is_reg_2020 (ANY crossreg in ictrp)
# is_not_withdrawn (ALL crossreg in ictrp)
# is_not_withdrawn_ictrp
# is_not_withdrawn_registry
# is_phase_1_manually_excluded (ANY crossreg in ictrp)
# is_rcd
# manual intervention
# manual covid
# manual not withdrawn

screening_trials <-

  # Collapse to row per trial with all cross-registrations
  registrations |>
  group_by(id) |>
  summarize(trn_all = paste(trn, collapse = ";")) |>

  # Add ICTRP and registries screening
  left_join(screening_deduped_ictrp, by = "id") |>
  left_join(screening_deduped_registries, by = "id") |>

  # Trial not auto withdrawn if not withdrawn per ictrp or registry
  mutate(is_not_withdrawn_auto = is_not_withdrawn_ictrp & is_not_withdrawn_registry) |>

  mutate(is_not_phase_1_manually_excluded = !is_phase_1_manually_excluded) |>
  select(-is_phase_1_manually_excluded) |>

# Add manual screening
  left_join(
    trials |>
    select(id, is_clinical_trial_manual, is_covid_manual, is_not_withdrawn_manual) |> mutate(is_extracted = TRUE),
    by = "id"
  ) |>

  # Verify no duplicate ids
  assertr::assert(assertr::is_uniq, id) |>

  # Add analysis population flags
  mutate(

    # Since numerous sensitivity analyses with different completion, create flag for analysis except completion
    is_pass_screening_ignore_cd = if_else(
      in_ictrp &
        is_reg_2020 &
        is_intervention &
        is_not_withdrawn_auto &
        is_not_phase_1_manually_excluded &
        is_clinical_trial_manual &
        is_covid_manual &
        is_not_withdrawn_manual,
      TRUE, FALSE, missing = FALSE),

    # Main analysis population (considering completion date)
    is_analysis_pop = if_else(
      is_pass_screening_ignore_cd & is_rcd_cutoff_3,
      TRUE, FALSE, missing = FALSE)
  )

# TODO: Figure out metaprogramming
# screening_criteria_auto <- c(
#   "in_ictrp",
#   "is_reg_2020",
#   "is_intervention",
#   "is_not_withdrawn_auto",
#   "is_not_phase_1_manually_excluded",
#   "is_rcd_cutoff_3"
# )
#
# screening_criteria <- c(
#   screening_criteria_auto,
#   "is_clinical_trial_manual",
#   "is_covid_manual",
#   "is_not_withdrawn_manual"
# )

# All trials that pass auto-screening should be extracted
screening_trials |>
  filter(
    in_ictrp,
    is_reg_2020,
    is_intervention,
    is_not_withdrawn_auto,
    is_not_phase_1_manually_excluded,
    is_rcd_cutoff_3
  ) |>
  assertr::assert(assertr::not_na, is_extracted)

readr::write_csv(screening_trials, here::here("data", "processed", "screening-trials.csv"))