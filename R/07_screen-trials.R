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
        TRUE, FALSE#,missing = FALSE # Note: Exclude rcd=NA so could code as FALSE
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
    is_not_withdrawn_registry = all(is_not_withdrawn_registry, na.rm = TRUE),
    is_any_rcd_cutoff_3 = any(is_rcd_cutoff_3, na.rm = TRUE)
  ) |>
  mutate(in_registries = TRUE, .after = id)

# For cross-registered trials, we want to include trials completed per ANY registry. Therefore, if excluded based on missing or post-cutoff completion date, we want to be sure we have checked all registrations, as they stood on phase 3 cutoff.
# To do this, we get all trials excluded based on completion date. Then we get all cross-registrations, which resolve. Then we check later registry scrapes visually.

excluded_trials_missing_crossreg <-
  screening_deduped_registries |>

  # Limit to trials excluded based on completion date
  filter(!is_any_rcd_cutoff_3) |>

  # Get all trns associated with excluded trials
  # Note: limit to registrations that resolve
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
# Intervention (any)
# Completion date  < 2021-07-01  (any)
# Registration date > 2020-01-01 (any)
# Not withdrawn (from both ICTRP & registry) (all)
# Additionally, for trials including in Phase 1, not manually manually excluded (all)

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
      )
  )

# Get deduped trn in ictrp
# Note: collapse multiple euctr registrations, also for count (i.e., mutliple euctr countries count as single registration)
screening_deduped_ictrp_trn <-
  screening_ictrp |>
  distinct(id, trn) |>
  group_by(id) |>
  summarize(
    trn_ictrp = paste(trn, collapse = ";"),
    n_ictrp = n()
  )

screening_deduped_ictrp <-
  screening_ictrp |>
  group_by(id) |>
  summarize(
    is_intervention = any(is_intervention),
    is_reg_2020 = any(is_reg_2020),
    is_not_withdrawn_ictrp = all(is_not_withdrawn_ictrp),
    is_phase_1_manually_excluded = any(is_phase_1_manually_excluded)
  ) |>
  left_join(screening_deduped_ictrp_trn) |>
  relocate(trn_ictrp, n_ictrp, .after = id) |>
  mutate(in_ictrp = TRUE, .after = id)


# Prepare screening: registries + ICTRP + phase 1 + manual ----------------
# Create dataframe with one row per unique trial with data from all sources

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

  mutate(is_extracted = tidyr::replace_na(is_extracted, FALSE)) |>

  # Verify no duplicate ids
  assertr::assert(assertr::is_uniq, id) |>

  # Add analysis population flags
  mutate(

    # Since numerous sensitivity analyses with different completion, create flag for pass auto and all screening, except completion

    is_pass_screening_auto = if_else(
      in_ictrp &
        is_reg_2020 &
        is_intervention &
        is_not_withdrawn_auto &
        is_not_phase_1_manually_excluded,
      TRUE, FALSE, missing = FALSE),

    is_pass_screening_manual_ignore_cd = if_else(
      is_pass_screening_auto &
        is_clinical_trial_manual &
        is_covid_manual &
        is_not_withdrawn_manual,
      TRUE, FALSE, missing = FALSE),

    # Pass screening considering *any* completion date
    is_pass_screening_manual = if_else(
      is_pass_screening_manual_ignore_cd & is_any_rcd_cutoff_3,
      TRUE, FALSE, missing = FALSE)
  ) |>

  # Add priority trn in ictrp based on prespecified study priorities
  mutate(trn_ictrp_priority = case_when(

    # If single or no ictrp registration, use that
    stringr::str_detect(trn_ictrp, ";", negate = TRUE)|is.na(trn_ictrp) ~ trn_ictrp,

    # If multiple ictrp registrations...
    # Prioritize ctgov
    # If multiple ctgov, visually/manually select one (prefer lastest registry update pre-cutoff with rcd)
    id == "tri01906" ~ "NCT04444700",
    id == "tri02944" ~ "NCT04348656",
    id == "tri01243" ~ "NCT04330690",
    id == "tri03086" ~ "NCT04399980",
    id == "tri00985" ~ "NCT04325906",
    stringr::str_detect(trn_ictrp, "NCT\\d{8}") ~ stringr::str_extract(trn_ictrp, "NCT\\d{8}"),

    # Else prioritize euctr (no multiple euctr after removing country dupes)
    stringr::str_detect(trn_ictrp, "EUCTR20\\d{2}-\\d{6}-\\d{2}") ~ stringr::str_extract(trn_ictrp, "EUCTR20\\d{2}-\\d{6}-\\d{2}"),

    # Else manually select (prefer lastest registry update pre-cutoff with rcd)
    # tri08138: ChiCTR2100041705 & ChiCTR2100041706 same data so arbitrarily select
    id == "tri08138" ~ "ChiCTR2100041705",

    # Else throw error
    TRUE ~ "ERROR"
  ), .after = "n_ictrp") |>
  assertr::assert(assertr::in_set("ERROR", allow.na = FALSE, inverse = TRUE), trn_ictrp_priority)

# All trials that pass auto-screening should be extracted
screening_trials |>
  filter(
    is_pass_screening_auto,
    is_any_rcd_cutoff_3
  ) |>
  assertr::assert(assertr::in_set(TRUE), is_extracted)

readr::write_csv(screening_trials, here::here("data", "processed", "screening-trials.csv"))
