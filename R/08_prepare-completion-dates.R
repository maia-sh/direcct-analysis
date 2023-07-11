# For the various completion date sensitivity analyses, we get the various completion dates for trials that pass automated screening (aside from completion date)

# 1) relevant completion date (rcd, = primary cd if available, otherwise study) in registration updated most recently prior to cutoff (i.e., July 2021), except prefer euctr rcd (though no last_updated date) [MAIN ANALYSIS]
# 2) rcd in registration updated most recently prior to cutoff (i.e., July 2021) (Note: exclude registrations without rcd even if last updated. E.g., tri01167: CTRI/2020/05/025271 updated later than NCT04358809 but former doesn't have rcd, so use later; Note: if a combination of registrations with and without last_updated date, disregard registration(s) with no last_updated date. I.e., always exclude euctr since no last_updated date; Note: if only registration(s) with no last_updated date, use latest date)
# 3) latest rcd prior to cutoff, across all registrations available in our data
# 4) latest rcd, regardless of cutoff, across all registrations available in our data
# 5) rcd based on registration in ICTRP in July 2021, and if multiple, based on study priorities (CTgov, etc.)
# 6) cd from april 22 data (if available otherwise from july 21 data if available) and even if earlier than july 21 [per main analysis]
# 7) study completion (exclude primary) [per main analysis]
# 8) cd from results publication (reported or calculated)

library(dplyr)

PHASE_3_CUTOFF <- as.Date("2021-07-01")

# Get data ----------------------------------------------------------------
dir_processed <- here::here("data", "processed")
dir_cleaned <- here::here("data", "cleaned")

trials_screening <- readr::read_csv(fs::path(dir_processed, "screening-trials.csv"))
results_all <-  readr::read_csv(fs::path(dir_processed, "matched-results.csv"))
registrations <- readr::read_csv(fs::path(dir_processed, "deduped-registrations.csv"))
registries <- readr::read_csv(fs::path(dir_cleaned, "2021-07_registries.csv"))
registries_22 <- readr::read_csv(fs::path(dir_cleaned, "2022-04_registries.csv"))

# Add completion flag to registries
registries_status <-
  registries |>
    mutate(status_complete = case_when(
      trial_status %in% c("Recruiting complete, follow-up continuing", "Recruitment complete", "Recruitment completed", "Recruiting stopped after recruiting started", "Enrollment stopped", "Inclusion stopped, follow up") ~ FALSE,
      stringr::str_detect(trial_status, "(?i)complete|compelte|terminated|prematurely ended|stopped|Main results already published") ~ TRUE,
      TRUE ~ FALSE
    ))

# registries_status |>
#   distinct(trial_status, status_complete) |>
#   arrange(desc(status_complete), trial_status) |>
#   readr::write_csv(here::here("data", "inspect", "complete-trial-statuses.csv"))

# Limit to trials that pass auto-screening
trials_pass_auto <-
  trials_screening |>
  filter(is_pass_screening_auto)

# Get all registrations (July 2021) for trials that pass auto-screening (to use for options 1-5)
reg_trials_pass_auto <-
  registrations |>
  semi_join(trials_pass_auto, by = "id") |>
  left_join(select(registries_status, -registry), by = "trn")


# NOTE: many trials (n = 1412) have no rcd for any registration
reg_trials_pass_auto_have_rcd <-
  reg_trials_pass_auto |>
  filter(!is.na(rcd))

n_trials_pass_auto_no_rcd <-
  n_distinct(trials_pass_auto$id) - n_distinct(reg_trials_pass_auto_have_rcd$id)


# 2) latest registry update pre-cutoff ------------------------------------
# 2) rcd in registration updated most recently prior to cutoff (i.e., July 2021) (Note: exclude registrations without rcd even if last updated. E.g., tri01167: CTRI/2020/05/025271 updated later than NCT04358809 but former doesn't have rcd, so use later; Note: if a combination of registrations with and without last_updated date, disregard registration(s) with no last_updated date. I.e., always exclude euctr since no last_updated date; Note: if only registration(s) with no last_updated date, use latest date)

# NOTE: some registrations have no last_updated
# tri03419: 2 registrations & neither has last_updated, NCT04402866 no rcd, EUCTR2020-001807-18 has rcd
# tri00650: euctr no last update & no rcd
# tri00744: euctr no last update but has rcd
reg_no_last_update <-
  reg_trials_pass_auto |>
  select(id, trn, registry, rcd, last_updated) |>
  filter(is.na(last_updated))
nrow(reg_no_last_update)

# NOTE: some registrations with no last_updated do have rcd, mostly euctr
count(reg_no_last_update, has_rcd = !is.na(rcd), registry)

# Get last updated registrations
reg_last_updated_exclude_missing_update_date <-
  # Registrations with rcd
  reg_trials_pass_auto_have_rcd |>

  select(id, trn, registry, rcd, scd, pcd, last_updated, status_complete) |>

  # Get latest last_updated date
  # NOTE: Disregard registrations missing last_updated
  # NOTE: This gives warnings which can be diregarded
  group_by(id) |>
  mutate(
    last_updated_latest = max(last_updated, na.rm = TRUE),
    last_updated_latest = na_if(last_updated_latest, as.Date(-Inf))
  ) |>
  ungroup() |>

  # Limit to registrations with latest last_updated (or no last_updated)
  filter((last_updated == last_updated_latest) |
           (is.na(last_updated) & is.na(last_updated_latest)))

# Get rcd of last updated registrations
rcd_last_updated_exclude_missing_update_date <-

  reg_last_updated_exclude_missing_update_date |>

  # Some trials have >1 registration updated same date (n = 2) OR >1 registration with no last_updated (n = 1) -> for these, take the latest rcd
  group_by(id) |>
  mutate(date_completion = max(rcd)) |>
  ungroup() |>

  distinct(id, date_completion, last_updated_latest, status_complete)


# 1) latest registry update pre-cutoff + euctr ----------------------------
# 1) relevant completion date (rcd, = primary cd if available, otherwise study) in registration updated most recently prior to cutoff (i.e., July 2021), except prefer euctr rcd (though no last_updated date) [MAIN ANALYSIS]

rcd_last_updated_prefer_euctr <-
  reg_trials_pass_auto_have_rcd |>

  # Get euctr rcd
  filter(registry == "EudraCT") |>

  select(id, last_updated_latest = last_updated, date_completion = rcd, status_complete) %>%

  rows_update(rcd_last_updated_exclude_missing_update_date, ., by = "id")


# 3) latest rcd pre-cutoff ------------------------------------------------
# 3) latest rcd prior to cutoff, across all registrations available in our data

rcd_latest_pre_cutoff <-

  reg_trials_pass_auto_have_rcd |>

  # Limit to rcd's before cutoff
  filter(rcd < PHASE_3_CUTOFF) |>

  # Collapse to latest rcd before cutoff
  group_by(id) |>
  summarise(date_completion = max(rcd, na.rm = TRUE))


# 4) latest rcd -----------------------------------------------------------
# 4) latest rcd, regardless of cutoff, across all registrations available in our data
rcd_latest <-

  reg_trials_pass_auto_have_rcd |>

  # Collapse to latest rcd before cutoff
  group_by(id) |>
  summarise(date_completion = max(rcd, na.rm = TRUE))


# 5) rcd per ictrp + priorities -------------------------------------------
# 5) rcd based on registration in ICTRP in July 2021, and if multiple, based on study priorities (CTgov, etc.)

rcd_ictrp <-
  trials_pass_auto |>
  select(id, trn_ictrp_priority) |>
  left_join(select(reg_trials_pass_auto_have_rcd, id, trn, date_completion = rcd, last_updated), by = c("id", "trn_ictrp_priority" = "trn"))


# 6) latest registry update pre-april 2022 + euctr ------------------------
# 6) cd from april 22 data (if available otherwise from july 21 data if available) and even if earlier than july 21

# Get all registrations (April 2022) for trials that pass auto-screening
reg_22_trials_pass_auto <-
  registrations |>
  semi_join(trials_pass_auto, by = "id") |>
  left_join(select(registries_22, -registry), by = "trn")

# Some trials (n = 108) have registry data from July 21 but not April 22; of these, some no longer resolve
# NOTE: ND manually checked April 22 historical versions of these 108 registrations and only 2 (NCT04685603 & EUCTR2020-005588-29) had completion dates which were the same as in July 21 scrapes (and also ONLY trials of these 108 with dates in July 21 scrapes) --> hence use July 21 scrapes for these missing scrapes as same data
registries_21_only <-
  anti_join(registries, registries_22, by = "trn") |>
  select(trn, registry) |>
  left_join(select(registrations, trn, resolved), by = "trn")
# count(registries_21_only, resolved, registry)
# readr::write_csv(registries_21_only, here::here("data", "inspect", "2022-04_registries_missing.csv"))

# For registrations missing 22 scrapes, use July 21 scrapes (available for 103 of 115)
reg_22_21_trials_pass_auto <-
  reg_22_trials_pass_auto |>
  filter(is.na(registry_scrape_date)) %>%
  semi_join(reg_trials_pass_auto, ., by = "trn") |>
  select(-status_complete) %>%
  rows_update(reg_22_trials_pass_auto, ., by = "trn")

# NOTE: many trials (n = 1331) have no rcd for any registration
reg_22_21_trials_pass_auto_have_rcd <-
  reg_22_21_trials_pass_auto |>
  filter(!is.na(rcd))

n_trials_pass_auto_no_rcd_22_21 <-
  n_distinct(trials_pass_auto$id) - n_distinct(reg_22_21_trials_pass_auto_have_rcd$id)

rcd_22_21_last_updated_exclude_missing_update_date <-

  # Registrations with rcd
  reg_22_21_trials_pass_auto_have_rcd |>

  select(id, trn, registry, rcd, last_updated) |>

  # Get latest last_updated date
  # NOTE: Disregard registrations missing last_updated
  # NOTE: This gives warnings which can be diregarded
  group_by(id) |>
  mutate(
    last_updated_latest = max(last_updated, na.rm = TRUE),
    last_updated_latest = na_if(last_updated_latest, as.Date(-Inf))
  ) |>
  ungroup() |>

  # Limit to registrations with latest last_updated (or no last_updated)
  filter((last_updated == last_updated_latest) |
           (is.na(last_updated) & is.na(last_updated_latest))) |>

  # Some trials have >1 registration updated same date (n = 2) OR >1 registration with no last_updated (n = 1) -> for these, take the latest rcd
  group_by(id) |>
  mutate(date_completion = max(rcd)) |>
  ungroup() |>

  distinct(id, date_completion, last_updated_latest)

rcd_22_21_last_updated_prefer_euctr <-
  reg_22_21_trials_pass_auto_have_rcd |>

  # Get euctr rcd
  filter(registry == "EudraCT") |>

  select(id, last_updated_latest = last_updated, date_completion = rcd) %>%

  rows_update(rcd_22_21_last_updated_exclude_missing_update_date, ., by = "id")


# 7) scd latest registry update pre-cutoff + euctr ------------------------
# 7) study completion (exclude primary)

# Get scd of last updated registrations
scd_last_updated_exclude_missing_update_date <-

  reg_last_updated_exclude_missing_update_date |>

  # Some trials have >1 registration updated same date (n = 2) OR >1 registration with no last_updated (n = 1) -> for these, take the latest rcd
  group_by(id) |>
  mutate(date_completion = max(scd)) |>
  ungroup() |>

  distinct(id, date_completion, last_updated_latest)

# Prefer euctr scd
scd_last_updated_prefer_euctr <-
  reg_trials_pass_auto_have_rcd |>

  # Get euctr rcd
  filter(registry == "EudraCT") |>

  select(id, last_updated_latest = last_updated, date_completion = scd) %>%

  rows_update(scd_last_updated_exclude_missing_update_date, ., by = "id")


# 8) cd reported in results -----------------------------------------------
# 8) cd from results publication (reported or calculated)

# Get completion dates from full results publications, including reported (scd, pcd, unclear) and calculated from final enrollment and follow-up (scd and/or pcd)
# NOTE: We could also look at all cds reported|calculated, pcd|scd|unclear
# date_completion_available (T/F), date_completion_reported_type (pcd, scd, unclear), date_completion_reported (date), reported_pcd (date), reported_scd(date), calculated_pcd (date), calculated_scd (date)

# Get full results with reported cd or enrollment
results_cd <-
  results_all |>
  filter(stringr::str_detect(pub_type, "full|summary")) |>
  select(id, pub_type, full_pub_group, doi, pmid, url, matches("^date_completion|date_final|followup|estimated")) |>
  filter(if_any(matches("^date_completion|date_final|followup|estimated"), ~!is.na(.)))

# Limit to results with reported cd
results_reported_cd <-
  results_cd |>
  filter(!is.na(date_completion_reported)) |>
  rename(estimated_cd = estimated_cd_enrollment)

# Some trials have multiple results with reported cd...
janitor::get_dupes(results_reported_cd, id)
# ... but all same cd of same cd type
results_reported_cd |>
  distinct(id, date_completion_reported, date_completion, estimated_cd) |>
  janitor::get_dupes()

# Get trials with reported cd
cd_reported <-
  results_reported_cd |>
  distinct(id, date_completion_type = date_completion_reported, date_completion, estimated_cd) |>

  # Confirm row per trial
  assertr::assert(assertr::is_uniq, id) |>

  mutate(type = "reported")

# Limit to results with NO reported cd
# Note: some results have both reported cd and enrollment/followup but not systematically captured so disregard
results_no_reported_cd <-
  results_cd |>
  filter(is.na(date_completion_reported)) |>
  select(-starts_with("date_completion")) |>
  rename(estimated_enrollment = estimated_cd_enrollment) |>

  # Display different calculation statuses, i.e., some have just enrollment or follow-up and could not calculate any cd; some could calculate pcd and/or scd
  mutate(cd_calc_status = case_when(

    is.na(date_final_enrollment) & (!is.na(followup_primary) | !is.na(followup_secondary)) ~ "followup_only",

    !is.na(date_final_enrollment) & is.na(followup_primary) & is.na(followup_secondary) ~ "enrollment_only",

    !is.na(date_final_enrollment) & !is.na(followup_primary) & !is.na(followup_secondary) ~ "pcd_scd",

    !is.na(date_final_enrollment) & !is.na(followup_primary) & is.na(followup_secondary) ~ "pcd",

    !is.na(date_final_enrollment) & is.na(followup_primary) & !is.na(followup_secondary) ~ "scd",
    TRUE ~ "ERROR"
  )) |>
  assertr::assert(assertr::in_set("ERROR", inverse = TRUE), cd_calc_status) |>

  # Calculate cds
  mutate(
    pcd_calc = date_final_enrollment + lubridate::days(followup_primary),
    scd_calc = date_final_enrollment + lubridate::days(followup_secondary),
    rcd_calc = if_else(!is.na(pcd_calc), pcd_calc, scd_calc)
  )

# How many results provide what type of cd calculation info?
count(results_no_reported_cd, cd_calc_status)

# Some trials have multiple results with calculated cd...
janitor::get_dupes(results_no_reported_cd, id)
# ... and cds differ
results_no_reported_cd |>
  filter(stringr::str_detect(cd_calc_status, "cd")) |>
  janitor::get_dupes(id)

# Inspect trials with discrepant calculated cd
results_no_reported_cd |>
  filter(if_any(matches("cd_calc"), ~!is.na(.))) |>
  distinct(id, rcd_calc, pcd_calc, scd_calc) |>
  janitor::get_dupes(id) %>%
  semi_join(results_no_reported_cd,., by = "id")

# Get trials with calculated cd
cd_calculated_w_info <-
  results_no_reported_cd |>

  # For trials with discrepant calculated cd from different publications, manually select, preferring articles to preprint, non-estimated, pcd & scd to just one, earlier full pub group (if two articles)
  filter(

    # Preprint has pcd in addition to same scd and enrollment as article --> select preprint
    !(id == "tri06088" & doi == "10.1016/j.intimp.2021.107916"),

    # Article and preprint differ --> select article
    !(id == "tri02982" & doi == "10.1101/2021.02.04.21251134"),

    # Two articles --> prefer earlier pubgroup
    !(id %in% c("tri00908", "tri03455") & full_pub_group == 2),

    # Article has enrollment only --> prefer preprint
    !(id == "tri02344" & doi == "10.1056/nejmoa2107659"),

    # Article estimated enrollment --> prefer article
    !(id == "tri00185" & doi == "10.1101/2020.04.15.20066266")
  ) |>

  distinct(id, date_final_enrollment, followup_primary, followup_secondary, estimated_enrollment, cd_calc_status, rcd_calc, pcd_calc, scd_calc) |>

  # Confirm row per trial
  assertr::assert(assertr::is_uniq, id)

cd_calculated <-
  cd_calculated_w_info |>
  filter(if_any(ends_with("calc"), ~!is.na(.))) |>
  select(id, ends_with("calc"), estimated_enrollment, cd_calc_status) |>
  mutate(type = "calculated")

# Check whether some trials have both reported and calculated cd --> prefer reported
# 3 trials: tri03277 (same); tri02445, tri07286 (differ)
cd_calculated |>
  semi_join(cd_reported, by = "id") |>
  left_join(cd_reported, by = "id")

# Combine reported/calculated cds
cd_results <-
  cd_calculated |>

  # For calculated cds, we use pcd when available
  mutate(date_completion_type = if_else(stringr::str_detect(cd_calc_status, "pcd"), "pcd", "scd")) |>
  select(id, date_completion = rcd_calc, estimated = estimated_enrollment, type, date_completion_type) |>
  anti_join(cd_reported, by = "id") %>%
  bind_rows(
    select(cd_reported, id, date_completion, estimated = estimated_cd, type, date_completion_type), .
  )

# NOTE: used in analysis but not in `reported` data
readr::write_csv(cd_results, fs::path(dir_processed, "completion-dates-results.csv"))

# Combine completion dates ------------------------------------------------
#TODO: include trn used for each date option? so rcd, last update, trn?
completion_dates <-
  trials_pass_auto |>
  select(id) |>

  left_join(rename(rcd_latest, date_completion_rcd_latest = date_completion)) |>
  left_join(rename(rcd_latest_pre_cutoff, date_completion_rcd_latest_pre_cutoff = date_completion)) |>

  left_join(
    select(rcd_last_updated_exclude_missing_update_date, id,
           date_completion_last_updated_exclude_missing_update_date = date_completion)
  ) |>

  left_join(
    select(rcd_last_updated_prefer_euctr, id,
           date_completion_last_updated_prefer_euctr = date_completion,
           status_complete_last_updated_prefer_euctr = status_complete)
  ) |>

  left_join(
    select(rcd_ictrp, id,
           trn_ictrp_priority,
           date_completion_ictrp = date_completion)
  ) |>
  left_join(
    select(rcd_22_21_last_updated_prefer_euctr, id,
           date_completion_22_21_last_updated_prefer_euctr = date_completion)
  ) |>
  left_join(
    select(scd_last_updated_prefer_euctr, id,
           date_completion_study_last_updated_prefer_euctr = date_completion)
  ) |>
  left_join(
    select(cd_results, id, date_completion_results = date_completion)
  )

readr::write_csv(completion_dates, fs::path(dir_processed, "completion-dates.csv"))
