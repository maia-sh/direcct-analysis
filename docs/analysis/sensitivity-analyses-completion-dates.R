# Completion dates sensitivity analyses

dir_sens <- fs::dir_create(here::here("data", "reporting", "sensitivity-analyses"))

# Function for completion data sensitivity screening ----------------------

add_trials_screening_cd_sens <- function(cd_type){
  trials_screening_cd_sens <-
    trials_screening |>

    # Add in completion date
    left_join(
      select(completion_dates, id,
             date_completion = all_of(cd_type))
    ) |>

    # Determine whether cd passes
    mutate(is_cd_cutoff_3 = if_else(
      date_completion < PHASE_3_CUTOFF,
      TRUE, FALSE
    )) |>

    # Add flag for whether results
    left_join(trials_w_results, by = "id")

  trials_screening_cd_sens
}

screen_trials_cd_sens <- function(trials_screening_cd_sens){

  trials_cd_sens <-
    trials_screening_cd_sens |>
    filter(is_pass_screening_manual, is_any_rcd_cutoff_3, is_cd_cutoff_3)

  trials_cd_sens
}


# 7) study completion (exclude primary) -----------------------------------

trials_screening_scd <- add_trials_screening_cd_sens("date_completion_study_last_updated_prefer_euctr")
trials_scd <- screen_trials_cd_sens(trials_screening_scd)

sens_scd_n_trials <- nrow(trials_scd)
sens_scd_n_trials_w_results <- nrow(filter(trials_scd, has_full_result))
sens_scd_p_trials_w_results <- sens_scd_n_trials_w_results/sens_scd_n_trials

km_scd <- prepare_km(trials_scd)
readr::write_csv(km_scd, fs::path(dir_sens, "kaplan-meier-time-to-pub_scd.csv"))


# 6) rcd from april 2022 --------------------------------------------------
# Some registrations not captured in 2022 so use 2021

trials_screening_22 <- add_trials_screening_cd_sens("date_completion_22_21_last_updated_prefer_euctr")
trials_22 <- screen_trials_cd_sens(trials_screening_22)

sens_22_n_trials <- nrow(trials_22)
sens_22_n_trials_w_results <- nrow(filter(trials_22, has_full_result))
sens_22_p_trials_w_results <- sens_22_n_trials_w_results/sens_22_n_trials

km_22 <- prepare_km(trials_22)
readr::write_csv(km_22, fs::path(dir_sens, "kaplan-meier-time-to-pub_22.csv"))

trials_screening_22_main <-
  trials_screening_22 |>
  rename(
    is_cd_cutoff_3_22 = is_cd_cutoff_3,
    date_completion_22 = date_completion
  ) |>

  # Use cd from result when available, else use main analysis cd
  left_join(select(trials_screening_main, id, is_cd_cutoff_3_main = is_cd_cutoff_3, date_completion_main = date_completion), by = "id") |>

  # Add count for whether cd changes/(dis)appears
  mutate(
    cd_22_only = !is.na(date_completion_22) & is.na(date_completion_main),
    cd_21_only = is.na(date_completion_22) & !is.na(date_completion_main),
    cd_22_later = date_completion_22 > date_completion_main,
    cd_22_earlier = date_completion_22 < date_completion_main
  )

n_cd_22_only <- nrow(filter(trials_screening_22_main, is_pass_screening_manual, cd_22_only))
n_cd_21_only <- nrow(filter(trials_screening_22_main, is_pass_screening_manual, cd_21_only))
n_cd_22_later <- nrow(filter(trials_screening_22_main, is_pass_screening_manual, cd_22_later))
n_cd_22_earlier <- nrow(filter(trials_screening_22_main, is_pass_screening_manual, cd_22_earlier))
n_cd_22_changed <- n_cd_22_only + n_cd_21_only + n_cd_22_later + n_cd_22_earlier

# Look into trials with removed cds
filter(trials_screening_22_main, is_pass_screening_auto, cd_21_only)

# Sanity check number of changed trials
if (!trials_screening_22_main |>
    filter(is_pass_screening_manual,
           cd_22_only |
           cd_21_only |
           cd_22_later |
           cd_22_earlier
    ) |>
    nrow() |>
    identical(n_cd_22_changed)) {
  stop("Mismatch in number of trials with completion date changes between July 2021 and April 2022")
}

# Trials that should be manually screened per 22 cd
n_cd_22_manual_screen <- nrow(filter(trials_screening_22_main, is_pass_screening_auto, is_cd_cutoff_3_22))

# Some trials pass auto screening and valid 2022 date but screened out with 21 date
n_cd_22_manual_screen_missing <- nrow(filter(trials_screening_22_main, is_pass_screening_auto, is_cd_cutoff_3_22, !is_any_rcd_cutoff_3))

# Sanity check missing trials from 22 manual screening
trials_22_missing <-
  trials_screening_22 |>
  filter(is_pass_screening_auto, is_cd_cutoff_3, !is_any_rcd_cutoff_3)

if (nrow(trials_22_missing) != n_cd_22_manual_screen_missing){
  stop("Mismatch in number of trials missing from manual screening per April 2022 cd")
}

# NOTE: 22 actually extracted; 93 extractions missing
# count(trials_22_missing, is_extracted)


# 4) latest rcd regardless of cutoff --------------------------------------

trials_screening_latest_rcd <- add_trials_screening_cd_sens("date_completion_rcd_latest")
trials_latest_rcd <- screen_trials_cd_sens(trials_screening_latest_rcd)

sens_latest_rcd_n_trials <- nrow(trials_latest_rcd)
sens_latest_rcd_n_trials_w_results <- nrow(filter(trials_latest_rcd, has_full_result))
sens_latest_rcd_p_trials_w_results <- sens_latest_rcd_n_trials_w_results/sens_latest_rcd_n_trials

km_latest_rcd <- prepare_km(trials_latest_rcd)
readr::write_csv(km_latest_rcd, fs::path(dir_sens, "kaplan-meier-time-to-pub_latest_rcd.csv"))


# 5) rcd based on ictrp registration --------------------------------------

trials_screening_ictrp <- add_trials_screening_cd_sens("date_completion_ictrp")
trials_ictrp <- screen_trials_cd_sens(trials_screening_ictrp)

sens_ictrp_n_trials <- nrow(trials_ictrp)
sens_ictrp_n_trials_w_results <- nrow(filter(trials_ictrp, has_full_result))
sens_ictrp_p_trials_w_results <- sens_ictrp_n_trials_w_results/sens_ictrp_n_trials

km_ictrp <- prepare_km(trials_ictrp)
readr::write_csv(km_ictrp, fs::path(dir_sens, "kaplan-meier-time-to-pub_ictrp.csv"))


# study completion status + rcd -------------------------------------------

trials_screening_completion_status <-
  trials_screening_main |>

  # Add flag for whether results
  left_join(trials_w_results, by = "id")

trials_completion_status <-
  trials_screening_completion_status |>
  filter(is_pass_screening_manual, is_any_rcd_cutoff_3, is_cd_cutoff_3, status_complete)

sens_completion_status_n_trials <- nrow(trials_completion_status)
sens_completion_status_n_trials_w_results <- nrow(filter(trials_completion_status, has_full_result))
sens_completion_status_p_trials_w_results <- sens_completion_status_n_trials_w_results/sens_completion_status_n_trials

km_data_completion_status <-
  km_main |>
  semi_join(trials_completion_status, by = "id")

readr::write_csv(km_data_completion_status, fs::path(dir_sens, "kaplan-meier-time-to-pub_latest_completion_status.csv"))


# 8) cd reported in results -----------------------------------------------

trials_screening_cd_results <- add_trials_screening_cd_sens("date_completion_results")

trials_cd_results_all <- screen_trials_cd_sens(trials_screening_cd_results)

# Some results post-results-cutoff, so should exclude
trials_cd_results_all |>
  filter(is.na(has_full_result)) %>%
  semi_join(results_all, ., by = "id") |>
  filter(stringr::str_detect(pub_type, "full|summary")) |>
  count(date_publication > RESULTS_CUTOFF)

trials_screening_cd_results_main <-
  trials_screening_cd_results |>

  # Exclude cd from results post-results-cutoff
  mutate(is_cd_cutoff_3 = if_else(
    !id %in% results$id,
    NA,
    is_cd_cutoff_3
  )) |>
  rename(
    is_cd_cutoff_3_results = is_cd_cutoff_3,
    date_completion_results = date_completion
  ) |>

  # Use cd from result when available, else use main analysis cd
  left_join(select(trials_screening_main, id, is_cd_cutoff_3_main = is_cd_cutoff_3, date_completion_main = date_completion), by = "id") |>

  mutate(
    is_cd_cutoff_3 = coalesce(is_cd_cutoff_3_results, is_cd_cutoff_3_main),
    date_completion = coalesce(date_completion_results, date_completion_main)
    )

# How many trials change screening? None!
# NOTE: Throw error if not
trials_screening_cd_results_main |>
  filter(!is.na(is_cd_cutoff_3_results)) |>
  filter(is_cd_cutoff_3_results != is_cd_cutoff_3_main) %>%
  assertr::verify(nrow(.) == 0)

# How many trials change date?
trials_cd_results_main_change <-
  trials_screening_cd_results_main |>
  filter(!is.na(is_cd_cutoff_3_results)) |>
  filter(date_completion_results != date_completion_main) |>
  count(cd_results_later = date_completion_results > date_completion_main)

trials_cd_results <- screen_trials_cd_sens(trials_screening_cd_results_main)

sens_cd_results_n_trials <- nrow(trials_cd_results)
sens_cd_results_n_trials_w_results <- nrow(filter(trials_cd_results, has_full_result))
sens_cd_results_p_trials_w_results <- sens_cd_results_n_trials_w_results/sens_cd_results_n_trials

km_cd_results <- prepare_km(trials_cd_results)
readr::write_csv(km_cd_results, fs::path(dir_sens, "kaplan-meier-time-to-pub_cd_results.csv"))

cd_results <-
  readr::read_csv(here::here("data", "processed", "completion-dates-results.csv")) |>
  filter(id %in% results$id) |>
  assertr::assert(assertr::in_set(trials$id), id)

cd_results_reported <- filter(cd_results, type == "reported")
cd_results_calculated <- filter(cd_results, type == "calculated")
