# Clean raw data to unify variable names and formats

library(dplyr)
library(pointblank)


# Function: Coalesce registry names ---------------------------------------
# Registry names differ between NDs ICTRP and MSHs ctregistries
# For parity, coalesce to ctregistries
# Note: Disambiguates 3 JPRN registries

disambiguate_registries <- function(reg_df){
  reg_df |>
    rename(registry = source_register) |>
    mutate(registry = case_when(
      registry == "EU Clinical Trials Register" ~ "EudraCT",
      registry == "CRIS" ~ "CRiS",
      registry == "German Clinical Trials Register" ~ "DRKS",
      registry == "REBEC" ~ "ReBec",
      registry == "Netherlands Trial Register" ~ "NTR",

      stringr::str_detect(trn, "JPRN-UMIN") ~ "UMIN-CTR",
      stringr::str_detect(trn, "JPRN-jRCT") ~ "jRCT",
      stringr::str_detect(trn, "JPRN-JapicCTI") ~ "JapicCTI",
      TRUE ~ registry
    ))
}

# In ICTRP, EUCTR TRNs have country codes/text (i.e., tri08064/EUCTR2021-001279-18-Outside/EEA)
# Therefore, often >1 row per EUCTR TRN
# Extract country code to separate variable and remove from TRN
tidy_euctr <- function(reg_df){
  reg_df |>
    mutate(
      euctr_country = if_else(
        stringr::str_detect(trn, "^EUCTR"),
        stringr::str_remove(trn, ".{20}"),
        NA_character_
      ),
      trn = if_else(
        stringr::str_detect(trn, "^EUCTR"),
        stringr::str_extract(trn, "^EUCTR20\\d{2}\\W*0\\d{5}\\W*\\d{2}"),
        trn
      )
    )
}


# Phase 1 Data ------------------------------------------------------------
trials_1 <-
  readr::read_csv(here::here("data", "raw", "trials_1.csv")) |>
  rename(trn = trialid) |>
  disambiguate_registries() |>
  tidy_euctr()
# registrations_1 <- readr::read_csv(here::here("data", "raw", "registrations_1.csv"))
# results_1 <- readr::read_csv(here::here("data", "raw", "results_1.csv"))

readr::write_csv(trials_1, here::here("data", "cleaned", "trials_1.csv"))


# ICTRP -------------------------------------------------------------------

ictrp <-
  readr::read_csv(here::here("data", "raw", "2021-07-01_ictrp.csv")) |>

  # Clean names
  select(-1, trn = trialid) |>

  disambiguate_registries() |>
  tidy_euctr() |>

  # Add title for EUCTR missing title
  mutate(public_title = if_else(
    trn == "EUCTR2021-002174-52",
    "Essai multicentrique randomisé en ouvert comparant l’efficacité immunologique d’un schéma vaccinal combinant deux vaccins ARNm Covid19 (Pfizer-BioNTech et Moderna) à celle d’une vaccination homologue de chaque vaccin ARNm Covid19 : essai de non infériorité",
    public_title
  )) |>

  # TODO: delete
  # Add date_registration for REBEC interventional trials
  # REPEC "PER-002-21" seems to no longer exist so no date found
  # rows_update(
  #   tibble::tribble(
  #     ~trn, ~date_registration,
  #     "RBR-62936mf", as.Date("2020-12-28"),
  #     "RBR-7rgqkhf", as.Date("2021-01-15"),
  #     "RBR-8z7v5wc", as.Date("2021-02-05"),
  #     "RBR-2tdj6vs", as.Date("2021-03-30"),
  #     "RBR-108fyykd", as.Date("2021-04-14"),
  #     "RBR-95wmphn", as.Date("2021-05-02"),
  #     "RBR-2ps876h", as.Date("2021-05-14"),
  #     "RBR-6gdmb8c", as.Date("2021-05-21")
  #   ), by = "trn"
  # ) |>

  # Check that all trials have title
  assertr::assert(assertr::not_na, public_title) |>

  # Check that trials have registration date, except known missing
  # Some REBEC (n=38) and REPEC (n=1) trials are missing registration date
  # REPEC "PER-002-21" seems to no longer exist so no date found
  # REBEC doesn't include completion date so would be excluded anyways
  pointblank::col_vals_not_null(
    vars(date_registration),
    preconditions = ~ . %>% filter(registry != "ReBec" & trn != "PER-002-21")
  )

# There should be dupes only for different EUCTR country codes
# They have different data (i.e., registration dates, titles), so keep for screening checks
janitor::get_dupes(ictrp, trn) |>
  assertr::verify(registry == "EudraCT")

readr::write_csv(ictrp, here::here("data", "cleaned", "2021-07-01_ictrp.csv"))

# Registries --------------------------------------------------------------

# We use registry data from 2021-07 for phase 3 trial screening. However, NDs scrapes in 2021-07 did not include the entire 2021-07 ICTRP COVID list, so for missing trials we use data from (1) registry data scrapes from 2022-04, if `last_updated` prior to earliest scrape date in 2021-07, and (2) if not available and trial eligible based on ICTRP, we manually check registries for data prior to that date (n = 9, none of which have registrations)
# We also had some cross-registrations missing registry data. We manually gathered historic completion date data in 2022-10 to use to check the inclusion of cross-registered trials across registrations.

# ND scraped various registries between 2021-07-11 and 2021-07-18
REGISTRY_SCRAPE_MIN <- as.Date("2021-07-11")
REGISTRY_SCRAPE_MAX <- as.Date("2021-07-18")

clean_registrations <- function(reg_df){
  reg_df |>
    select(-1, -relevant_comp_date) |>
    rename(trn = trial_id) |>
    mutate(rcd = if_else(!is.na(pcd), pcd, scd), .after = "scd") |>
    mutate(across(
      c(pcd, scd, rcd, last_updated), ~ as.Date(., "%d/%m/%Y")
    )) |>
    mutate(across(c(tabular_results, potential_other_results), ~if_else(. == 1, TRUE, FALSE))) |>
    distinct() |>

    # Add EUCTR prefix
    mutate(trn = if_else(
      stringr::str_detect(trn, "^20\\d{2}\\W*0\\d{5}\\W*\\d{2}"),
      stringr::str_c("EUCTR", trn),
      trn
    )) |>

    # Get registry
    # NOTE: computationally inefficient, rework function
    rowwise() |>
    mutate(registry = ctregistries::which_registry(trn), .after = "trn") |>
    ungroup() |>

    arrange(registry)
}

registries_2107 <-
  readr::read_csv(here::here("data", "raw", "2021-07_registries.csv")) |>
  clean_registrations() |>
  mutate(registry_scrape_date = "2021-07")

registries_2204 <-
  readr::read_csv(here::here("data", "raw", "2022-04_registries.csv")) |>

  # Remove invalid trn
  filter(stringr::str_detect(trial_id, "\\?", negate = TRUE)) |>

  clean_registrations() |>
  mutate(registry_scrape_date = "2022-04")

# ND post-hoc collected registry data from 2021-07 for several cross-registrations
registries_2107_manual <-
  readr::read_csv(here::here("data", "manual", "2021-07_registries-manual.csv")) |>
  select(-resolved, -rcd) |>

  # Recalculate rcd in case
  mutate(rcd = if_else(!is.na(pcd), pcd, scd), .after = "scd")

# Confirm that later registry data not in earlier data
semi_join(registries_2107_manual, registries_2107, by = "trn") %>%
  assertr::verify(nrow(.) == 0)

# Confirm that no later registry data with includable registry scrape date not in earlier data
registries_2204 |>
  semi_join(registries_2107_manual, by = "trn") |>
  filter(last_updated < REGISTRY_SCRAPE_MIN) %>%
  assertr::verify(nrow(.) == 0)

# Combine 2021-07 data (auto and manual)
registries_2107_auto_manual <-
  bind_rows(registries_2107, registries_2107_manual)

# Add additional registrations from 2022-04 data last updated prior to 2021-07 scrapes
# May include registrations NOT in 2021-07 ictrp
registries_2107_2204 <-
  registries_2204 |>

  # Limit to registrations updated prior to earliest 2021-07 scrapes
  filter(last_updated < REGISTRY_SCRAPE_MIN) |>

  # Limit to registrations not already in 2021-07 registries
  anti_join(registries_2107_auto_manual, by = "trn") %>%

  # Combine with 2021-07 registries
  bind_rows(registries_2107_auto_manual, .) |>
  arrange(registry)

readr::write_csv(registries_2107_2204, here::here("data", "cleaned", "2021-07_registries.csv"))

readr::write_csv(registries_2204, here::here("data", "cleaned", "2022-04_registries.csv"))
