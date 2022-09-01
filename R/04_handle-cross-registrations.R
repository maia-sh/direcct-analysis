# Combine and deduplicate ALL registrations, including:
# - Numbat extractions
# - Pre-Numbat input extractions (e.g., from Phase 1, ICTRP, registries, etc.)
#   NOTE: Manually added registrations from pre-numbat repository
# - Cross-registrations checks 2022

library(dplyr)

source(here::here("R", "functions", "latest.R"))


# Combine registrations from numbat and pre-numbat input ------------------

reg_numbat <-
  readr::read_csv(latest("registrations.csv", here::here("data", "cleaned")))

reg_input <-
  readr::read_csv(here::here("data", "manual", "cleaned-registrations-3.csv")) |>

  # Use source 3 as source (differs from source 2 rarely)
  select(id, trn, registry, source_input = source_3)

reg_source <-

  reg_numbat |>

  # Add input registrations
  full_join(reg_input, by = c("id", "trn", "registry")) |>

  arrange(id)

# NOTE: `registrations_source` may include duplicates from different usernames as uploaded to numbat
janitor::get_dupes(reg_source, id, trn, registry)

reg_input_numbat <-
  reg_source |>
  distinct(id, trn, registry)

# readr::write_csv(registrations, here::here(dir_cleaned, paste0(latest_export_date, "_registrations.csv")))


# Add cross-registrations found in 2022 -----------------------------------
# After numbat extractions were complete, we checked (webscrape, manual) all registrations for any additional cross-registrations

reg_2022_raw <-
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16J5HBZm93CTp8Ydk-wetFtFxI5FfzqFU3tkg1JKavVw/edit#gid=1116415005", sheet = "cleaned")

reg_2022 <-
  reg_2022_raw |>
  select(trial_id, crossreg, euctr_hidden) |>
  mutate(euctr_hidden = if_else(euctr_hidden == 1, TRUE, FALSE)) |>

  # Check that one row per trial
  assertr::assert(assertr::is_uniq, trial_id) |>

  # Get one row per cross-registration
  tidyr::separate_rows(crossreg, sep = "; ") |> #count(euctr_hidden)

  # euctr_hidden only relevant for euctr registrations
  mutate(euctr_hidden = if_else(!stringr::str_detect(crossreg, "EUCTR"), NA, euctr_hidden)) |>

  # Add in tri ids for trial_id and crossreg and then deduplicate
  left_join(select(reg_input_numbat, id, trn), by = c("trial_id" = "trn")) |>
  rename(id_trial_id = id) |>
  left_join(select(reg_input_numbat, id, trn), by = c("crossreg" = "trn")) |>
  rename(id_crossreg = id)

# Create df of hidden euctr
euctr_hidden_df <-
  reg_2022 |>
  filter(!is.na(euctr_hidden)) |>
  select(crossreg, euctr_hidden)

# Get all trn/id combinations, excluding empty and duplicates
reg_2022_trns_ids <-
  bind_rows(
    select(reg_2022, id = id_trial_id, trn = trial_id),
    select(reg_2022, id = id_crossreg, trn = trial_id),
    select(reg_2022, id = id_trial_id, trn = crossreg),
    select(reg_2022, id = id_crossreg, trn = crossreg)
  ) |>
  tidyr::drop_na() |>
  distinct(id, trn) |>
  arrange(id) |>

  # Limit to new trn/id combinations, not in registrations
  anti_join(select(reg_input_numbat, id, trn)) |>

  # Add in registries
  mutate(registry = ctregistries::which_registries(trn))

reg_input_numbat_2022 <-
  bind_rows(reg_input_numbat, reg_2022_trns_ids) |>
  arrange(id) |>
  distinct()


# Deduplicate database ----------------------------------------------------
# We found additional cross-registrations during extraction as well as post-extraction checks in 2022, and now have multiple db_ids associated with one trn. We need to deduplicate so that each trn is associated with only a single db_id. A db_id can be associated with multiple trns (i.e., cross-registrations).

