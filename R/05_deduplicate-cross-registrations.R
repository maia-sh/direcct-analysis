# Combine and deduplicate ALL registrations, including:
# - Numbat extractions
# - Pre-Numbat input extractions (e.g., from Phase 1, ICTRP, registries, etc.)
#   NOTE: Manually added registrations from pre-numbat repository
# - Cross-registrations checks 2022

library(dplyr)

dir_manual <- here::here("data", "manual")

# Set mode ----------------------------------------------------------------
# Iterative process originally done in google sheets.
# For reproducibility, google sheets saved locally in repo.
# Here we specify whether to use "local" or "google" mode (default: local).

dedupe_mode <-ifelse(
  utils::menu(c("local", "google"), title = glue::glue("Select cross-registration deduplication mode\n(default = local)")) == 2,
  "google", "local"
)

# If google mode, set up google identity
# Note: Google identity must have access to specified sheets
if (dedupe_mode == "google"){

  # Use google identity (i.e., gmail) to access for google sheets
  # Get google identity if locally stored as "google", if available
  # Else ask user and store
  google_id <-
    ifelse(
      nrow(keyring::key_list("google")) == 1,
      keyring::key_get("google"),
      keyring::key_set("google")
    )

  message("Accessing googlesheets via: ", google_id)

  # If new google identity, prompt user in web browser to authenticate
  googlesheets4::gs4_auth(google_id)

}


# Combine registrations from numbat and pre-numbat input ------------------

reg_numbat <-
  readr::read_csv(here::here("data", "cleaned", "registrations.csv"))

reg_input <-
  readr::read_csv(here::here("data", "manual", "cleaned-registrations-3.csv")) |>

  # Use source 3 as source (differs from source 2 rarely)
  select(id, trn, registry, source = source_3)

if (nrow(filter(reg_input, stringr::str_detect(trn, "Outside"))) > 0) {
  stop("There is 1 or more TRNs with 'Outside', presumably EUCTR!")
}

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


# Add cross-registrations found in 2022 -----------------------------------
# After numbat extractions were complete, we checked (webscrape, manual) all registrations for any additional cross-registrations

reg_2022_raw <- if (dedupe_mode == "google") {
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16J5HBZm93CTp8Ydk-wetFtFxI5FfzqFU3tkg1JKavVw/edit#gid=1116415005", sheet = "cleaned")
} else {
  readr::read_csv(fs::path(dir_manual, "2022_cross-registrations.csv"))
}

reg_2022 <-
  reg_2022_raw |>
  select(trn, crossreg, euctr_hidden) |>
  mutate(euctr_hidden = if_else(euctr_hidden == 1, TRUE, FALSE)) |>

  # Check that one row per trial
  assertr::assert(assertr::is_uniq, trn) |>

  # Get one row per cross-registration
  tidyr::separate_rows(crossreg, sep = "; ") |> #count(euctr_hidden)

  # euctr_hidden only relevant for euctr registrations
  mutate(euctr_hidden = if_else(!stringr::str_detect(crossreg, "EUCTR"), NA, euctr_hidden)) |>

  # Add in tri ids for trn and crossreg and then deduplicate
  left_join(select(reg_input_numbat, id, trn), by = c("trn"), relationship = "many-to-many") |>
  rename(id_trial_id = id) |>
  left_join(select(reg_input_numbat, id, trn), by = c("trn"), relationship = "many-to-many") |>
  rename(id_crossreg = id)

# Get all trn/id combinations, excluding empty and duplicates
reg_2022_trns_ids <-
  bind_rows(
    select(reg_2022, id = id_trial_id, trn),
    select(reg_2022, id = id_crossreg, trn),
    select(reg_2022, id = id_trial_id, trn = crossreg),
    select(reg_2022, id = id_crossreg, trn = crossreg)
  ) |>
  tidyr::drop_na() |>
  distinct(id, trn) |>
  arrange(id) |>

  # Limit to new trn/id combinations, not in registrations
  anti_join(select(reg_input_numbat, id, trn), by = c("id", "trn")) |>

  # Add in registries
  mutate(registry = ctregistries::which_registries(trn))

reg_input_numbat_2022 <-
  bind_rows(reg_input_numbat, reg_2022_trns_ids) |>
  arrange(id) |>
  distinct()


# Deduplicate database ----------------------------------------------------
# We found additional cross-registrations during extraction as well as post-extraction checks in 2022, and now have multiple db_ids associated with one trn. We need to deduplicate so that each trn is associated with only a single db_id. A db_id can be associated with multiple trns (i.e., cross-registrations).

trials_numbat <-
  readr::read_csv(here::here("data", "cleaned", "trials.csv"))

# Prepare database duplicates (trns associated with >1 db_id) for manual deduplication
prepare_db_dupes <- function(tbl_in) {

  tbl_out <-
    tbl_in |>

    # Add flag for whether db_id coded
    mutate(db_id_coded = if_else(id %in% trials_numbat$id, TRUE, FALSE)) |>

    # Add number of ids per trn
    group_by(trn) |>
    mutate(n_db_ids_per_trn = n()) |>
    ungroup()  |>

    # Add number of trns per id
    group_by(id) |>
    mutate(n_trns_per_db_id = n()) |>
    ungroup() |>

    # Limit to trns associated with >1 db_id
    group_by(id) |>
    filter(any(n_db_ids_per_trn > 1)) |>
    mutate(trns = paste(trn, collapse = "; ")) |>
    ungroup() |>
    mutate(id_coded = glue::glue("{id} ({db_id_coded})")) |>
    group_by(trn) |>
    mutate(ids = paste(id_coded, collapse = "; ")) |>
    ungroup() |>
    distinct(ids, trns)

  message("There are ", nrow(tbl_out), " database ids to manually deduplicate!")

  return(tbl_out)
}

# Get all trns associated with >1 db_id, for manual deduplication
dedupe_to_code <- prepare_db_dupes(reg_input_numbat_2022)

readr::write_csv(dedupe_to_code, fs::path(dir_manual, "crossreg-dedupe_to-code.csv"))

# Note: Manually uploaded to google sheets and deduplicated manually

if (dedupe_mode == "google") {
  dedupe_googlesheet <- "https://docs.google.com/spreadsheets/d/1HXG_4rQDG4Yt08XvnUwfzJIY5k2SXRu-Tfxa7goZoOE/edit#gid=1539112480"

  dedupe_coded <- googlesheets4::read_sheet(dedupe_googlesheet)

} else {
  dedupe_coded <- readr::read_csv(fs::path(dir_manual, "crossreg-dedupe_coded.csv"))
}

# Create lookup table of dedupe trns/ids
dedupe_lookup <-

  dedupe_coded |>

  # row_keep should be TRUE/FALSE
  pointblank::col_is_logical("row_keep") |>
  pointblank::col_vals_not_null("row_keep") |>

  # id_final should appear if and only if row_keep
  pointblank::col_vals_null("id_final", ~ . %>% filter(!row_keep)) |>
  pointblank::col_vals_not_null("id_final", ~ . %>% filter(row_keep)) |>

  filter(row_keep) |>
  select(trns, id_final) |>

  # Check that all ids unique
  assertr::assert(assertr::is_uniq, id_final) |>
  tidyr::separate_rows(trns, sep = "; ") |>
  rename(trn = trns) |>

  # Check that all trns unique
  assertr::assert(assertr::is_uniq, trn)

# Join into registrations, fill in missing with id old, check for dupes
reg_deduped <-
  reg_input_numbat_2022 |>

  # Match final ids to trns
  left_join(dedupe_lookup, by = "trn") |>

  # If no new final id, use old id
  mutate(id_final = if_else(is.na(id_final), id, id_final)) |>

  # Collapse all old ids for each trn
  group_by(trn) |>
  mutate(ids_old = paste(id, collapse = "; ")) |>
  ungroup() |>

  distinct(id = id_final, trn, registry, ids_old)

# Deduplicated registrations should have only one row per trn
# If not (i.e., >1 row per trn), get dupes and append to google sheets, and repeat from reading in `dedupe_googlesheet`, and if not google mode, error
if (!(identical(n_distinct(reg_input_numbat_2022$trn), n_distinct(reg_deduped$trn)) & identical(n_distinct(reg_deduped$trn), nrow(reg_deduped)))) {
  message("There is >1 row per TRN! Additional deduplication needed.")
  if (dedupe_mode == "google"){
    prepare_db_dupes(reg_deduped) |>
      googlesheets4::sheet_append(dedupe_googlesheet, .)
  } else {stop("Use 'google' mode for additional deduplication")}
} else message("Trials successfully deduplicated!")


# Deduplicated registrations should have
# - Each unique trn should map to single db id
# - Each unique old id should map to single db id
reg_deduped |>

  # As expected, all trns are unique (only 1 db id per trn)
  assertr::assert(assertr::is_uniq, trn) |>

  # Get one row per old id/new id combination
  tidyr::separate_rows(ids_old, sep = "; ") |>

  # Limit to unique old id/new id combination
  distinct(id, ids_old) |>
  count(ids_old) |>

  # Verify only single old id/new id combination
  assertr::verify(n == 1)

# Add info about unavailable registrations --------------------------------
# Some registrations are unavailable, e.g., hidden on EUCTR or otherwise unresolved.

# Registrations which didn't resolve at time of search (phase 1 or 2/3)
unresolved_registrations <-
  bind_rows(reg_input, reg_numbat) |>
  filter(source == "unresolved") |>
  distinct(trn) |>
  arrange(trn) |>
  pull(trn)

# Euctr registrations hidden at time of manual cross-registrations checks (2022)
hidden_euctr <-
  reg_2022 |>
  filter(!is.na(euctr_hidden)) |>
  arrange(crossreg) |>
  pull(crossreg)

# Check unresolved trns
if (dedupe_mode == "google") {
  unresolved_googlesheet <-
    "https://docs.google.com/spreadsheets/d/1LwEtItdmSAK9X9FzXtQDasMsgaoYsR1fUhfzC_d3Lwk/edit#gid=0"

  unresolved_checks <- googlesheets4::read_sheet(unresolved_googlesheet)

} else {
  unresolved_checks <- readr::read_csv(fs::path(dir_manual, "unresolved-crossreg-checks.csv"))
}

# If any additional unresolved trns, add to googlesheet for manual checks, if google mode, and if not google mode, error

additional_unresolved_checks <-
  tibble(trn = unique(c(hidden_euctr, unresolved_registrations))) |>
  anti_join(unresolved_checks, by = "trn")

if (nrow(additional_unresolved_checks) > 0){
  if (dedupe_mode == "google") {
    googlesheets4::sheet_append(unresolved_googlesheet, additional_unresolved_checks)
  } else {stop("Use 'google' mode for additional unresolved checks")}
}

# Get date(s) of unresolved checks
unresolved_checks_date <- unique(pull(unresolved_checks, date))

# Limit to trns verified unresolved
reg_unresolved <-
  unresolved_checks |>
  filter(!resolved) |>
  distinct(trn, resolved)

# Add resolved info to registrations
reg_deduped_unresolved <-
  reg_deduped |>
  left_join(reg_unresolved, by = "trn")


dir_processed <- fs::dir_create(here::here("data", "processed"))
readr::write_csv(reg_deduped_unresolved, fs::path(dir_processed, "deduped-registrations.csv"))


# Deduplicate trials/results extractions ----------------------------------

# Using deduplicated cross-registrations, deduplicate numbat extractions (trials/results)

results_numbat <-
  readr::read_csv(here::here("data", "cleaned", "results.csv"))

arms_numbat <-
  readr::read_csv(here::here("data", "cleaned", "arms.csv"))

# Create lookup table of new/old ids
db_id_lookup <-
  reg_deduped_unresolved|>

  # Since some db ids not in ids_old, combine all ids
  # filter(stringr::str_detect(ids_all, id, negate = TRUE))
  mutate(ids_all = paste(id, ids_old, sep = "; ")) |>

  tidyr::separate_rows(ids_all, sep = "; ") |>
  distinct(id, id_old = ids_all)

# Add deduplicated id to numbat trials
trials_new_id <-
  trials_numbat |>

  # Verify one row per id
  assertr::assert(assertr::is_uniq, id) |>
  rename(id_numbat = id) |>
  left_join(db_id_lookup, by = c("id_numbat" = "id_old")) |>
  relocate(id, .before = id_numbat) |>

  # Verify new id matched to each old id
  assertr::assert(assertr::not_na, id)

# Get duplicate trial/result extractions (i.e., new id appears >1)
trials_dupe <-
  trials_new_id |>
  janitor::get_dupes(id) |>
  arrange(id, desc(trn))


# Visually inspect results/arms for cross-registrations of trials
# Make sure "true" results/arms encoded (via numbat) for primary trn
results_dupe <-
  results_numbat |>
  rename(id_numbat = id) |>
  semi_join(trials_dupe, by = "id_numbat") |>
  left_join(select(trials_dupe, id, id_numbat), by = "id_numbat") |>
  relocate(id, .before = id_numbat) |>
  arrange(id, desc(trn))

arms_dupe <-
  arms_numbat |>
  rename(id_numbat = id) |>
  semi_join(trials_dupe, by = "id_numbat") |>
  left_join(select(trials_dupe, id, id_numbat), by = "id_numbat") |>
  relocate(id, .before = id_numbat) |>
  arrange(id, desc(trn))

# Keep rows where new/old id match (i.e., remove duplicates)
trials_deduped <-
  trials_new_id |>
  filter(id == id_numbat) |>
  select(-id_numbat)

trials_removed <-
  trials_numbat |>
  anti_join(trials_deduped, by = "id")

n_numbat_ids_removed <- n_distinct(trials_dupe$id_numbat) - n_distinct(trials_dupe$id)

# Check that n deduped trials same as numbat trials + removed trials
if(nrow(trials_deduped) != nrow(trials_numbat) - n_numbat_ids_removed){
  stop("n deduped trials should be same as numbat trials + removed trials")
}

# Use deduped trials to dedupe results/arms
results_deduped <-
  results_numbat |>
  semi_join(trials_deduped, by = "id")

results_removed <-
  results_numbat |>
  anti_join(results_deduped, by = "id")

arms_deduped <-
  arms_numbat |>
  semi_join(trials_deduped, by = "id")

arms_removed <-
  arms_numbat |>
  anti_join(arms_deduped, by = "id")

numbat_ids_removed <-
  trials_removed |>
  distinct(id) |>
  arrange(id) |>
  pull()

# Check that all removed results/arms in removed trials
all(pull(distinct(results_removed, id)) %in% numbat_ids_removed)
all(pull(distinct(arms_removed, id)) %in% numbat_ids_removed)

# Check that expected and only expected trials removed
# Note: We manually verified all expected removals
# https://docs.google.com/spreadsheets/d/1c1kNntef5itBiuMGkg-9b8ccbwmmEDekrlvOPRZldnU/
numbat_ids_removed_expected <- c(
  "tri00300", "tri00516", "tri00569", "tri00651", "tri00699", "tri00845",
  "tri01265", "tri01296", "tri01763", "tri02158", "tri02235", "tri02657",
  "tri02675", "tri02990", "tri03027", "tri03220", "tri03604", "tri03940",
  "tri04111", "tri04421", "tri04425", "tri04691", "tri04843", "tri05580",
  "tri05731", "tri05866", "tri07329", "tri07829", "tri08139", "tri08224",
  "tri08270", "tri08939", "tri10297", "tri10336"
)

if(!all(numbat_ids_removed == numbat_ids_removed_expected)){
  stop("Unexpected trials removed in deduping!")
}

readr::write_csv(trials_deduped, fs::path(dir_processed, "deduped-trials.csv"))
readr::write_csv(results_deduped, fs::path(dir_processed, "deduped-results.csv"))
readr::write_csv(arms_deduped, fs::path(dir_processed, "deduped-arms.csv"))
