library(dplyr)

dir_cleaned <- here::here("data", "cleaned")
dir_processed <- here::here("data", "processed")
dir_reporting <- fs::dir_create(here::here("data", "reporting"))

# Report trial extraction metadata/info
# Note: removed `username`, alternatively could keep as is (only single coded), or add dual coded

readr::read_csv(fs::path(dir_processed, "deduped-trials.csv")) |>
  select(
    id,
    trn_extracted = trn, # primary trn displayed for numbat extraction
    dual_coded = is_reconciled,
    timestamp_finished, # last update, extraction or reconciliation (if dual coded)
    starts_with("incidental"),
    comments
  ) |>
  readr::write_csv(fs::path(dir_reporting, "extraction-info.csv"))

# Copy reporting versions
# - trials screening
# - results
# - registrations
# - ictrp 2021-07
# - registries 2021-07 & 2022-04

fs::file_copy(
  fs::path(dir_processed, "screening-trials.csv"),
  fs::path(dir_reporting, "screening-trials.csv"),
  overwrite = TRUE
)

fs::file_copy(
  fs::path(dir_processed, "matched-results.csv"),
  fs::path(dir_reporting, "results.csv"),
  overwrite = TRUE
)

fs::file_copy(
  fs::path(dir_processed, "deduped-registrations.csv"),
  fs::path(dir_reporting, "registrations.csv"),
  overwrite = TRUE
)

fs::file_copy(
  fs::path(dir_cleaned, "2021-07-01_ictrp.csv"),
  fs::path(dir_reporting, "2021-07-01_ictrp.csv"),
  overwrite = TRUE
)

fs::file_copy(
  fs::path(dir_cleaned, "2021-07_registries.csv"),
  fs::path(dir_reporting, "2021-07_registries.csv"),
  overwrite = TRUE
)

fs::file_copy(
  fs::path(dir_cleaned, "2022-04_registries.csv"),
  fs::path(dir_reporting, "2022-04_registries.csv"),
  overwrite = TRUE
)
