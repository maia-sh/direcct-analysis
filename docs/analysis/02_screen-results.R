# library(dplyr)

# RESULTS_CUTOFF <- as.Date("2021-08-15")

# trials_screening <-
#   readr::read_csv(here::here("data", "reporting", "screening-trials.csv"))
# trials <- filter(trials_screening, is_analysis_pop)
# results_all <-  readr::read_csv(here::here("data", "reporting", "results.csv"))



# Prepare results (summary, full article, full preprint) ------------------


results <-
  results_all |>

  # Limit to full results
  filter(stringr::str_detect(pub_type, "full|summary")) |>

  # Limit to results of included trials
  semi_join(trials, by = "id") |>

  # Verify no missing publication dates
  assertr::assert(assertr::not_na, date_publication) |>

  # Limit to those published before cutoff
  filter(date_publication <= RESULTS_CUTOFF)

# Check for same result for >1 trial
# Note: 1 result included for 2 trials
# tri03525/tri03533: 10.1016/s0140-6736(20)31866-3/32896291
dupe_results <-
  results |>
  add_count(doi, name = "dupe_doi") |>
  add_count(pmid, name = "dupe_pmid") |>
  add_count(url, name = "dupe_url") |>
  mutate(
    dupe_doi = if_else(!is.na(doi) & dupe_doi >1, TRUE, FALSE),
    dupe_pmid = if_else(!is.na(pmid) & dupe_pmid >1, TRUE, FALSE),
    dupe_url = if_else(!is.na(url) & dupe_url >1, TRUE, FALSE),
    dupe = dupe_doi | dupe_pmid | dupe_url
  ) |>
  filter(dupe)

# Remove duplicate result from count
n_full_results <- nrow(results) - n_distinct(dupe_results$doi)
n_trials_w_results <- n_distinct(results$id)
p_trials_w_results <- n_trials_w_results/ n_analysis


# Prepare results (full OR interim) ---------------------------------------

results_interim <-
  results_all |>

  # Limit to full results
  filter(stringr::str_detect(pub_type, "full|interim|summary")) |>

  # Limit to results of included trials
  semi_join(trials, by = "id") |>

  # Verify no missing publication dates
  assertr::assert(assertr::not_na, date_publication) |>

  # Limit to those published before cutoff
  filter(date_publication <= RESULTS_CUTOFF)

# Check for same result for >1 trial
# Note: 1 full result included for 2 trials
# tri03525/tri03533: 10.1016/s0140-6736(20)31866-3/32896291
# Note: 1 result included as both interim and full result for 1 trial
# tri01483: 10.1056/nejmoa2007764
dupe_results_interim <-
  results_interim |>
  add_count(doi, name = "dupe_doi") |>
  add_count(pmid, name = "dupe_pmid") |>
  add_count(url, name = "dupe_url") |>
  mutate(
    dupe_doi = if_else(!is.na(doi) & dupe_doi >1, TRUE, FALSE),
    dupe_pmid = if_else(!is.na(pmid) & dupe_pmid >1, TRUE, FALSE),
    dupe_url = if_else(!is.na(url) & dupe_url >1, TRUE, FALSE),
    dupe = dupe_doi | dupe_pmid | dupe_url
  ) |>
  filter(dupe)

# Remove duplicate results from count
n_full_interim_results <- nrow(results_interim) - n_distinct(dupe_results_interim$doi)
n_trials_w_full_interim_results <- n_distinct(results_interim$id)
p_trials_w_full_interim_results <- n_trials_w_full_interim_results/ n_analysis

