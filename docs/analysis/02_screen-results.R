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

trials_w_results <-
  results |>
  distinct(id) |>
  mutate(has_full_result = TRUE)

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


# Prepare results for km (with type and publication date) -----------------

# Get earliest publication by type
results_type <-
  results |>

  mutate(pub_type = case_when(
    pub_type == "full_results_journal_article" ~ "article",
    pub_type == "full_results_preprint" ~ "preprint",
    pub_type == "summary_results" ~ "summary"
  )) |>

  group_by(id, pub_type) |>
  summarise(
    date_publication = min(date_publication),
    .groups = "drop"
  )

# Get earliest publication any
results_any <-
  results |>
  group_by(id) |>
  summarise(
    pub_type = "any",
    date_publication = min(date_publication)
  )

# Get earliest publication any including interim
results_interim_any <-
  results_interim |>
  group_by(id) |>
  summarise(
    pub_type = "interim_any",
    date_publication = min(date_publication)
  )

# Combine earliest publications
results_km_all <-
  bind_rows(
    results_any,
    results_type,
    results_interim_any
  ) |>
  mutate(publication = TRUE, .after = "pub_type")
