# Analysis for main kaplan meier
# NOTE: if do additional km sensitivity analyses, then functionalize

km_data_wide <- km_main
trials_cd <- select(trials, id, date_completion)

# Analyze kaplan meier ----------------------------------------------------

# Get follow-up times for unreported trials
follow_up_unreported <-
  km_data_wide |>
  filter(!publication_any) |>
  pull(time_publication_any)


# Analyze preprint-to-article ---------------------------------------------

# Trials with preprint only

# For preprint/article conversion analysis, get trials with preprint and article from matching groups (pair or more)
preprint_article_groups <-
  km_data_wide |>

  # Get trials with both preprint and article
  filter(publication_article & publication_preprint) %>%

  # Get all full results for those trials
  semi_join(results, ., by = "id") |>

  # Limit to preprint and article
  filter(stringr::str_detect(pub_type, "article|preprint")) |>

  # Limit to preprint/article pairs
  # tri01280 has preprint/article but not matching --> disregard both results and trial
  # tri02914 has additional article not matching preprint --> disregard that result
  # tri03276 has two preprint/article pairs --> consider both result pairs
  add_count(id, full_pub_group) |>
  filter(n > 1) |>

  arrange(id) |>

  select(id, full_pub_group, pub_type, date_publication)

# Limit groups to pairs, i.e., tri01886, tri02964, tri03275, tri05790 have two preprints preceeding articles
# Use earliest date for each pub type in group (since we're interested in how much sooner preprints come out)
# TODO: discuss with nd how to handle these. could do each preprint-article pair, just earliest, just latest
preprint_article_pairs <-
  preprint_article_groups |>
  group_by(id, full_pub_group, pub_type) |>
  mutate(date_publication = min(date_publication)) |>
  ungroup() |>
  distinct()

n_trials_preprint_article <- n_distinct(preprint_article_pairs$id)

n_preprint_article_pairs <-
  preprint_article_pairs |>
  distinct(id, full_pub_group) |>
  nrow()

n_trials_multi_preprint_article_pair <- n_preprint_article_pairs - n_trials_preprint_article

n_trials_multi_preprint_article_type <-
  preprint_article_groups |>
  group_by(id, full_pub_group, pub_type) |>
  mutate(n_type = row_number()) |>
  filter(n_type > 1) |>
  nrow()

n_results_preprint_article <- nrow(preprint_article_groups)

km_preprint_article <-
  preprint_article_pairs |>

  mutate(pub_type = case_when(
    pub_type == "full_results_journal_article" ~ "article",
    pub_type == "full_results_preprint" ~ "preprint"
  )) |>

  # Pivot preprint/articles to row to calculate time to conversion
  tidyr::pivot_wider(c(id, full_pub_group), names_from = pub_type, names_prefix = "date_publication_", values_from = date_publication) |>

  # Add completion date
  left_join(trials_cd, by = "id") |>

  # Calculate time from completion to preprint, and preprint to article
  mutate(
    # date_cutoff = RESULTS_CUTOFF,
    time_cd_preprint = lubridate::as.duration(date_completion %--% date_publication_preprint)/ ddays(1),
    time_cd_article = lubridate::as.duration(date_completion %--% date_publication_article)/ ddays(1),
    time_preprint_article = lubridate::as.duration(date_publication_preprint %--% date_publication_article)/ ddays(1)
  )


# Get preprints that do NOT convert to article
# tri01280 has preprint/article but not matching so should be included as non-converted preprint. Hence can't use `filter(km_data_wide, !publication_article & publication_preprint)` since would be excluded and instead use results
km_preprint_cutoff <-
  results |>

  # Get results of trials the don't have matching preprint/article
  anti_join(km_preprint_article, by = "id") |>

  # Limit to preprint
  filter(stringr::str_detect(pub_type, "preprint")) |>

  select(id, full_pub_group, date_publication_preprint = date_publication) |>
  # Add completion date
  left_join(trials_cd, by = "id") |>

  # Calculate time from completion to preprint, and preprint to cutoff
  mutate(
    # date_cutoff = RESULTS_CUTOFF,
    time_cd_preprint = lubridate::as.duration(date_completion %--% date_publication_preprint)/ ddays(1),
    time_preprint_cutoff = lubridate::as.duration(date_publication_preprint %--% RESULTS_CUTOFF)/ ddays(1)
  )

n_results_preprint_no_article <- nrow(km_preprint_cutoff)
n_trials_preprint_no_article <- n_distinct(km_preprint_cutoff$id)

# Prepare preprint-to-article km
km_preprint_article_combined <-
  km_preprint_cutoff |>
  rename(time_preprint_article = time_preprint_cutoff) |>
  # select(-date_cutoff) |>
  bind_rows(mutate(km_preprint_article, publication_article = TRUE)) |>
  mutate(publication_preprint = TRUE, date_cutoff = RESULTS_CUTOFF) |>
  mutate(publication_article = tidyr::replace_na(publication_article, FALSE))

readr::write_csv(km_preprint_article_combined, here::here("data", "reporting", "kaplan-meier-preprint-to-article.csv"))


# Get trials with article only (NO preprint)
km_article_only <-
  km_data_wide |>

  # Get trials with both preprint and article
  filter(publication_article & !publication_preprint)

# Get trials with article NOT converted from preprint
# tri01280 has preprint/article but not matching --> included
# tri02914 has additional article not matching preprint --> already included in trials with preprint/article; could include here as well but *manually*
km_article_no_matched_preprint <-
  results |>

  # Get results of trials the don't have matching preprint/article
  anti_join(
    # TODO: Decide whether to include tri02914
    filter(km_preprint_article, id != "tri02914"),
    by = "id"
  ) |>

  # Limit to article
  filter(stringr::str_detect(pub_type, "article")) |>

  select(id, full_pub_group, date_publication_article = date_publication) |>

  # Add completion date
  left_join(trials_cd, by = "id") |>

  # Calculate time from completion to preprint, and preprint to cutoff
  mutate(
    # date_cutoff = RESULTS_CUTOFF,
    time_cd_article = lubridate::as.duration(date_completion %--% date_publication_article)/ ddays(1)
  )

n_results_article_no_matched_preprint <- nrow(km_article_no_matched_preprint)
n_trials_article_no_matched_preprint <- n_distinct(km_article_no_matched_preprint$id)
