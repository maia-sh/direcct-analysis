# Create dataframe with info on each trial's earliest result per and across types


# Prep kaplan meier -------------------------------------------------------

library(lubridate)

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

# Get earliest publication by type including interim
# results_interim_type <-
#   results_interim |>
#
#   # TODO: decide whether interim-only OR either interim or full
#   # Limit to interim preprint/article
#   filter(stringr::str_detect(pub_type, "interim")) |>
#   mutate(pub_type = case_when(
#     pub_type == "interim_results_journal_article" ~ "interim_article",
#     pub_type == "interim_results_preprint" ~ "interim_preprint"
#   )) |>
#
#   # Get earliest publication by type and any
#   group_by(id, pub_type) |>
#   summarise(
#     date_publication = min(date_publication),
#     .groups = "drop"
#   )

# Combine earliest publications
results_km_all <-
  bind_rows(
    results_any,
    results_type,
    results_interim_any
  ) |>
  mutate(publication = TRUE, .after = "pub_type")

# Prepare latest completion date before cutoff
latest_rcd_pre_cutoff <-
  registrations |>

  # Get all trns for included trials
  semi_join(trials, by = "id") |>

  # Get all rcd's for all registrations of included trials
  left_join(select(registries, trn, rcd), by = "trn") |>
  select(id, trn, registry, rcd) |>

  # Limit to rcd's before cutoff
  filter(rcd < PHASE_3_CUTOFF) |>

  # Collapse to latest rcd before cutoff
  group_by(id) |>
  summarise(date_completion = max(rcd, na.rm = TRUE))

km_data_long <-

  # Create df with row per trial per pubtype
  tibble(
    id = rep(trials$id, each = 5),
    pub_type = rep(c("any", "article", "preprint", "summary", "interim_any"), length(trials$id))
  ) |>

  # Add in cutoff and completion dates to calculate time to publication/censoring
  mutate(date_cutoff = PHASE_3_CUTOFF) |>
  left_join(latest_rcd_pre_cutoff, by = "id") |>

  # Add in results
  left_join(results_km_all, by = c("id", "pub_type")) |>

  # Change NA to FALSE for no results
  mutate(publication = tidyr::replace_na(publication, FALSE)) |>

  # Calculate time to publication/censoring
  mutate(
    time_publication =
      if_else(publication,
              lubridate::as.duration(date_completion %--% date_publication)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      )
  )

km_data_wide <-
  km_data_long |>
  tidyr::pivot_wider(
    names_from = pub_type,
    names_glue = "{.value}_{pub_type}",
    values_from = ends_with("publication")
  )

readr::write_csv(km_data_wide, here::here("data", "reporting", "kaplan-meier-time-to-pub.csv"))


# Analyze kaplan meier ----------------------------------------------------
# TODO: issue with min follow-up <6 weeks due to cd selection
# Get follow-up times for unreported trials
follow_up_unreported <-
  km_data_wide |>
  filter(!publication_any) |>
  pull(time_publication_any)

